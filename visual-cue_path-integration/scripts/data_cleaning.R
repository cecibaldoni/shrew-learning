# Libraries ----
library(tidyverse)
library(here)
library(sf)          # spatial manipulation
library(zoo)         # linear interpolation on NA values
library(parallel)
library(ggplot2)
library(trajr)
library(plotly)
library(RColorBrewer)
library(scales)

source(here("utils.R"))

# Load files ----
doors <- read.csv(here("visual-cue_path-integration", "data", "raw", "trial_door.csv")) %>%
  mutate(Trial = paste0("T", trial_n))

coords <- read.csv(here("visual-cue_path-integration", "data", "raw", "coords.csv"))

# tracking.csv: raw per-frame tracking data (x, y, frame, time, speed) for each trial.
# Output of the Python tracking pipeline. One row per frame per trial.
tracking <- read.csv(here("visual-cue_path-integration", "data", "raw", "tracking.csv"))

# Empty dataframes for saving info on visits to other doors on trip back
other_door_visits <- data.frame(ID = NULL, door = NULL, Trial = NULL, Season = NULL)
no_visits <- data.frame(unique_trial_ID = NULL, other_door_visits = NULL, ID = NULL, trial = NULL, season = NULL)

trial_ls <- split(tracking, tracking$unique_trial_ID)

# Replace Inf with NA and interpolate x and y
trial_ls <- lapply(trial_ls, function(df) {
  df <- df %>%
    mutate(x = ifelse(is.infinite(x) | x == "inf", NA, as.numeric(x)),
           y = ifelse(is.infinite(y) | y == "inf", NA, as.numeric(y))) %>%
    mutate(x = na.approx(x, na.rm = FALSE),
           y = na.approx(y, na.rm = FALSE)) %>%
    mutate(frame = seq(0, nrow(df) - 1),
           time = frame / 30) %>%
    mutate(distance = sqrt((x - lag(x))^2 + (y - lag(y))^2), speed = distance * 30) %>%
    mutate(speed = ifelse(is.na(speed), 0, speed)) %>%
    select(-distance) %>%
    relocate(time, .before = frame)
  return(df)
})

# Prepare cluster for parallel computation ----
mycl <- makeCluster(4)
clusterExport(mycl, c("coords", "trial_ls", "doors", "other_door_visits"))
clusterEvalQ(mycl, {
  library(sf)
  library(sp)
  library(tidyverse)
})

## Example code: check single trials and plot (e.g. to verify coordinate system) ----

y = trial_ls[["winter_20240303-1_T6"]]
food_coords <- coords %>%
  filter(unique_trial_ID == unique(y$unique_trial_ID))
doors_x <- coords %>%
  filter(unique_trial_ID == unique(y$unique_trial_ID)) %>%
  select(4:11) %>%
  pivot_longer(cols = contains("x"), names_to = "door", values_to = "x") %>%
  select(x)
doors_coords <- coords %>%
  filter(unique_trial_ID == unique(y$unique_trial_ID)) %>%
  select(4:11) %>%
  pivot_longer(cols = contains("y"), names_to = "door", values_to = "y") %>%
  mutate(door_ID = substr(door, 1, 1)) %>%
  select(c("door_ID", "y")) %>%
  bind_cols(doors_x)
p <- ggplot() +
  geom_point(data = y,
             aes(x = x, y = y, color = frame, text = paste("X:", x, "Y:", y))) +
  geom_point(data = doors_coords, aes(x = x, y = y, text = paste("Door", door_ID)),
             colour = "green", size = 5) +
  geom_point(data = food_coords,
             aes(x = FOOD_x, y = FOOD_y, text = paste("FOOD_X:", FOOD_x, "FOOD_Y:", FOOD_y)),
             colour = "red", size = 5) +
  ggtitle(paste("Trial ID:", y$unique_trial_ID[1]))
ggplotly(p, tooltip = "text")

# Main loop: process all trials ----

# Output files:
# master_results.csv: per-frame tracking data with food_journey labels
# master_distance.csv: per-trial summary metrics (straightness, path length, speed, time)
results  <- here("visual-cue_path-integration", "data", "processed", "master_results.csv")
distance <- here("visual-cue_path-integration", "data", "processed", "master_distance.csv")

other_door_visits_ls <- lapply(trial_ls, function(x) {
  ## To process a single trial for testing, uncomment one of:
  # x = trial_ls[["spring_20240520-4_T8"]]
  # x = trial_ls[[1]]
  
  # Recalculate speed from x and y
  x <- x %>%
    mutate(distance = sqrt((x - lag(x))^2 + (y - lag(y))^2),
           speed = ifelse(is.na(distance), 0, distance * 30)) %>%
    select(-distance)
  
  # Extract food coordinates and buffer for this trial
  food_coords <- coords %>%
    filter(unique_trial_ID == unique(x$unique_trial_ID)) %>%
    dplyr::select(c("FOOD_x", "FOOD_y")) %>%
    st_as_sf(coords = c("FOOD_x", "FOOD_y"))
  food_buffer <- st_buffer(food_coords, dist = 4)
  
  # Determine sequence (winter_2024 had a different door assignment)
  season_filter <- if (any(str_starts(x$unique_trial_ID, "winter_2024"))) "winter_2024" else "other"
  
  # Get correct door for this trial
  trial_door_ID <- doors %>%
    filter(Trial == unique(x$trial), season == season_filter) %>%
    pull(door)
  
  # Extract and convert all doors to sf objects
  doors_x <- coords %>%
    filter(unique_trial_ID == unique(x$unique_trial_ID)) %>%
    select(4:11) %>%
    pivot_longer(cols = contains("x"), names_to = "door", values_to = "x") %>%
    select(x)
  doors_coords <- coords %>%
    filter(unique_trial_ID == unique(x$unique_trial_ID)) %>%
    select(4:11) %>%
    pivot_longer(cols = contains("y"), names_to = "door", values_to = "y") %>%
    mutate(door_ID = substr(door, 1, 1)) %>%
    select(c("door_ID", "y")) %>%
    bind_cols(doors_x)
  all_doors_buffer <- doors_coords %>%
    st_as_sf(coords = c("x", "y")) %>%
    st_buffer(dist = 4)
  trial_door_buffer <- all_doors_buffer %>%
    filter(door_ID == trial_door_ID)
  
  # Convert shrew movement to sf object
  track_sf <- x %>%
    st_as_sf(coords = c("x", "y"))
  
  # Find food intersections
  at_food <- track_sf %>%
    st_intersection(food_buffer) %>%
    as.data.frame() %>%
    arrange(frame) %>%
    mutate(timediff = frame - lag(frame)) %>%
    mutate(new_timediff = ifelse(is.na(timediff) | timediff >= 30, 1, 0)) %>%
    mutate(visit_seq = cumsum(new_timediff))
  # Expected Warning Message from st_intersection ----
  
  # Skip trial if food was never reached
  if (nrow(at_food) == 0) {
    print(paste0("Food out of buffer zone in ", unique(x$unique_trial_ID)))
    return(NULL)
  }
  
  ## Label food journey phases ----
  track_sf_2 <- track_sf %>%
    full_join(at_food[c("frame", "visit_seq")]) %>%
    arrange(frame) %>%
    mutate(old_food_journey = case_when(
      frame == head(at_food$frame, 1) ~ "arrival",
      frame == tail(at_food[at_food$visit_seq == 1, "frame"], 1) ~ "departure",
      frame < head(at_food$frame, 1) ~ "trip_to",
      between(frame,
              head(at_food[at_food$visit_seq == 1, "frame"], 1),
              tail(at_food[at_food$visit_seq == 1, "frame"], 1)) ~ "at_food",
      frame %in% at_food[at_food$visit_seq != 1, "frame"] ~ paste("trip_back_revisit", visit_seq, sep = "_"),
      TRUE ~ "trip_back"
    ))
  
  # Label "exploration": frames after the shrew passes through the exit door on trip_back
  at_exit <- track_sf_2 %>%
    filter(old_food_journey == "trip_back") %>%
    st_intersection(all_doors_buffer %>% filter(door_ID == trial_door_ID)) %>%
    as.data.frame() %>%
    mutate(timediff = frame - lag(frame)) %>%
    mutate(new_timediff = ifelse(is.na(timediff) | timediff != 1, 1, 0)) %>%
    mutate(exit_seq = cumsum(new_timediff))
  
  if (nrow(at_exit) > 0) {
    track_sf_2 <- track_sf_2 %>%
      full_join(at_exit[c("frame", "exit_seq")]) %>%
      arrange(frame) %>%
      mutate(food_journey = ifelse(frame > tail(at_exit[at_exit$exit_seq == 1, "frame"], 1),
                                   "exploration", as.character(old_food_journey)),
             food_journey = ifelse(food_journey == "exploration" & !is.na(visit_seq),
                                   paste0("exploration_revisit_", visit_seq), food_journey)) %>%
      select(-exit_seq)
  } else {
    track_sf_2 <- track_sf_2 %>%
      mutate(food_journey = old_food_journey)
  }
  
  # Save per-frame labelled data to master_results.csv
  track_save <- track_sf_2 %>%
    mutate(coordinates = st_coordinates(geometry)) %>%
    mutate(x = coordinates[, "X"], y = coordinates[, "Y"]) %>%
    relocate(x, .after = frame) %>%
    relocate(y, .after = x) %>%
    relocate(unique_trial_ID, .before = season) %>%
    select(-old_food_journey, -visit_seq, -coordinates) %>%
    st_drop_geometry()
  
  write.table(track_save, file = results, append = TRUE, sep = ",",
              row.names = FALSE, col.names = !file.exists(results))
  
  ## Summary metrics ----
  
  x$food_journey <- track_sf_2$food_journey
  
  get_traj_metrics <- function(trip_df) {
    traj <- TrajFromCoords(trip_df, fps = 30, spatialUnits = "cm")
    list(length = TrajLength(traj),
         straightness = {s <- TrajStraightness(traj); if (length(s) == 0) NA_real_ else s},
         traj = traj)
  }
  
  trip_to      <- x %>% filter(food_journey == "trip_to")      %>% select(x, y, time)
  trip_back    <- x %>% filter(food_journey == "trip_back")    %>% select(x, y, time)
  exploration  <- x %>% filter(food_journey == "exploration")  %>% select(x, y, time)
  
  to_metrics          <- get_traj_metrics(trip_to)
  back_metrics        <- get_traj_metrics(trip_back)
  exploration_metrics <- get_traj_metrics(exploration)
  
  dist_doorfood <- if (!is.null(to_metrics$traj)) {
    TrajDistance(to_metrics$traj, startIndex = 1, endIndex = nrow(to_metrics$traj))
  } else NA_real_
  
  time_summary <- x %>%
    group_by(unique_trial_ID) %>%
    summarize(
      time_to_food = if (any(food_journey == "trip_to"))
        max(time[food_journey == "trip_to"], na.rm = TRUE) else NA_real_,
      time_back = if (any(food_journey == "trip_back"))
        max(time[food_journey == "trip_back"], na.rm = TRUE) else NA_real_,
      time_exploration = if (any(food_journey == "exploration"))
        diff(range(time[food_journey == "exploration"], na.rm = TRUE)) else NA_real_,
      time_at_food = {
        at_food_times <- time[food_journey == "at_food"]
        if (length(at_food_times) > 1) max(at_food_times) - min(at_food_times)
        else if (length(at_food_times) == 1) 1 / 30 else 0
      },
      .groups = "drop"
    )
  
  # straightness_to_food: used for Visual Associative Learning analysis
  # straightness_back: used for Path Integration analysis
  df <- data.frame(
    unique_trial_ID        = as.factor(x$unique_trial_ID[1]),
    season                 = as.factor(x$season[1]),
    ID                     = as.factor(x$ID[1]),
    trial                  = as.factor(x$trial[1]),
    status                 = as.factor(x$status[1]),
    food_door              = dist_doorfood,
    walked_to              = to_metrics$length,
    walked_back            = back_metrics$length,
    walk_exploration       = exploration_metrics$length,
    straightness_to_food   = to_metrics$straightness,
    straightness_back      = back_metrics$straightness,
    straightness_exploration = exploration_metrics$straightness,
    stringsAsFactors = FALSE
  ) %>%
    bind_cols(time_summary %>% select(-unique_trial_ID))
  
  write.table(df, file = distance, append = TRUE, sep = ",",
              row.names = FALSE, col.names = !file.exists(distance))
  
  # Check for visits to other doors on trip back or exploration
  other_doors <- track_sf_2 %>%
    filter(food_journey %in% c("trip_back", "exploration")) %>%
    st_intersection(all_doors_buffer %>% filter(door_ID != trial_door_ID))
  
  if (nrow(other_doors) > 0) {
    new_visits <- other_doors %>%
      group_by(door_ID) %>% slice(1) %>%
      dplyr::select(c("ID", "season", "trial", "door_ID", "food_journey")) %>%
      st_drop_geometry()
    other_door_visits <<- rbind(other_door_visits, new_visits)
    return(new_visits)
  } else {
    empty_data <- data.frame(unique_trial_ID = unique(x$unique_trial_ID), other_door_visits = 0) %>%
      separate(unique_trial_ID, into = c("season", "trial", "ID"), sep = "_")
    no_visits <<- rbind(no_visits, empty_data)
  }
  print(paste0("trial ", unique(x$unique_trial_ID), " completed."))
})

write.csv(other_door_visits, here("visual-cue_path-integration", "data", "processed", "other_door_visit.csv"), row.names = FALSE)

stopCluster(mycl)

## End of function ----

# Speed ----
# Calculate mean speed per food_journey phase and merge into master_distance.csv

tracking_results <- read.csv(here("visual-cue_path-integration", "data", "processed", "master_results.csv")) %>%
  mutate(season        = as.factor(season),
         ID            = as.factor(ID),
         status        = as.factor(status),
         food_journey  = as.factor(food_journey),
         trial         = as.factor(trial),
         unique_trial_ID = as.factor(unique_trial_ID)) %>%
  droplevels()

trial_list <- split(tracking_results, tracking_results$unique_trial_ID)

mean_speed_summary <- bind_rows(lapply(trial_list, function(x) {
  x %>%
    filter(food_journey %in% c("trip_to", "trip_back", "exploration")) %>%
    group_by(unique_trial_ID, food_journey) %>%
    summarise(mean_speed = mean(speed, na.rm = TRUE), .groups = "drop")
})) %>%
  pivot_wider(names_from = food_journey, values_from = mean_speed, names_prefix = "speed_")

path_data <- read_csv(here("visual-cue_path-integration", "data", "processed", "master_distance.csv")) %>%
  left_join(mean_speed_summary, by = "unique_trial_ID")

write_csv(path_data, here("visual-cue_path-integration", "data", "processed", "master_distance.csv"))

# Previous door ----
# Builds door_result: whether the shrew visited the door used in the previous trial.

other_door_visits <- read.csv(here("visual-cue_path-integration", "data", "processed", "other_door_visit.csv"), header = TRUE) %>%
  mutate(unique_trial_ID = paste(season, ID, trial, sep = "_"),
         unique_trial_ID = as.factor(unique_trial_ID),
         trial_n         = as.integer(str_remove(trial, "T")),
         door_ID         = as.factor(door_ID),
         sequence        = as.factor(if_else(str_starts(unique_trial_ID, "winter_2024"),
                                             "winter_2024", "other")))

doors <- read.csv(here("visual-cue_path-integration", "data", "raw", "trial_door.csv")) %>%
  mutate(door = as.factor(door),
         sequence = as.factor(season)) %>%
  relocate(sequence, .before = season) %>%
  select(-season) %>%
  arrange(trial_n, sequence) %>%
  group_by(sequence) %>%
  mutate(door = as.factor(door),
         previous_door_ID = ifelse(trial_n == 1, as.character(door), as.character(lag(door)))) %>%
  ungroup() %>%
  mutate(previous_door_ID = as.factor(previous_door_ID))

door_result <- other_door_visits %>%
  mutate(previous_trial_n = trial_n - 1) %>%
  left_join(doors, by = c("previous_trial_n" = "trial_n", "sequence")) %>%
  mutate(door_match = door_ID == previous_door_ID,
         season     = as.factor(season),
         status     = case_when(
           season == "summer"                          ~ "summer_wild",
           season == "winter" & grepl("^2021", ID)    ~ "winter_wild",
           season == "winter" & grepl("^2020", ID)    ~ "winter_captive",
           season == "winter" & grepl("^2024", ID)    ~ "winter_wild",
           season == "spring" & grepl("^2024", ID)    ~ "spring_wild",
           season == "spring" & grepl("^2020", ID)    ~ "spring_captive"),
         status       = as.factor(status),
         food_journey = as.factor(food_journey)) %>%
  filter(!is.na(door_match)) %>%
  select(-sequence, -previous_trial_n)

ggplot(door_result, aes(x = trial_n, fill = door_match)) +
  geom_bar(stat = "count") +
  scale_x_continuous(breaks = pretty_breaks()) +
  facet_grid(food_journey ~ status,
             labeller = labeller(
               food_journey = c(trip_to = "Trip To", at_food = "At Food", trip_back = "Trip Back"),
               status = c(summer_wild = "Summer Wild", winter_captive = "Winter Captive",
                          winter_wild = "Winter Wild", spring_captive = "Spring Captive",
                          spring_wild = "Spring Wild"))) +
  labs(x = "Trial", y = "Count", fill = "Previous Door Match") +
  theme_bw()

# Save processed data for models.R ----

# visual_assoc_data: straightness_to_food, used for Visual Associative Learning models
# path_integration_data: straightness_back, used for Path Integration models
# door_result: previous door visit data, used for Previous Door models

visual_assoc_data <- read_csv(here("visual-cue_path-integration", "data", "processed", "master_distance.csv")) %>%
  mutate(unique_trial_ID = as.factor(paste(season, ID, trial, sep = "_")),
         trial    = as.character(trial),
         trial    = str_remove(trial, "^T"),
         trial_n  = as.numeric(trial),
         trial    = factor(trial, levels = as.character(sort(unique(trial_n)))),
         season   = as.factor(season),
         ID       = as.factor(ID),
         category = case_when(season == "summer" ~ "summer_wild",
                              season == "winter" & grepl("^2021", ID) ~ "winter_wild",
                              season == "winter" & grepl("^2020", ID) ~ "winter_captive",
                              season == "winter" & grepl("^2024", ID) ~ "winter_wild",
                              season == "spring" & grepl("^2024", ID) ~ "spring_wild",
                              season == "spring" & grepl("^2020", ID) ~ "spring_captive"),
         status   = case_when(season == "summer" ~ "wild",
                              season == "winter" & grepl("^2021", ID) ~ "wild",
                              season == "winter" & grepl("^2020", ID) ~ "captive",
                              season == "winter" & grepl("^2024", ID) ~ "wild",
                              season == "spring" & grepl("^2024", ID) ~ "wild",
                              season == "spring" & grepl("^2020", ID) ~ "captive"),
         status   = as.factor(status),
         category = factor(category, levels = category_levels),
         season   = factor(season, levels = c("summer", "winter", "spring")),
         status   = factor(status, levels = c("wild", "captive"))) %>%
  relocate(category, .after = status) %>%
  filter(!is.na(straightness_to_food)) %>%
  mutate(straightness_to_food = if (max(straightness_to_food) == 1)
    constrain_open_interval(straightness_to_food) else straightness_to_food)

path_integration_data <- read_csv(here("visual-cue_path-integration", "data", "processed", "master_distance.csv")) %>%
  mutate(unique_trial_ID = as.factor(paste(season, ID, trial, sep = "_")),
         trial    = as.character(trial),
         trial    = str_remove(trial, "^T"),
         trial_n  = as.numeric(trial),
         trial    = factor(trial, levels = as.character(sort(unique(trial_n)))),
         season   = as.factor(season),
         ID       = as.factor(ID),
         category = case_when(season == "summer" ~ "summer_wild",
                              season == "winter" & grepl("^2021", ID) ~ "winter_wild",
                              season == "winter" & grepl("^2020", ID) ~ "winter_captive",
                              season == "winter" & grepl("^2024", ID) ~ "winter_wild",
                              season == "spring" & grepl("^2024", ID) ~ "spring_wild",
                              season == "spring" & grepl("^2020", ID) ~ "spring_captive"),
         status   = case_when(season == "summer" ~ "wild",
                              season == "winter" & grepl("^2021", ID) ~ "wild",
                              season == "winter" & grepl("^2020", ID) ~ "captive",
                              season == "winter" & grepl("^2024", ID) ~ "wild",
                              season == "spring" & grepl("^2024", ID) ~ "wild",
                              season == "spring" & grepl("^2020", ID) ~ "captive"),
         status   = as.factor(status),
         category = factor(category, levels = category_levels),
         season   = factor(season, levels = c("summer", "winter", "spring")),
         status   = factor(status, levels = c("wild", "captive"))) %>%
  relocate(category, .after = status)

saveRDS(visual_assoc_data, here("visual-cue_path-integration", "data", "processed", "visual_assoc_data.rds"))
saveRDS(path_integration_data, here("visual-cue_path-integration", "data", "processed", "path_integration_data.rds"))
saveRDS(door_result, here("visual-cue_path-integration", "data", "processed", "door_result.rds"))
