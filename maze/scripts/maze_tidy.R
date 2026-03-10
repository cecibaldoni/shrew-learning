# Libraries ----

library(here)
library(tidyverse)
library(ggdist)

source(here("utils.R"))

# Load Data ----

maze <- read.csv(here("maze", "data", "results", "maze_results.csv"))
deviations <- read.csv(here("maze", "data", "results", "deviation_report.csv"))

all_maze <- read.csv(here("maze", "data", "results", "all_track.csv")) %>% 
  mutate(unique_trial_ID = as.factor(paste(season, ID, trial, sep = "_"))) %>% 
  mutate(category = case_when(season == "summer" ~ "summer_wild",
                              season == "winter" & grepl("^2021", ID) ~ "winter_captive",
                              season == "winter" & grepl("^2024", ID) ~ "winter_wild",
                              season == "spring" & grepl("^2021", ID) ~ "spring_captive",
                              season == "spring" & grepl("^2024", ID) ~ "spring_wild"),
         #create captivity "status" (wild or captive)
         status = case_when(season == "summer" ~ "wild",
                            season == "winter" & grepl("^2021", ID) ~ "captive",
                            season == "winter" & grepl("^2024", ID) ~ "wild",
                            season == "spring" & grepl("^2021", ID) ~ "captive",
                            season == "spring" & grepl("^2024", ID) ~ "wild"))

## Tidy ####
maze <- maze %>% 
  mutate(ID = as.factor(ID),
         season = as.factor(season),
         trial = factor(trial, levels = c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9", "T10"), ordered = TRUE),
         category = case_when(season == "summer" ~ "summer_wild",
                              season == "winter" & grepl("^2021", ID) ~ "winter_captive",
                              season == "winter" & grepl("^2024", ID) ~ "winter_wild",
                              season == "spring" & grepl("^2021", ID) ~ "spring_captive",
                              season == "spring" & grepl("^2024", ID) ~ "spring_wild"),
         status = case_when(season == "summer" ~ "wild",
                            season == "winter" & grepl("^2021", ID) ~ "captive",
                            season == "winter" & grepl("^2024", ID) ~ "wild",
                            season == "spring" & grepl("^2021", ID) ~ "captive",
                            season == "spring" & grepl("^2024", ID) ~ "wild"),
         category = as.factor(category),
         status = as.factor(status),
         season_ID = paste(season, ID, sep = "_"),
         trial_n = as.numeric(trial),
         season_ID = as.factor(season_ID),
         unique_trial_ID = as.factor(unique_trial_ID))
maze$season <- factor(maze$season, levels = c("summer", "winter", "spring"))
maze$status <- factor(maze$status, levels = c("wild", "captive"))
str(maze)

min(maze$directional_path_efficiency)
mean(maze$directional_path_efficiency)
max(maze$directional_path_efficiency)
#percentage of trials in which shrew_path_length is shorter than correct_path_length
mean(maze$shrew_path_length < maze$correct_path_length) * 100


## if max(maze$directional_path_efficiency) ==1, use function in utils.R put it between (0,1)

if (max(maze$directional_path_efficiency) == 1) {
  maze$directional_path_efficiency <- constrain_open_interval(maze$directional_path_efficiency)
}

deviations <- deviations %>% 
  mutate(ID = as.factor(ID),
         season = as.factor(season),
         trial = factor(trial, levels = c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9", "T10"), ordered = TRUE),
         trial_n = as.numeric(trial),
         category = case_when(season == "summer" ~ "summer_wild",
                              season == "winter" & grepl("^2021", ID) ~ "winter_captive",
                              season == "winter" & grepl("^2024", ID) ~ "winter_wild",
                              season == "spring" & grepl("^2021", ID) ~ "spring_captive",
                              season == "spring" & grepl("^2024", ID) ~ "spring_wild"),
         category = as.factor(category),
         unique_trial_ID = paste(season, ID, trial, sep = "_"),
         unique_trial_ID = as.factor(unique_trial_ID)) %>% 
  select(-deviation_start) %>% 
  filter(deviation_frame != 0,
         deviation_length != 0) #%>% 
#filter(unique_trial_ID != "20210802-8_winter_T2")

str(deviations)
min(deviations$deviation_length)

deviation_counts <- deviations %>%
  group_by(ID, category, trial, trial_n, deviation_point) %>%
  summarise(deviation_count = n())

# EDA ####

a <-ggplot(maze, aes(x = directional_path_efficiency)) +
  geom_histogram(fill = "blue", alpha = 0.4)
b <- ggplot(maze, aes(x = directional_path_efficiency)) +
  geom_density(fill = "blue", alpha = 0.4) +
  facet_wrap(~category)
c <- ggplot(maze, aes(x = directional_path_efficiency)) +
  geom_density(fill = "blue", alpha = 0.4) +
  facet_wrap(~season)
d <- ggplot(maze, aes(x = directional_path_efficiency)) +
  geom_density(fill = "blue", alpha = 0.4) +
  facet_wrap(~status)
ggpubr::ggarrange(a,b,c,d)


ggplot(maze) +
  geom_jitter(mapping = aes(x = trial, y = path_efficiency, color = status, group = status), alpha =0.3) +
  geom_smooth(mapping = aes(x = trial, y = path_efficiency, color = status, group = status)) +
  facet_wrap(~status) +
  theme_bw()

ggplot(maze %>%  mutate(season = factor(season, levels = c("summer", "winter", "spring")))) +
  geom_jitter(mapping = aes(x = trial, y = path_efficiency, color = season, group = season), alpha =0.3) +
  geom_smooth(mapping = aes(x = trial, y = path_efficiency, color = season, group = season)) +
  scale_color_manual(values = season_colors, labels = season_names) +
  scale_fill_manual(values = season_colors, labels = season_names) +
  facet_wrap(~season) +
  theme_bw() +
  theme(legend.position = "none")

ggplot(maze %>%  mutate(season = factor(season, levels = c("summer", "winter", "spring")))) +
  geom_jitter(mapping = aes(x = trial, y = directional_path_efficiency, color = season, group = season), alpha =0.3) +
  geom_smooth(mapping = aes(x = trial, y = directional_path_efficiency, color = season, group = season)) +
  scale_color_manual(values = season_colors, labels = season_names) +
  scale_fill_manual(values = season_colors, labels = season_names) +
  facet_wrap(~season) +
  theme_bw() +
  theme(legend.position = "none")

ggplot(maze, aes(x = path_efficiency, y = directional_path_efficiency, color = perc_backward_buffer)) +
  geom_point(size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_color_viridis_c(option = "plasma")

ggplot(maze) +
  geom_jitter(mapping = aes(x = trial, y = directional_path_efficiency, color = category, group = category), alpha =0.3) +
  geom_smooth(mapping = aes(x = trial, y = directional_path_efficiency, color = category, group = category)) +
  facet_wrap(~season) +
  theme_bw()

ggplot(maze, aes(x = path_efficiency, y = category, fill = category)) +
  ggridges::geom_density_ridges(alpha = 0.6) +
  scale_color_manual(values = category_colors, labels = category_names) +
  scale_fill_manual(values = category_colors, labels = category_names) +
  theme_bw()

ggplot(maze) +
  geom_jitter(mapping = aes(x = trial, y = path_efficiency, color = category, group = category), alpha =0.3) +
  geom_smooth(mapping = aes(x = trial, y = path_efficiency, color = category, group = category), se = FALSE) +
  scale_color_manual(values = category_colors, labels = category_names) +
  scale_fill_manual(values = category_colors, labels = category_names) +
  facet_wrap(~ID) +
  theme_bw()
ggplot(maze) +
  geom_jitter(mapping = aes(x = trial, y = first_deviation_point, color = category, group = category), alpha =0.3) +
  geom_smooth(mapping = aes(x = trial, y = first_deviation_point, color = category, group = category), se = FALSE) +
  scale_color_manual(values = category_colors, labels = category_names) +
  scale_fill_manual(values = category_colors, labels = category_names) +
  theme_bw()

deviations_by_trial_category <- deviations %>%
  group_by(trial, category) %>%
  summarise(count_deviations = n())

ggplot(deviations_by_trial_category, aes(x = trial, y = count_deviations, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_color_manual(values = category_colors, labels = category_names) +
  scale_fill_manual(values = category_colors, labels = category_names) +
  labs(x = "Trial",
       y = "Number of Deviations",
       fill = "category") +
  theme_bw()

deviations <- deviations %>%
  mutate(frame_interval = (deviation_frame %/% 300) * 300)

aggregated_deviations <- deviations %>%
  group_by(ID, season, trial, category, unique_trial_ID, frame_interval, deviation_point) %>%
  summarise(total_deviation_length = sum(deviation_length, na.rm = TRUE),
            deviation_points = first(deviation_point),
            deviation_points_list = paste(unique(deviation_point), collapse = ", ")) %>%
  ungroup()

deviation_counts <- aggregated_deviations %>%
  group_by(unique_trial_ID, category,trial, deviation_point) %>%
  summarise(deviation_count = n())
ggplot(deviation_counts, aes(x = deviation_count)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Deviation Counts", x = "Deviation Count", y = "Frequency") +
  theme_minimal()

ggplot(deviation_counts, aes(x = trial, fill = category, colour = category)) +
  geom_bar() +
  scale_color_manual(values = category_colors, labels = category_names) +
  scale_fill_manual(values = category_colors, labels = category_names) +
  facet_wrap(~category)

ggplot(deviations, aes(x = trial_n, y = deviation_length, fill = category, colour = category)) +
  geom_point() +
  geom_smooth() +
  scale_color_manual(values = category_colors, labels = category_names) +
  scale_fill_manual(values = category_colors, labels = category_names) +
  facet_wrap(~category)

ggplot(maze, aes(x = category, y = path_efficiency)) +
  geom_violin() +
  geom_boxplot(width = 0.1, outlier.shape = NA) 

## Speed ####

maze_list <- split(all_maze, all_maze$unique_trial_ID)

for (i in 1:length(maze_list)) {
  data <- maze_list[[i]]
  
  # Replace infinite values with NA and drop rows where 'x' is NA
  data <- data %>%
    mutate(x = ifelse(is.infinite(x), NA, x)) %>%
    drop_na(x)
  
  x_diff <- c(NA, diff(data$x))
  y_diff <- c(NA, diff(data$y))
  time_diff <- c(NA, diff(data$time))
  
  distance <- sqrt(x_diff^2 + y_diff^2)
  speed <- ifelse(time_diff > 0, distance / time_diff, NA)
  data$speed_new <- speed
  maze_list[[i]] <- data
}

calculate_speed_summary <- function(df) {
  df %>%
    group_by(unique_trial_ID, category, trial) %>%
    summarise(mean_speed = mean(speed_new, na.rm = TRUE), .groups = 'drop') %>%
    ungroup()
}

speed_summaries <- lapply(maze_list, calculate_speed_summary)
speed_summary <- bind_rows(speed_summaries)

maze <- merge(maze, speed_summary[c("unique_trial_ID", "mean_speed")], by = "unique_trial_ID", all.x = TRUE)
str(maze)


## Save tidy data for models

saveRDS(maze, here("maze", "data", "results", "maze.rds"))
saveRDS(deviations, here("maze", "data", "results", "deviations.rds"))
saveRDS(deviation_counts, here("maze", "data", "results", "deviation_counts.rds"))
