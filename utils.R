category_colors <- c("summer_wild" = "#D16103", "winter_captive" = "#0AA9A9", 
                     "winter_wild" = "#0A33A9", "spring_captive" = "#8AD918", 
                     "spring_wild" = "#0E8125")
category_names <- c("summer_wild" = "Summer Wild", "winter_wild" = "Winter Wild",  
                    "spring_wild" = "Spring Wild", "winter_captive" = "Winter Captive",  
                    "spring_captive" = "Spring Captive")

season_colors <- c("summer" = "#D16103", "winter" = "#0A33A9", "spring" = "#006400")
season_names <- c("summer" = "Summer", "winter" = "Winter", "spring" = "Spring")


constrain_open_interval <- function(x) {
  N <- length(x)
  s <- 0.5
  ifelse(x == 1, (N - 1 + s) / N, x)
}