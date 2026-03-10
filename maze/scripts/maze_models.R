library(here)
library(tidyverse)
library(ggdist)
library(bayesplot)
library(rstan)
library(brms)
library(tidybayes)
library(bayestestR)
library(rstanarm)
library(BayesFactor)
library(cmdstanr)

source(here("utils.R"))

## Load data

maze <- readRDS(here("maze", "data", "results", "maze.rds"))
deviations <- readRDS(here("maze", "data", "results", "deviations.rds"))
deviation_counts <- readRDS(here("maze", "data", "results", "deviation_counts.rds"))

## Models ----

set.seed(1234)

# beta models ----

priors_beta <- c(set_prior("normal(0, 1)", class = "Intercept"),
                 set_prior("normal(0, 1)", class = "b", coef = "trial_n"),
                 set_prior("normal(-0.5, 1)", class = "b", coef = "seasonwinter"),  
                 set_prior("normal(-0.3, 1)", class = "b", coef = "seasonspring"),
                 set_prior("gamma(2, 0.1)", class = "phi"),
                 set_prior("exponential(2)", class = "sd"))

beta_model <- brm(formula = directional_path_efficiency ~ trial_n + season + status + (1 | ID),
                  data = maze,  family = Beta(link = "logit"),
                  prior = priors_beta,  chains = 4, cores = 4,  
                  iter = 8000,  warmup = 4000, seed = 1234,
                  control = list(adapt_delta = 0.99, max_treedepth = 15))

pp_check(beta_model, ndraws=100)
conditional_effects(beta_model)

# beta_model_RS: random slopes model, fitted for comparison.
# Retained to document model selection process; not used in final plots.
beta_model_RS <- brm(formula = directional_path_efficiency ~ trial_n + season + status + (trial_n | ID),
                  data = maze,  family = Beta(link = "logit"),
                  prior = priors_beta,  chains = 4, cores = 4,  
                  iter = 6000,  warmup = 3000, seed = 1234,
                  control = list(adapt_delta = 0.99, max_treedepth = 15))

priors_smooth <- c(set_prior("normal(0, 1)", class = "Intercept"),
                 set_prior("normal(0, 0.5)", class = "b"),
                 set_prior("gamma(2, 0.1)", class = "phi"),
                 set_prior("exponential(2)", class = "sd"))

smooth_model_season <- brm(directional_path_efficiency ~ s(trial_n, by = season) + status + (1 | ID),
                           data = maze, family = Beta(link = "logit"),
                           prior = priors_smooth, chains = 4, cores = 4, 
                           iter = 6000, warmup = 3000, seed = 1234,
                           control = list(adapt_delta = 0.99, max_treedepth = 15))

smooth_model_status <- brm(directional_path_efficiency ~ s(trial_n, by = status) + season + (1 | ID),
                           data = maze, family = Beta(link = "logit"),
                           prior = priors_smooth, chains = 4, cores = 4, 
                           iter = 6000, warmup = 3000, seed = 1234,
                           control = list(adapt_delta = 0.99, max_treedepth = 15))

priors_interaction <- c(set_prior("normal(1, 1)", class = "Intercept"),
                        set_prior("normal(0, 2)", class = "sds"),  # smooths
                        set_prior("exponential(1)", class = "sd", group = "ID"),
                        set_prior("gamma(2, 0.1)", class = "phi"))

beta_model_interaction <- brm(directional_path_efficiency ~ s(trial_n, by= category) + (1 | ID),
                              data = maze, family = Beta(link = "logit"),
                              prior = priors_interaction, chains = 4, cores = 4,
                              iter = 8000, warmup = 4000,  seed = 1234, control = list(adapt_delta = 0.99))

# model comparison: spline by season vs status vs category interaction. beta_model_interaction selected as final non-mixture model.
smooth_model_season <- add_criterion(smooth_model_season, "loo")
smooth_model_status <- add_criterion(smooth_model_status, "loo")
beta_model_interaction   <- add_criterion(beta_model_interaction, "loo")

loo_compare(smooth_model_season, smooth_model_status, beta_model_interaction, criterion = "loo")

# Mixture Models ----

# mix_full: initial mixture model without splines, linear effects only.
# Retained to document model selection; mix_maze_refined preferred based on better posterior predictive checks and ability to capture non-linear
# trial-by-trial learning trajectories.
priors_mix_full <- c(set_prior("normal(1, 1)", class = "Intercept", dpar = "mu1"),
                     set_prior("normal(1, 1)", class = "Intercept", dpar = "mu2"),
                     set_prior("normal(0, 1)", class = "Intercept", dpar = "theta1"),
                     set_prior("normal(0, 1)", class = "b", coef = "trial_n", dpar = "mu1"),
                     set_prior("normal(0, 1)", class = "b", coef = "trial_n", dpar = "mu2"),
                     set_prior("normal(0, 0.5)", class = "b", coef = "trial_n", dpar = "theta1"),
                     set_prior("normal(-0.5, 0.5)", class = "b", coef = "seasonwinter", dpar = "mu1"),  
                     set_prior("normal(-0.3, 0.5)", class = "b", coef = "seasonspring", dpar = "mu1"),
                     set_prior("normal(-0.5, 0.5)", class = "b", coef = "seasonwinter", dpar = "mu2"),
                     set_prior("normal(-0.3, 0.5)", class = "b", coef = "seasonspring", dpar = "mu2"),
                     set_prior("normal(-0.3, 0.5)", class = "b", coef = "seasonwinter", dpar = "theta1"),
                     set_prior("normal(-0.2, 0.5)", class = "b", coef = "seasonspring", dpar = "theta1"),
                     set_prior("normal(0.5, 0.5)", class = "b", coef = "statuscaptive", dpar = "mu1"),
                     set_prior("normal(0.5, 0.5)", class = "b", coef = "statuscaptive", dpar = "mu2"),
                     set_prior("normal(0.3, 0.5)", class = "b", coef = "statuscaptive", dpar = "theta1"),
                     set_prior("gamma(2, 0.1)", class = "phi1"),
                     set_prior("gamma(2, 0.1)", class = "phi2"))

mix_full <- brm(bf(directional_path_efficiency ~ 1) +
                  lf(mu1 ~ trial_n + season + status, cmc = FALSE) +
                  lf(mu2 ~ trial_n + season + status, cmc = FALSE) +
                  lf(theta1 ~ trial_n + season + status, cmc = FALSE),
                data = maze, prior = priors_mix_full,
                family = mixture(Beta(link = "logit"), Beta(link = "logit")),
                chains = 4, cores = 4, iter = 20000, warmup = 8000,  seed = 1234,
                control = list(adapt_delta = 0.999, max_treedepth = 20))

pp_check(mix_full, ndraw=1000)
post_check <- as_draws_df(mix_full)
mcmc_trace(post_check, pars = c("b_mu1_Intercept", "b_mu2_Intercept"))
mcmc_pairs(post_check, pars = c("b_mu1_Intercept", "b_mu2_Intercept"))

### Final model: mixture Beta with splines. ----
# Preferred over mix_full based on better pp_check and ability to capture non-linear trial-by-trial trajectories.

priors_maze_spline <- c(set_prior("normal(-0.5, 0.5)", class = "Intercept", dpar = "mu1"),
                        set_prior("normal(2.5, 0.5)", class = "Intercept", dpar = "mu2"),
                        #Intercepts: mu1 (Low peak ~0.4), mu2 (High peak ~0.9)
                        
                        set_prior("normal(0, 1)", class = "Intercept", dpar = "theta1"),
                        set_prior("student_t(3, 0, 1)", class = "sds", dpar = "mu1"),
                        set_prior("student_t(3, 0, 1)", class = "sds", dpar = "mu2"),
                        set_prior("normal(0, 0.5)", class = "b", dpar = "mu1"),
                        set_prior("normal(0, 0.5)", class = "b", dpar = "mu2"),
                        set_prior("normal(0, 0.5)", class = "b", dpar = "theta1"),
                        set_prior("gamma(2, 0.1)", class = "phi1"), 
                        set_prior("gamma(10, 0.1)", class = "phi2"))

mix_maze_refined <- brm(bf(directional_path_efficiency ~ 1,
                           # splines to capture the "peak then drop" behavior
                           mu1 ~ s(trial_n, k = 5) + season + status + (1|ID),
                           mu2 ~ s(trial_n, k = 5) + season + status + (1|ID),
                           # theta1 models the probability of being in the low-performance group
                           theta1 ~ trial_n + season + status), 
                        data = maze, prior = priors_maze_spline, family = mixture(Beta(link = "logit"), Beta(link = "logit")),
                        init = 0, chains = 4, cores = 4, iter = 8000, warmup = 2000,  seed = 1234,
                        control = list(adapt_delta = 0.99))

pp_check(mix_maze_refined, ndraw=1000)
post_check <- as_draws_df(mix_maze_refined)
mcmc_trace(post_check, pars = c("b_mu1_Intercept", "b_mu2_Intercept"))
mcmc_pairs(post_check, pars = c("b_mu1_Intercept", "b_mu2_Intercept"))

# #Save model as rds if needed. It will save in the project root.
# saveRDS(mix_maze_refined, "maze_model.rds")

## PLOTS ----

## Data preparation ----

new_season <- maze %>%
  distinct(ID, trial_n, trial, season, status, category)

fitted_all <- fitted(mix_maze_refined, newdata = new_season) %>%
  data.frame() %>%
  bind_cols(new_season) %>%
  mutate(category = factor(category, levels = c("summer_wild", "winter_wild", "spring_wild",
                                                "winter_captive", "spring_captive")),
         season = factor(season, levels = c("summer", "winter", "spring")),
         status = ifelse(grepl("captive", category), "captive", "wild"))

fitted_avg_all <- fitted_all %>%
  group_by(season, trial) %>%
  summarize(AvgEstimate = mean(Estimate), Lower = mean(Q2.5), Upper = mean(Q97.5), .groups = 'drop')


ggplot(fitted_all, aes(x = Estimate, y = category, fill = category)) +
  stat_halfeye(adjust = 1, width = 0.2, .width = 0, justification = 0, point_colour = NA) +
  geom_point(alpha = 0.4, size = 1, position = position_jitter(height = 0.1)) +
  # geom_point(size = 1.5, alpha = .2, position = position_jitter(seed = 1, width = .1)) +
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.5, position = position_nudge(y = -0.15)) +
  scale_fill_manual(values = category_colors, labels = category_names) +
  scale_y_discrete(labels = category_names) +
  labs(x = "Fitted Estimate", y = "Category") +
  #scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  theme_bw() +
  theme(legend.position = "none")

fitted_avg_all <- fitted_all %>%
  group_by(category, trial) %>%
  summarize(AvgEstimate = mean(Estimate), Lower = mean(Q2.5), Upper = mean(Q97.5), .groups = 'drop')

ggplot(fitted_avg_all, aes(x = trial, y = AvgEstimate, group = category, color = category, fill = category)) +
  geom_point(data = maze, aes(x = trial, y = path_efficiency, color = category), alpha = 0.2) +
  geom_line(data = fitted_avg_all, aes(x = trial, y = AvgEstimate, group = category)) +  
  geom_ribbon(data = fitted_avg_all,
              aes(x = trial, ymin = Lower, ymax = Upper, fill = category, group = category),
              alpha = 0.2, inherit.aes = FALSE) +
  facet_wrap(~category, scales = "free_y", ncol = 5, labeller = as_labeller(category_names)) +
  scale_color_manual(values = category_colors, labels = category_names) +
  scale_fill_manual(values = category_colors, labels = category_names) +
  theme_bw() +
  theme(legend.position = "none")

theta_data <- fitted(mix_maze_refined, dpar = "theta1", 
                     newdata = new_season, re_formula = NA) %>%
  data.frame() %>%
  bind_cols(new_season)

# Calculate the probability of the SECOND (High-Efficiency) component
# Since theta1 + theta2 = 1, we subtract the Estimate from 1.
theta_data <- theta_data %>%
  mutate(Prob_High_Eff = 1 - Estimate,
         Prob_Lower = 1 - Q97.5, 
         Prob_Upper = 1 - Q2.5)

# Plot probability of High-Efficiency Strategy
ggplot(theta_data, aes(x = trial, y = Prob_High_Eff, group = category, color = category, fill = category)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = Prob_Lower, ymax = Prob_Upper), alpha = 0.2, color = NA) +
  geom_point() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = category_colors) +
  scale_fill_manual(values = category_colors) +
  labs(x = "Trial Number",
       y = expression(paste("Probability of High-Efficiency Component (", theta[2], ")"))) +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(~category, scales = "free_y", ncol = 5, labeller = as_labeller(category_names))


## Deviation ----

deviation_counts$category <- factor(deviation_counts$category, levels = c("summer_wild", "winter_wild", "spring_wild", "winter_captive", "spring_captive"))
deviation_counts <- deviation_counts %>% 
  mutate(trial_n = as.numeric(trial)) %>%
  separate(unique_trial_ID, 
           into = c("season", "ID", NA), 
           sep = "_", 
           remove = FALSE)

# How many total deviations happen in a trial, and how does this vary by trial number and group?
get_prior(deviation_count ~ trial_n * category + (1 | ID), family = negbinomial(), data = deviation_counts)
priors_deviation <- c(prior(normal(-0.2, 0.1), class = "b", coef = "trial_n"),
                      prior(normal(0.3, 0.5), class = "b", coef = "categorywinter_captive"), 
                      prior(normal(0.5, 0.5), class = "b", coef = "categorywinter_wild"),
                      prior(normal(0.7, 0.5), class = "b", coef = "categoryspring_wild"), 
                      prior(normal(-0.05, 0.2), class = "b", coef = "trial_n:categorywinter_captive"),
                      prior(normal(-0.05, 0.2), class = "b", coef = "trial_n:categorywinter_wild"),
                      prior(normal(0, 0.2), class = "b", coef = "trial_n:categoryspring_wild"),
                      prior(normal(1.5, 0.5), class = "Intercept"),
                      prior(exponential(2), class = "sd", group = "ID"),
                      prior(exponential(1), class = "shape"))

fit_deviation <- brm(deviation_count ~ trial_n * category + (1 | ID), family = negbinomial(), data = deviation_counts,
                     prior = priors_deviation,
                     chains = 4, iter = 4000, seed = 1234,
                     control = list(adapt_delta = 0.99, max_treedepth = 15))

new_d <- deviation_counts %>%
  distinct(ID, category, trial, trial_n)
fitted_deviation <- fitted(fit_deviation,
                           newdata = new_d) %>%  
  data.frame() %>%
  bind_cols(new_d) %>%
  mutate(ID = str_c("ID[", ID, "]"),
         category = factor(category),
         trial = factor(trial))

fitted_avg_deviation <- fitted_deviation %>%
  group_by(category, trial_n) %>%
  summarize(AvgEstimate = mean(Estimate),
            Lower = mean(Q2.5),
            Upper = mean(Q97.5))

ggplot(deviation_counts %>% filter(deviation_count <= 50), aes(x = category, y = deviation_count, fill = category)) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1.5, shape = 21, stroke = 0.2, color = "black") +
  scale_color_manual(values = category_colors, labels = category_names) +
  scale_fill_manual(values = category_colors, labels = category_names) +
  geom_boxplot(data = fitted_avg_deviation, aes(x = category, y = AvgEstimate, group = category, color = category), linewidth = 1) +
  labs(x = "Trial",
       y = "Number of Deviations",
       fill = "Category", color = "Category") +
  theme_bw()

ggplot(deviation_counts, aes(x = trial, y = deviation_count, fill = category)) +
 # geom_bar(stat = "identity", position = "dodge") +
  scale_color_manual(values = category_colors, labels = category_names) +
  scale_fill_manual(values = category_colors, labels = category_names) +
  geom_line(data = fitted_avg_deviation, aes(x = trial_n, y = AvgEstimate, group = category, color = category), linewidth = 1) +
  geom_ribbon(data = fitted_avg_deviation,
              aes(x = trial_n, ymin = Lower, ymax = Upper, fill = category, group = category),
              alpha = 0.07, inherit.aes = FALSE) +
  labs(x = "Trial",
       y = "Number of Deviations",
       fill = "Category", color = "Category") +
  theme_bw()


all_points <- expand.grid(unique_trial_ID = unique(deviations$unique_trial_ID),
                          deviation_point = 1:13) %>%
  left_join(distinct(deviations[, c("unique_trial_ID", "trial_n", "category", "ID")]), by = "unique_trial_ID") %>%
  mutate(deviated = as.integer(paste(unique_trial_ID, deviation_point) %in%
                                 paste(deviations$unique_trial_ID, deviations$deviation_point)))

all_points$category <- factor(all_points$category, levels = c("summer_wild", "winter_wild", "spring_wild", "winter_captive", "spring_captive"))

get_prior(deviated ~ deviation_point * category + (1 | ID), data = all_points, family = bernoulli())

priors_points <- c(prior(normal(0, 1), class = "Intercept"),
                   prior(normal(0, 0.5), class = "b", coef = "deviation_point"),
                   prior(normal(0, 0.5), class = "b", coef = "categoryspring_captive"),
                   prior(normal(0, 0.5), class = "b", coef = "categoryspring_wild"),
                   prior(normal(0, 0.5), class = "b", coef = "categorywinter_captive"),
                   prior(normal(0, 0.5), class = "b", coef = "categorywinter_wild"),
                   prior(normal(0, 0.3), class = "b", coef = "deviation_point:categoryspring_captive"),
                   prior(normal(0, 0.3), class = "b", coef = "deviation_point:categoryspring_wild"),
                   prior(normal(0, 0.3), class = "b", coef = "deviation_point:categorywinter_captive"),
                   prior(normal(0, 0.3), class = "b", coef = "deviation_point:categorywinter_wild"),
                   prior(exponential(2), class = "sd"))

fit_binary_point <- brm(deviated ~ deviation_point * category + (1 | ID), 
                        data = all_points, family = bernoulli(),
                        chains = 4, iter = 4000,  seed = 1234,
                        control = list(adapt_delta = 0.99, max_treedepth = 15))


# fitted values on probability scale
fitted_binary <- fitted(fit_binary_point, 
                           newdata = expand.grid(deviation_point = 1:13,
                                                 category = levels(all_points$category), ID = NA),
                           re_formula = NA, summary = TRUE)


plot_data <- cbind(expand.grid(deviation_point = 1:13,  category = levels(all_points$category)), 
                   fitted_binary)

ggplot(plot_data, aes(x = deviation_point, y = Estimate, color = category)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = category), alpha = 0.2, color = NA) +
  scale_color_manual(values = category_colors, labels = category_names) +
  scale_fill_manual(values = category_colors, labels = category_names) +
  labs(x = "1 = entrance, 13 = exit",
       y = "Predicted probability of deviation",
       color = "Category", fill = "Category") +
  theme_bw(base_size = 14)

