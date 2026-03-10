# Libraries ----
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

# Load data ----

# visual_assoc_data:     straightness_to_food — Visual Associative Learning
# path_integration_data: straightness_back    — Path Integration
# door_result:           previous door visits  — Door Memory

visual_assoc_data <- readRDS(here("visual-cue_path-integration", "data", "processed", "visual_assoc_data.rds"))
path_integration_data <- readRDS(here("visual-cue_path-integration", "data", "processed", "path_integration_data.rds"))
door_result <- readRDS(here("visual-cue_path-integration", "data", "processed", "door_result.rds"))

set.seed(1234)

# Visual Associative Learning (outcome: straightness_to_food) ----

## EDA ----

summary(visual_assoc_data)

ggplot(visual_assoc_data, aes(x = status, y = straightness_to_food)) +
  ggdist::stat_halfeye()

a <- ggplot(visual_assoc_data, aes(x = straightness_to_food)) +
  geom_density(fill = "blue", alpha = 0.4)
b <- ggplot(visual_assoc_data, aes(x = straightness_to_food)) +
  geom_density(fill = "blue", alpha = 0.4) +
  facet_wrap(~status)
c <- ggplot(visual_assoc_data, aes(x = straightness_to_food)) +
  geom_density(fill = "blue", alpha = 0.4) +
  facet_wrap(~season)
d <- ggplot(visual_assoc_data, aes(x = straightness_to_food)) +
  geom_density(fill = "blue", alpha = 0.4) +
  facet_wrap(~category)
ggpubr::ggarrange(a, b, c, d)

ggplot(visual_assoc_data) +
  geom_jitter(mapping = aes(x = trial, y = straightness_to_food, color = category, group = category), alpha = 0.3) +
  geom_smooth(mapping = aes(x = trial, y = straightness_to_food, color = category, group = category)) +
  facet_wrap(~season) +
  theme_bw()

ggplot(visual_assoc_data) +
  geom_jitter(mapping = aes(x = trial, y = walked_to, color = category, group = category), alpha = 0.3) +
  geom_smooth(mapping = aes(x = trial, y = walked_to, color = category, group = category)) +
  facet_wrap(~season, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none")

## Intermediate models — retained to document model selection ----

# Intercept-only mixture
priors_simple <- c(set_prior("normal(0, 1)", class = "Intercept", dpar = "mu1"),
                   set_prior("normal(0, 1)", class = "Intercept", dpar = "mu2"),
                   set_prior("gamma(2, 0.1)", class = "phi1"),
                   set_prior("gamma(2, 0.1)", class = "phi2"))

vac_mix_simple <- brm(bf(straightness_to_food ~ 1) +
                        lf(mu1 ~ 1, cmc = FALSE) +
                        lf(mu2 ~ 1, cmc = FALSE),
                      data = visual_assoc_data,
                      family = mixture(Beta(link = "logit"), Beta(link = "logit")),
                      prior = priors_simple,
                      chains = 4, cores = 4, iter = 6000, warmup = 3000, seed = 1234,
                      control = list(adapt_delta = 0.99, max_treedepth = 15))

summary(vac_mix_simple)
pp_check(vac_mix_simple)

# + trial_n
priors_trial <- c(set_prior("normal(0, 1)", class = "Intercept", dpar = "mu1"),
                  set_prior("normal(0, 1)", class = "Intercept", dpar = "mu2"),
                  set_prior("normal(0, 1)", class = "b", coef = "trial_n", dpar = "mu1"),
                  set_prior("normal(0, 1)", class = "b", coef = "trial_n", dpar = "mu2"),
                  set_prior("gamma(2, 0.1)", class = "phi1"),
                  set_prior("gamma(2, 0.1)", class = "phi2"))

vac_mix_trial <- brm(bf(straightness_to_food ~ 1) +
                       lf(mu1 ~ trial_n, cmc = FALSE) +
                       lf(mu2 ~ trial_n, cmc = FALSE),
                     data = visual_assoc_data,
                     family = mixture(Beta(link = "logit"), Beta(link = "logit")),
                     prior = priors_trial,
                     chains = 4, cores = 4, iter = 6000, warmup = 3000, seed = 1234,
                     control = list(adapt_delta = 0.995, max_treedepth = 15))

summary(vac_mix_trial)
pp_check(vac_mix_trial)

# + season
priors_season <- c(set_prior("normal(1, 1)", class = "Intercept", dpar = "mu1"),
                   set_prior("normal(1, 1)", class = "Intercept", dpar = "mu2"),
                   set_prior("normal(0, 1)", class = "b", coef = "trial_n", dpar = "mu1"),
                   set_prior("normal(0, 1)", class = "b", coef = "trial_n", dpar = "mu2"),
                   set_prior("normal(-0.5, 0.3)", class = "b", coef = "seasonwinter", dpar = "mu1"),
                   set_prior("normal(-0.3, 0.3)", class = "b", coef = "seasonspring", dpar = "mu1"),
                   set_prior("normal(-0.5, 0.3)", class = "b", coef = "seasonwinter", dpar = "mu2"),
                   set_prior("normal(-0.3, 0.3)", class = "b", coef = "seasonspring", dpar = "mu2"),
                   set_prior("gamma(2, 0.1)", class = "phi1"),
                   set_prior("gamma(2, 0.1)", class = "phi2"))

vac_mix_trial_season <- brm(bf(straightness_to_food ~ 1) +
                              lf(mu1 ~ trial_n + season, cmc = FALSE) +
                              lf(mu2 ~ trial_n + season, cmc = FALSE),
                            data = visual_assoc_data,
                            family = mixture(Beta(link = "logit"), Beta(link = "logit")),
                            prior = priors_season,
                            chains = 4, cores = 4, iter = 6000, warmup = 2000, seed = 1234,
                            control = list(adapt_delta = 0.995, max_treedepth = 20))

summary(vac_mix_trial_season)
pp_check(vac_mix_trial_season, ndraws = 50)

# + status (linear, no theta)
priors_full <- c(set_prior("normal(1, 1)", class = "Intercept", dpar = "mu1"),
                 set_prior("normal(1, 1)", class = "Intercept", dpar = "mu2"),
                 set_prior("normal(0, 1)", class = "b", coef = "trial_n", dpar = "mu1"),
                 set_prior("normal(0, 1)", class = "b", coef = "trial_n", dpar = "mu2"),
                 set_prior("normal(-0.5, 0.5)", class = "b", coef = "seasonwinter", dpar = "mu1"),
                 set_prior("normal(-0.3, 0.5)", class = "b", coef = "seasonspring", dpar = "mu1"),
                 set_prior("normal(-0.5, 0.5)", class = "b", coef = "seasonwinter", dpar = "mu2"),
                 set_prior("normal(-0.3, 0.5)", class = "b", coef = "seasonspring", dpar = "mu2"),
                 set_prior("normal(0.5, 0.5)", class = "b", coef = "statuscaptive", dpar = "mu1"),
                 set_prior("normal(0.5, 0.5)", class = "b", coef = "statuscaptive", dpar = "mu2"),
                 set_prior("gamma(2, 0.1)", class = "phi1"),
                 set_prior("gamma(2, 0.1)", class = "phi2"))

vac_mix_full <- brm(bf(straightness_to_food ~ 1) +
                      lf(mu1 ~ trial_n + season + status, cmc = FALSE) +
                      lf(mu2 ~ trial_n + season + status, cmc = FALSE),
                    data = visual_assoc_data,
                    family = mixture(Beta(link = "logit"), Beta(link = "logit")),
                    prior = priors_full,
                    chains = 4, cores = 4, iter = 8000, warmup = 2000, seed = 1234,
                    control = list(adapt_delta = 0.995, max_treedepth = 20))

summary(vac_mix_full)
post <- as_draws_df(vac_mix_full)
mcmc_trace(post, pars = c("b_mu1_Intercept", "b_mu2_Intercept"))
mcmc_pairs(post, pars = c("b_mu1_Intercept", "b_mu2_Intercept"))

# Separating the two mixture components with asymmetric intercept priors.
# vac_mix_full was the best starting point but assumed theta = 50/50.
# On logit scale: -1 is ~27% straightness, 1 is ~73% straightness.
mix_priors2 <- c(set_prior("normal(-1.5, 1)", class = "Intercept", dpar = "mu1"),
                 set_prior("normal(1, 1)", class = "Intercept", dpar = "mu2"),
                 set_prior("normal(0, 1)", class = "b", dpar = "mu1"),
                 set_prior("normal(0, 1)", class = "b", dpar = "mu2"),
                 set_prior("gamma(2, 0.1)", class = "phi1"),
                 set_prior("gamma(2, 0.1)", class = "phi2"),
                 set_prior("exponential(1)", class = "sd", dpar = "mu2"))

vac_final <- brm(formula = bf(straightness_to_food ~ 1,
                              mu1 ~ trial_n + season + status,
                              mu2 ~ trial_n + season + status + (1 | ID)
                              # theta1 ~ trial_n + season + status
                              ),
                 data = visual_assoc_data,
                 family = mixture("beta", "beta", order = "mu"), # order = "mu" enforces mu1 < mu2
                 prior = mix_priors2, chains = 4, iter = 8000, warmup = 2000, seed = 1234,
                 control = list(adapt_delta = 0.99))

pp_check(vac_final, ndraw = 1000)
post <- as_draws_df(vac_final)
mcmc_trace(post, pars = c("b_mu1_Intercept", "b_mu2_Intercept"))
mcmc_pairs(post, pars = c("b_mu1_Intercept", "b_mu2_Intercept"))

## Visual Associative Learning — Final model ----
# Splines on trial_n + modelled theta. Preferred over vac_final based on
# better pp_check and ability to capture non-linear learning trajectories.

mix_priors_to_food <- c(set_prior("normal(-1.5, 0.5)", class = "Intercept", dpar = "mu1"),
                        set_prior("normal(1.5, 0.5)", class = "Intercept", dpar = "mu2"),
                        set_prior("normal(0, 1)", class = "b", dpar = "mu1"),
                        set_prior("normal(0, 1)", class = "b", dpar = "mu2"),
                        set_prior("normal(0, 1)", class = "b", dpar = "theta1"),
                        set_prior("student_t(3, 0, 1)", class = "sds", dpar = "mu1"),
                        set_prior("student_t(3, 0, 1)", class = "sds", dpar = "mu2"),
                        set_prior("exponential(2)", class = "sd", dpar = "mu1"),
                        set_prior("exponential(2)", class = "sd", dpar = "mu2"),
                        set_prior("gamma(2, 2)", class = "phi1"),
                        set_prior("gamma(2, 2)", class = "phi2"))

vac_final2 <- brm(formula = bf(straightness_to_food ~ 1,
                               mu1 ~ s(trial_n, k = 5) + season + status + (1 | ID),
                               mu2 ~ s(trial_n, k = 5) + season + status + (1 | ID),
                               # linear effect for theta to stabilize mixture proportion
                               theta1 ~ trial_n + season + status),
                  data = visual_assoc_data,
                  family = mixture("beta", "beta", order = "mu"), # order = "mu" enforces mu1 < mu2
                  prior = mix_priors_to_food, chains = 4, iter = 8000, warmup = 4000, seed = 1234,
                  control = list(adapt_delta = 0.99))

pp_check(vac_final2, ndraw = 1000)
post <- as_draws_df(vac_final2)
mcmc_trace(post, pars = c("b_mu1_Intercept", "b_mu2_Intercept"))
mcmc_pairs(post, pars = c("b_mu1_Intercept", "b_mu2_Intercept"))


# #Save model as rds if needed. It will save in the project root.
# saveRDS(vac_final2, "cue_model.rds")


# Path Integration (outcome: straightness_back) ----

## EDA ----

e <- ggplot(path_integration_data, aes(x = straightness_back)) +
  geom_density(fill = "blue", alpha = 0.4)
f <- ggplot(path_integration_data, aes(x = straightness_back)) +
  geom_density(fill = "blue", alpha = 0.4) +
  facet_wrap(~status)
g <- ggplot(path_integration_data, aes(x = straightness_back)) +
  geom_density(fill = "blue", alpha = 0.4) +
  facet_wrap(~season)
h <- ggplot(path_integration_data, aes(x = straightness_back)) +
  geom_density(fill = "blue", alpha = 0.4) +
  facet_wrap(~category)
ggpubr::ggarrange(e, f, g, h)

ggplot(path_integration_data) +
  geom_jitter(mapping = aes(x = trial, y = straightness_back, color = category, group = category), alpha = 0.3) +
  geom_smooth(mapping = aes(x = trial, y = straightness_back, color = category, group = category)) +
  facet_wrap(~season) +
  theme_bw()

min(path_integration_data$straightness_back)
max(path_integration_data$straightness_back)

## Intermediate models — retained to document model selection; pi_final2 preferred ----

pi_mix_simple <- brm(bf(straightness_back ~ 1) +
                       lf(mu1 ~ 1, cmc = FALSE) +
                       lf(mu2 ~ 1, cmc = FALSE),
                     data = path_integration_data,
                     family = mixture(Beta(link = "logit"), Beta(link = "logit")),
                     prior = priors_simple,
                     chains = 4, cores = 4, iter = 6000, warmup = 3000, seed = 1234,
                     control = list(adapt_delta = 0.99, max_treedepth = 15))

summary(pi_mix_simple)
pp_check(pi_mix_simple, ndraw = 100)

pi_mix_trial <- brm(bf(straightness_back ~ 1) +
                      lf(mu1 ~ trial_n, cmc = FALSE) +
                      lf(mu2 ~ trial_n, cmc = FALSE),
                    data = path_integration_data,
                    family = mixture(Beta(link = "logit"), Beta(link = "logit")),
                    prior = priors_trial,
                    chains = 4, cores = 4, iter = 6000, warmup = 3000, seed = 1234,
                    control = list(adapt_delta = 0.995, max_treedepth = 15))

summary(pi_mix_trial)
pp_check(pi_mix_trial, ndraw = 100)

pi_mix_trial_season <- brm(bf(straightness_back ~ 1) +
                             lf(mu1 ~ trial_n + season, cmc = FALSE) +
                             lf(mu2 ~ trial_n + season, cmc = FALSE),
                           data = path_integration_data,
                           family = mixture(Beta(link = "logit"), Beta(link = "logit")),
                           prior = priors_season,
                           chains = 4, cores = 4, iter = 6000, warmup = 2000, seed = 1234,
                           control = list(adapt_delta = 0.995, max_treedepth = 20))

summary(pi_mix_trial_season)
pp_check(pi_mix_trial_season, ndraws = 50)
post_back <- as_draws_df(pi_mix_trial_season)
mcmc_trace(post_back, pars = c("b_mu1_Intercept", "b_mu2_Intercept"))
mcmc_pairs(post_back, pars = c("b_mu1_Intercept", "b_mu2_Intercept"))

pi_mix_full <- brm(bf(straightness_back ~ 1) +
                     lf(mu2 ~ trial_n + season + status + (1 | ID), cmc = FALSE) +
                     lf(mu1 ~ trial_n + season + status + (1 | ID), cmc = FALSE) +
                     lf(theta1 ~ trial_n + season + status, cmc = FALSE),
                   data = path_integration_data,
                   family = mixture(Beta(link = "logit"), Beta(link = "logit")),
                   prior = mix_priors2,
                   chains = 4, cores = 4, iter = 15000, warmup = 8000, seed = 1234,
                   control = list(adapt_delta = 0.999, max_treedepth = 20))

summary(pi_mix_full)
pp_check(pi_mix_full, ndraws = 100)
post_back <- as_draws_df(pi_mix_full)
mcmc_trace(post_back, pars = c("b_mu1_Intercept", "b_mu2_Intercept"))
mcmc_pairs(post_back, pars = c("b_mu1_Intercept", "b_mu2_Intercept"))

## Path Integration — Final model ----
# Splines on trial_n + modelled theta. Preferred over pi_mix_full based on
# better pp_check and ability to capture non-linear trajectories.

mix_priors_back <- c(set_prior("normal(-2.0, 0.3)", class = "Intercept", dpar = "mu1"),
                     set_prior("normal(1.0, 0.3)", class = "Intercept", dpar = "mu2"),
                     set_prior("normal(0, 0.5)", class = "b", dpar = "mu1"),
                     set_prior("normal(0, 0.5)", class = "b", dpar = "mu2"),
                     set_prior("normal(0, 0.5)", class = "b", dpar = "theta1"),
                     set_prior("student_t(3, 0, 0.5)", class = "sds", dpar = "mu1"),
                     set_prior("student_t(3, 0, 0.5)", class = "sds", dpar = "mu2"),
                     set_prior("exponential(3)", class = "sd", dpar = "mu1"),
                     set_prior("exponential(3)", class = "sd", dpar = "mu2"),
                     set_prior("gamma(2, 0.1)", class = "phi1"),
                     set_prior("gamma(2, 0.1)", class = "phi2"))

pi_final2 <- brm(bf(straightness_back ~ 1,
                    mu1 ~ s(trial_n, k = 5) + season + status + (1 | ID),
                    mu2 ~ s(trial_n, k = 5) + season + status + (1 | ID),
                    # linear effect for theta to stabilize mixture proportion
                    theta1 ~ trial_n + season + status),
                 data = path_integration_data,
                 family = mixture(Beta(link = "logit"), Beta(link = "logit")),
                 prior = mix_priors_back,
                 chains = 4, cores = 4, iter = 8000, warmup = 2000, seed = 1234,
                 control = list(adapt_delta = 0.999, max_treedepth = 20))

post_back <- as_draws_df(pi_final2)
mcmc_trace(post_back, pars = c("b_mu1_Intercept", "b_mu2_Intercept"))
mcmc_pairs(post_back, pars = c("b_mu1_Intercept", "b_mu2_Intercept"))


# #Save model as rds if needed. It will save in the project root.
saveRDS(pi_final2, "path-integration_model.rds")

# Door Memory ----

table_door_status <- table(door_result$door_match, door_result$status)
bf_door_status <- contingencyTableBF(table_door_status, sampleType = "indepMulti", fixedMargin = "rows")
print(bf_door_status)

table_door_food_journey <- table(door_result$door_match, door_result$food_journey)
bf_door_food_journey <- contingencyTableBF(table_door_food_journey, sampleType = "indepMulti", fixedMargin = "rows")
print(bf_door_food_journey)

get_prior(formula = bf(door_match ~ trial_n + season + status + food_journey + (1 | ID)),
          data = door_result, family = bernoulli("logit"))

door_model <- brm(formula = bf(door_match ~ trial_n + season + status + food_journey + (1 | ID)),
                  data = door_result, family = bernoulli("logit"),
                  prior = c(prior("normal(0, 0.5)", class = "b"),
                            prior("normal(0, 1)", class = "Intercept"),
                            prior("exponential(1)", class = "sd", group = "ID")),
                  chains = 4, iter = 4000, cores = 4, seed = 1234,
                  control = list(adapt_delta = 0.95, max_treedepth = 15))

# PLOTS ----

## Data preparation ----

new_season <- visual_assoc_data %>%
  distinct(ID, trial_n, trial, season, status, category)

## Visual Associative Learning plots ----

fitted_vac <- fitted(vac_final2, newdata = new_season) %>%
  data.frame() %>%
  bind_cols(new_season) %>%
  mutate(category = factor(category, levels = category_levels),
         season = factor(season, levels = c("summer", "winter", "spring")),
         status = ifelse(grepl("captive", category), "captive", "wild"))

fitted_avg_vac <- fitted_vac %>%
  group_by(category, trial) %>%
  summarize(AvgEstimate = mean(Estimate), Lower = mean(Q2.5), Upper = mean(Q97.5), .groups = "drop")

ggplot(fitted_vac, aes(x = Estimate, y = category, fill = category)) +
  stat_halfeye(adjust = 1, width = 0.2, .width = 0, justification = 0, point_colour = NA) +
  geom_point(alpha = 0.4, size = 1, position = position_jitter(height = 0.1)) +
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.5, position = position_nudge(y = -0.15)) +
  scale_fill_manual(values = category_colors, labels = category_names) +
  scale_y_discrete(labels = category_names) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(x = "Fitted Estimate", y = "Category") +
  theme_bw() +
  theme(legend.position = "none")

ggplot(fitted_avg_vac, aes(x = as.numeric(trial), y = AvgEstimate, group = category, color = category, fill = category)) +
  geom_point(data = visual_assoc_data, aes(x = as.numeric(as.factor(trial)), y = straightness_to_food, color = category), alpha = 0.2) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, color = NA) +
  facet_wrap(~category, scales = "free_y", ncol = 5, labeller = as_labeller(category_names)) +
  scale_color_manual(values = category_colors, labels = category_names) +
  scale_fill_manual(values = category_colors, labels = category_names) +
  scale_x_continuous(breaks = 1:10, labels = 1:10) +  # keep axis labels looking like trials
  labs(x = "Trial", y = "Average Estimate") +
  theme_bw() +
  theme(legend.position = "none")

### Theta plot — probability of high-efficiency component ----

theta_vac <- fitted(vac_final2, dpar = "theta1",
                    newdata = new_season, re_formula = NA) %>%
  data.frame() %>%
  bind_cols(new_season) %>%
  mutate(Prob_High_Eff = 1 - Estimate,
         Prob_Lower = 1 - Q97.5,
         Prob_Upper = 1 - Q2.5)

ggplot(theta_vac, aes(x = trial, y = Prob_High_Eff, group = category, color = category, fill = category)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = Prob_Lower, ymax = Prob_Upper), alpha = 0.2, color = NA) +
  geom_point() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = category_colors) +
  scale_fill_manual(values = category_colors) +
  labs(x = "Trial Number",
       y = expression(paste("Probability of high-efficiency component (", theta[2], ")"))) +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(~category, scales = "free_y", ncol = 5, labeller = as_labeller(category_names))

## Path Integration plots ----

new_season_pi <- path_integration_data %>%
  distinct(ID, trial_n, trial, season, status, category)

fitted_pi <- fitted(pi_final2, newdata = new_season_pi) %>%
  data.frame() %>%
  bind_cols(new_season_pi) %>%
  mutate(category = factor(category, levels = category_levels),
         season = factor(season, levels = c("summer", "winter", "spring")),
         status = ifelse(grepl("captive", category), "captive", "wild"))

fitted_avg_pi <- fitted_pi %>%
  group_by(category, trial) %>%
  summarize(AvgEstimate = mean(Estimate), Lower = mean(Q2.5), Upper = mean(Q97.5), .groups = "drop")

ggplot(fitted_pi, aes(x = Estimate, y = category, fill = category)) +
  stat_halfeye(adjust = 1, width = 0.2, .width = 0, justification = 0, point_colour = NA) +
  geom_point(alpha = 0.4, size = 1, position = position_jitter(height = 0.1)) +
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.5, position = position_nudge(y = -0.15)) +
  scale_fill_manual(values = category_colors, labels = category_names) +
  scale_y_discrete(labels = category_names) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(x = "Fitted Estimate", y = "Category") +
  theme_bw() +
  theme(legend.position = "none")

ggplot(fitted_avg_pi, aes(x = as.numeric(trial), y = AvgEstimate, color = category, fill = category)) +
  geom_point(data = path_integration_data, aes(x = as.numeric(as.factor(trial)), y = straightness_back, color = category), alpha = 0.2) +
  geom_line(aes(group = category), linewidth = 1.2) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = category, group = category), alpha = 0.2, color = NA) +
  facet_wrap(~category, scales = "free_y", ncol = 5, labeller = as_labeller(category_names)) +
  scale_color_manual(values = category_colors, labels = category_names) +
  scale_fill_manual(values = category_colors, labels = category_names) +
  scale_x_continuous(breaks = 1:10, labels = 1:10) +
  labs(x = "Trial", y = "Average Estimate") +
  theme_bw() +
  theme(legend.position = "none")

### Theta plot — probability of high-efficiency component ----

theta_pi <- fitted(pi_final2, dpar = "theta1",
                   newdata = new_season_pi, re_formula = NA) %>%
  data.frame() %>%
  bind_cols(new_season_pi) %>%
  mutate(Prob_High_Eff = 1 - Estimate,
         Prob_Lower = 1 - Q97.5,
         Prob_Upper = 1 - Q2.5)

ggplot(theta_pi, aes(x = trial_n, y = Prob_High_Eff, color = category, fill = category)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = Prob_Lower, ymax = Prob_Upper), alpha = 0.2, color = NA) +
  geom_point() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = category_colors) +
  scale_fill_manual(values = category_colors) +
  scale_x_continuous(breaks = 1:10, labels = 1:10) +
  labs(x = "Trial Number",
       y = expression(paste("Probability of High-Efficiency Component (", theta[2], ")"))) +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(~category, scales = "free_y", ncol = 5, labeller = as_labeller(category_names))
