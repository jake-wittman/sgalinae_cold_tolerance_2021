# Libraries and other setup ----------------------------------------------------

if (!require(pacman)) install.packages("pacman")
if (!require(rio)) install.packages("rio")
library(DescTools)
pacman::p_load(tidyverse)
pacman::p_load(lubridate)
pacman::p_load(here)
pacman::p_load(survminer)
pacman::p_load(survival)
pacman::p_load(doBy)
pacman::p_load(lme4)
pacman::p_load(ggeffects)
pacman::p_load(lmerTest)
pacman::p_load(geepack)
pacman::p_load(rsample)


set.seed(1234)

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lmer", "lmerTest")
# Read in data
dat <- read_csv(here::here("data/cleaned_scp_data.csv")) %>% 
   mutate(degree_min = as.factor(degree_min),
          trial_id = as.factor(trial_id))

# Models ------------------------------------------------------------------

# Cooling rate as a continuous variable

# Modeling individual survival instead of proportion in batch

glmm_fm1 <- glmer(discolor_3day ~ temp_pulled_grouped * degree_min_continuous + (1 | trial_id),
           data = dat,
           family = binomial,
           nAGQ = 50)
summary(glmm_fm1)

pred_dat_glmm_1 <- ggpredict(glmm_fm1, c("temp_pulled_grouped", "degree_min_continuous"))

set.seed(1)
dat %>% 
   ggplot(aes(x = temp_pulled_grouped,
              y = eclose,
              colour = degree_min)) +
   geom_jitter(aes(fill = degree_min), 
               width = 0.09, height = 0.02, pch = 21, colour = "black") +
   geom_line(data = pred_dat_glmm_1,
             aes(x = x,
                 y = predicted,
                 colour = group),
             size = 1.05) +
   geom_ribbon(data = pred_dat_glmm_1,
               aes(x = x,
                   ymin = conf.low,
                   ymax = conf.high,
                   fill = group),
               inherit.aes = FALSE,
               alpha = 0.15) +
   scale_color_viridis_d(aesthetics = c("colour", "fill"),
                         option = "viridis") +
   labs(x = "Low temperature reached (C)",
        y = "Proportion of insects discolored",
        colour = "Chill Rate (C/min)",
        fill = "Chill Rate (C/min)",
        title = "GLMM predictions and CI") +
   scale_x_reverse() +
   coord_cartesian(ylim = c(0, 1))




# Model diagnostics -------------------------------------------------------
par(mfrow = c(2, 2))
plot(glmm_fm1, which = c(1, 4, 5, 6))

# Effect of removing each point on the beta values in the model
# dfbetas(glmm_fm1) %>% 
#    as.data.frame(.) %>% 
#    gather(., variable, beta_effect, temp_pulled:degree_min1) %>% 
#    ggplot() +
#    geom_boxplot(aes(x = variable, y = beta_effect)) +
#    geom_point(aes(x = variable, y = beta_effect)) 

# Doesn't look like either model has much in the way of bad diagnostics

pred_dat <- pred_dat_glmm_1

# Plot selected model -----------------------------------------------------
discolor_model_plot <- dat %>% 
   ggplot(aes(x = temp_pulled_grouped, y = discolor_3day, colour = degree_min)) +
   geom_jitter(width = 0.07, height = 0.01) +
   geom_point(colour = "black", shape = 1, size = 1.5) +
   geom_line(data = pred_dat,
             aes(x = temp_pulled_grouped,
                 y = fitted),
             size = 1.05) +
   geom_ribbon(data = pred_dat,
               aes(x = temp_pulled_grouped,
                   ymin = low_ci,
                   ymax = high_ci,
                   fill = degree_min),
               inherit.aes = FALSE,
               alpha = 0.15) +
   scale_color_viridis_d(aesthetics = c("colour", "fill"),
                         option = "viridis") +
   labs(x = "Low temperature reached (C)",
        y = "Proportion of insects discolored",
        colour = "Chill Rate (C/min)",
        fill = "Chill Rate (C/min)") +
   scale_x_reverse()



# Model eclosion ----------------------------------------------------------

glmm_fm2 <- glmer(eclose ~ temp_pulled_grouped * degree_min_continuous + (1 | trial_id),
                  data = dat,
                  family = binomial,
                  nAGQ = 50)
summary(glmm_fm2)

predict(glmm_fm2,
        newdata = data.frame(
           temp_pulled_grouped = seq(0, -32, -2),
           degree_min_continuous = 1
        ),
        re.form = NA)

pred_dat_glmm_2 <- ggpredict(glmm_fm2, c("temp_pulled_grouped", "degree_min_continuous"))# %>% 

set.seed(1)
dat %>% 
   ggplot(aes(x = temp_pulled_grouped,
              y = eclose,
              colour = degree_min)) +
   geom_jitter(aes(fill = degree_min), 
               width = 0.09, height = 0.02, pch = 21, colour = "black") +
   geom_line(data = pred_dat_glmm_2,
             aes(x = x,
                 y = predicted,
                 colour = group),
             size = 1.05) +
   geom_ribbon(data = pred_dat_glmm_2,
               aes(x = x,
                   ymin = conf.low,
                   ymax = conf.high,
                   fill = group),
               inherit.aes = FALSE,
               alpha = 0.15) +
   scale_color_viridis_d(aesthetics = c("colour", "fill"),
                         option = "viridis") +
   labs(x = "Low temperature reached (C)",
        y = "Proportion of insects eclosing",
        colour = "Chill Rate (C/min)",
        fill = "Chill Rate (C/min)",
        title = "GLMM predictions and CI") +
   scale_x_reverse() +
   coord_cartesian(ylim = c(0, 1))





# Including discoloration in eclosion model -------------------------------
glmm_fm3 <- glmer(eclose ~ temp_pulled_grouped * degree_min_continuous + discolor_3day + (1 | trial_id),
                  data = dat,
                  family = binomial,
                  nAGQ = 50)
summary(glmm_fm3)

pred_dat_glmm_3 <- ggpredict(glmm_fm3, c("temp_pulled_grouped", "degree_min_continuous"))# %>% 

set.seed(1)
dat %>% 
   ggplot(aes(x = temp_pulled_grouped,
              y = eclose,
              colour = degree_min)) +
   geom_jitter(aes(fill = degree_min), 
               width = 0.09, height = 0.02, pch = 21, colour = "black") +
   geom_line(data = pred_dat_glmm_3,
             aes(x = x,
                 y = predicted,
                 colour = group),
             size = 1.05) +
   geom_ribbon(data = pred_dat_glmm_3,
               aes(x = x,
                   ymin = conf.low,
                   ymax = conf.high,
                   fill = group),
               inherit.aes = FALSE,
               alpha = 0.15) +
   scale_color_viridis_d(aesthetics = c("colour", "fill"),
                         option = "viridis") +
   labs(x = "Low temperature reached (C)",
        y = "Proportion of insects eclosing",
        colour = "Chill Rate (C/min)",
        fill = "Chill Rate (C/min)",
        title = "GLMM predictions and CI") +
   scale_x_reverse() +
   coord_cartesian(ylim = c(0, 1))


# Removing the interaction ------------------------------------------------

discolor_no_intrxn <- glmer(discolor_3day ~ temp_pulled_grouped + degree_min_continuous + (1 | trial_id),
                  data = dat,
                  family = binomial,
                  nAGQ = 50)
summary(discolor_no_intrxn)

eclose_no_intrxn <- glmer(eclose ~ temp_pulled_grouped + degree_min_continuous + (1 | trial_id),
                            data = dat,
                            family = binomial,
                            nAGQ = 50)
summary(eclose_no_intrxn)


# The final model used for map predictions (or set of final models)
map_model <- glmm_fm1

discolor_model <- glmm_fm1
eclosion_model <- glmm_fm2

# Fit a model that just uses lower temperature for use with my field data
field_model_discolor <- glmer(discolor_3day ~ temp_pulled_grouped  + (1 | trial_id),
                     data = dat,
                     family = binomial,
                     nAGQ = 50)
field_model_eclose <- glmer(eclose ~ temp_pulled_grouped  + (1 | trial_id),
                            data = dat,
                            family = binomial,
                            nAGQ = 50)

# Compare SCP -------------------------------------------------------------

scp_fm <- lmer(est_scp ~ degree_min  +(1|trial_id), data = dat)
summary(scp_fm)
anova(scp_fm)

difflsmeans(scp_fm, test.effs = "Group")
emmeans::emmeans(scp_fm, list(pairwise ~ degree_min), adjust = "tukey")

# Presentation plots ------------------------------------------------------
set.seed(1)
discoloration_plot <- dat %>% 
   ggplot(aes(x = temp_pulled_grouped,
              y = discolor_3day,
              colour = degree_min)) +
   geom_jitter(aes(fill = degree_min), 
               width = 0.09, height = 0.02, pch = 21, colour = "black") +
   geom_line(data = pred_dat_glmm_1,
             aes(x = x,
                 y = predicted,
                 colour = group),
             size = 1.05) +
   geom_ribbon(data = pred_dat_glmm_1,
               aes(x = x,
                   ymin = conf.low,
                   ymax = conf.high,
                   fill = group),
               inherit.aes = FALSE,
               alpha = 0.15) +
   geom_vline(xintercept = -24.5) +
   scale_color_viridis_d(aesthetics = c("colour", "fill"),
                         option = "viridis") +
   labs(x = "Low temperature reached (°C)",
        y = "Proportion of larvae surviving",
        colour = "Chill Rate (°C/min)",
        fill = "Chill Rate (°C/min)") +
   scale_x_reverse() +
   coord_cartesian(ylim = c(0, 1)) +
   theme(axis.title = element_text(size = 26, colour = "black"),
         axis.text = element_text(size = 23, colour = "black"),
         legend.title = element_text(size = 26),
         legend.text = element_text(size = 23))
# ggsave(
#    here::here("figures/discoloration_model.tiff"),
#    discoloration_plot,
#    height = 7,
#    width = 12,
#    units = "in",
#    dpi = 300
# )

adult_emergence_plot <- dat %>%
   ggplot(aes(x = temp_pulled_grouped,
              y = eclose,
              colour = degree_min)) +
   geom_jitter(
      aes(fill = degree_min),
      width = 0.09,
      height = 0.02,
      pch = 21,
      colour = "black"
   ) +
   geom_line(data = pred_dat_glmm_2,
             aes(x = x,
                 y = predicted,
                 colour = group),
             size = 1.05) +
   geom_ribbon(
      data = pred_dat_glmm_2,
      aes(
         x = x,
         ymin = conf.low,
         ymax = conf.high,
         fill = group
      ),
      inherit.aes = FALSE,
      alpha = 0.15
   ) +
   geom_vline(xintercept = -24.5) +
   scale_color_viridis_d(aesthetics = c("colour", "fill"),
                         option = "viridis") +
   labs(
      x = "Low temperature reached (°C)",
      y = "Proportion of adults emerging",
      colour = "Chill Rate (°C/min)",
      fill = "Chill Rate (°C/min)"
   ) +
   scale_x_reverse() +
   coord_cartesian(ylim = c(0, 1)) +
   theme(
      axis.title = element_text(size = 26, colour = "black"),
      axis.text = element_text(size = 23, colour = "black"),
      legend.title = element_text(size = 26),
      legend.text = element_text(size = 23)
   )

# ggsave(
#    here::here("figures/adult_emergence_model.tiff"),
#    adult_emergence_plot,
#    height = 7,
#    width = 12,
#    units = "in",
#    dpi = 300
# )
