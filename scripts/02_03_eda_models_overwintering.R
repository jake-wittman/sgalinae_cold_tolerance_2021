
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lme4)
library(DHARMa)
library(merTools)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")
# Read in data ------------------------------------------------------------

dat <- read_csv(here::here("data/cleaned_sgalinae_emergence_peeling.csv"))
hobo_dat <- read_csv(here::here("data/hobo_data/hobo_summary.csv"))

# Summaries ---------------------------------------------------------------
# Create a variable for the number of total adults
dat <- dat %>% 
   separate(treatment,
            into = c("site", "treatment"),
            sep = "_")  %>% 
   mutate(total_adults = number_emg_sg + unemerged_adults,
          total_insects = total_adults + unemerged_larvae,
          log_uid = paste(site, treatment, log_id_number, sep = "_"),
          brood_uid = paste(log_uid, sg_brood_id, sep = "_")) %>% 
   mutate(total_insects = case_when(total_insects == 0 ~ NA_real_,
                                    TRUE ~ total_insects),
          treatment = case_when(treatment == "Outdoor" ~ "Control",
                                TRUE ~ treatment),
          prop_success = total_adults / (unemerged_larvae + total_adults),
          prop_emerge = number_emg_sg / total_insects)
# Make hobo data site information match format in dat
hobo_dat <- hobo_dat %>% 
   filter(!str_detect(location, "incubator")) %>% 
   separate(location,
            into = c("site", "treatment"),
            sep = "_") %>% 
   mutate(treatment = case_when(treatment == "Air" & site == "DE" ~ "Control",
                                TRUE ~ treatment))

dat <- left_join(dat, hobo_dat, by = c("site", "treatment"))
# Log level summary
dat %>% 
   group_by(treatment, log_id_number) %>% 
   select(-contains("temp")) %>% 
   summarise(across(where(is.numeric), sum)) 

ggplot(dat, aes(x = min_temp, y = prop_success)) +
   geom_jitter() +
   scale_x_reverse()

# How did the shipping control do?
dat %>% 
   group_by(site) %>% 
   summarise(prop_survive = sum(total_adults, na.rm = TRUE) / sum(total_insects, na.rm = TRUE),
             prop_emerged = sum(number_emg_sg, na.rm = TRUE) / sum(total_insects, na.rm = TRUE))

# Add Abbott Correction proportions
dat <- dat %>% 
   group_by(site, treatment, min_temp) %>% 
   summarise(prop_eclose = sum(total_adults, na.rm = TRUE) / sum(total_insects, na.rm = TRUE),
             prop_emerged = sum(number_emg_sg, na.rm = TRUE) / sum(total_insects, na.rm = TRUE),
             n = sum(total_insects, na.rm = TRUE)) %>% 
   mutate(control_eclose = 0.977,
          control_emerged = 0.718) %>% 
   filter(site != "Shipping") %>% 
   mutate(abbott_eclose = 1 - (control_eclose - prop_eclose) / control_eclose,
          abbott_emerge = 1 - (control_emerged - prop_emerged) / control_emerged) %>% 
   mutate(abbott_eclose = case_when(abbott_eclose > 1 ~ 1, 
                                       abbott_eclose < 0 ~ 0,
                                       TRUE ~ abbott_eclose),
             abbot_emerge = case_when(abbott_emerge > 1 ~ 1,
                                      abbott_emerge < 0 ~ 0,
                                      TRUE ~ abbott_emerge))

dat %>% 
   pivot_longer(cols = c(prop_eclose, prop_emerged, abbott_eclose, abbott_emerge),
                names_to = "prop_type",
                values_to = "proportion") %>% 
   ggplot(aes(x = min_temp, y = proportion, weight = n)) +
   geom_point() +
   stat_smooth(method = "glm", method.args = list(family = "binomial")) +
   facet_wrap(~ prop_type)

# Models ------------------------------------------------------------------
dat_narm <- dat[complete.cases(dat[, c("min_temp", "total_insects")]), ]

# Testing different random effect structures
glm1 <- glmer(cbind(total_adults, unemerged_larvae) ~ min_temp + (1 |  brood_uid),
              data = dat_narm,
              family = binomial)
summary(glm1)
resids_dharma <- simulateResiduals(fittedModel = glm1, n = 1000)
plot(resids_dharma, asFactor = TRUE)
plotResiduals(resids_dharma, dat_narm$min_temp, quantreg = TRUE)
res2 <- recalculateResiduals(resids_dharma, group = dat_narm$brood_uid)
plot(res2)

glm2 <- glmer(cbind(total_adults, unemerged_larvae) ~ min_temp + (1 | log_uid / brood_uid),
              data = dat_narm,
              family = binomial)
summary(glm2)

glm3 <- glmer(cbind(total_adults, unemerged_larvae) ~ min_temp + (1 | site / log_uid / brood_uid),
              data = dat_narm,
              family = binomial)
summary(glm3)

glm4 <- glmer(cbind(total_adults, unemerged_larvae) ~ min_temp + (1 | site / brood_uid),
              data = dat_narm,
              family = binomial)
summary(glm4)

anova(glm1, glm2, glm3, glm4)

predictions <- predict(glm3,
                       newdata = data.frame(min_temp = seq(-5, -30, length.out = 100),
                                            mean_RH = rep(85, 100)),
                       re.form = NA,
                       type = "response")
predictions <- as.data.frame(predictions)
predictions$x <- seq(-5, -30, length.out = 100)

pred_interval <- predictInterval(merMod = glm3,
                                 newdata = data.frame(min_temp = seq(-5, -30, length.out = 100),
                                                      brood_uid = rep(0, length.out = 100),
                                                      log_uid = rep(0, length.out = 100),
                                                      site = rep(0, length.out = 100)),
                                 level = 0.95, 
                                 n.sims = 10000,
                                 stat = "median",
                                 type = "probability",
                                 include.resid.var = TRUE) %>% 
   mutate(min_temp = seq(-5, -30, length.out = 100))

ggplot() +
   geom_line(data = predictions, aes(x = x, y = predictions)) +
   geom_ribbon(data = pred_interval, aes(x = min_temp, ymin = lwr, ymax = upr), alpha = 0.1) +
   geom_jitter(data = dat_narm, aes(x = min_temp, y = prop_success)) +
   scale_x_reverse() +
   labs(x = "Temperature (C)", y = "Proportion S. galinae maturing to adult \n") +
   theme(axis.title = element_text(size = 20),
         axis.text = element_text(size = 14))

# Get fitted and observed
overwinter_model <- as.data.frame(predict(glm3,
                                          re.form = NA,
                                          type = "response"))
names(overwinter_model) <- "fitted"
overwinter_model$observed <- dat_narm$prop_success 
overwinter_model %>% 
   mutate(diff_sq = (observed - fitted)^2) %>% 
   summarize(MSE = mean(diff_sq, na.rm = TRUE))

# Model comparing treatments ----------------------------------------------
glm5 <- glmer(cbind(total_adults, unemerged_larvae) ~ treatment + (1 | site / log_uid / brood_uid),
              data = dat_narm,
              family = binomial)

summary(glm5)
anova(glm5, glm3)


# Models for RH -----------------------------------------------------------
glm6 <- glmer(cbind(total_adults, unemerged_larvae) ~ mean_RH + (1 | site / log_uid / brood_uid),
              data = dat_narm,
              family = binomial)
summary(glm6)

glm6.5 <- glmer(cbind(total_adults, unemerged_larvae) ~ mean_RH + min_temp + (1 | site / log_uid / brood_uid),
              data = dat_narm,
              family = binomial)
summary(glm6.5)
# Interaction for RH and Temp? --------------------------------------------

glm7 <- glmer(cbind(total_adults, unemerged_larvae) ~ mean_RH + min_temp + (1 | site / log_uid / brood_uid),
              data = dat_narm,
              family = binomial)
summary(glm7)

# 


dat_narm$scaled_mean_RH <- scale(dat_narm$mean_RH)
dat_narm$scaled_min_temp <- scale(dat_narm$min_temp)

# Scaling solves the issue
glm8 <- glmer(cbind(total_adults, unemerged_larvae) ~ scaled_mean_RH * scaled_min_temp + 
                 (1 | site / log_uid / brood_uid),
              data = dat_narm,
              family = binomial)
summary(glm8)

# Hmmmmm model unidentifiable - not with brms!
# bglm7 <- brm(bf(total_adults | trials(total_insects) ~
#                       mean_RH * min_temp + (1 | site / log_uid / brood_uid),
#                 decomp = "QR"),
#                    data = dat_narm,
#                    family = binomial,
#                    backend = "cmdstanr",
#                    chains = 3,
#                    iter = 5000,
#                    cores = 3,
#                    control = list(max_treedepth = 15,
#                                   adapt_delta = 0.9))
# Predictions
predictions <- predict(glm6,
                       newdata = data.frame(min_temp = seq(-5, -30, length.out = 100),
                                            mean_RH = rep(85, 100)),
                       re.form = NA,
                       type = "response")
predictions <- as.data.frame(predictions)
predictions$x <- seq(-5, -30, length.out = 100)

pred_interval <- predictInterval(merMod = glm7,
                                 newdata = data.frame(min_temp = seq(-5, -30, length.out = 100),
                                                      mean_RH = rep(85, 100),
                                                      brood_uid = rep(0, length.out = 100),
                                                      log_uid = rep(0, length.out = 100),
                                                      site = rep(0, length.out = 100)),
                                 level = 0.95, 
                                 n.sims = 10000,
                                 stat = "median",
                                 type = "probability",
                                 include.resid.var = TRUE) %>% 
   mutate(min_temp = seq(-5, -30, length.out = 100))

ggplot() +
   geom_line(data = predictions, aes(x = x, y = predictions)) +
   geom_ribbon(data = pred_interval, aes(x = min_temp, ymin = lwr, ymax = upr), alpha = 0.1) +
   geom_jitter(data = dat_narm, aes(x = min_temp, y = prop_success)) +
   scale_x_reverse() +
   labs(x = "Temperature (C)", y = "Proportion S. galinae maturing to adult \n") +
   theme(axis.title = element_text(size = 20),
         axis.text = element_text(size = 14))


# Average minimum temp and variation --------------------------------------
# Not significanta, singular fit unless I remove log_uid. Then significant
# Comparing this to glm4 (which has the same RE structure), this model fits worse
avgmintemp_glm <- glmer(cbind(total_adults, unemerged_larvae) ~ avg_min_temp + (1 | site / brood_uid),
                        data = dat_narm,
                        family = binomial)

var_mintemp_glm <- glmer(cbind(total_adults, unemerged_larvae) ~ var_min_temp + (1 | site / log_uid / brood_uid),
                        data = dat_narm,
                        family = binomial)
# Singular fit, not significant. Altering RE structure doesn't seem to help, unless I pare it down to just site
avg_avg_RH_glm <- glmer(cbind(total_adults, unemerged_larvae) ~ avg_avg_RH + (1 | site),
                                       data = dat_narm,
                                       family = binomial)
# Not sig. singular fit unless I remove log_uid. Then significant.
var_RH_glm <- glmer(cbind(total_adults, unemerged_larvae) ~ var_RH + (1 | site /  brood_uid),
                        data = dat_narm,
                        family = binomial)


# Compare eclose vs emerge models -----------------------------------------




# Talk to Brian about which to fit. My gut is the full model since it's the way the experiment was designed
# Predictions from SCP models ---------------------------------------------
field_dat <- pmap_dfr(dat_narm,
                      function(min_temp,
                               mean_RH,
                               treatment,
                               total_adults,
                               total_insects,
                               site,
                               log_uid,
                               brood_uid,
                               sd_temp,
                               mean_temp,
                               prop_success,
                               ...) {
                         data.frame(
                            site = site,
                            treatment = treatment,
                            log_uid = log_uid,
                            brood_uid = brood_uid,
                            sd_temp = sd_temp,
                            mean_temp = mean_temp,
                            mean_RH = mean_RH,
                            min_temp = min_temp,
                            prop_success = prop_success,
                            emerged = c(rep(1, total_adults),
                                        rep(0, (
                                           total_insects - total_adults
                                        )))
                         )
                      }) %>% 
   mutate(discolor_3day = emerged,
          temp_pulled_grouped = min_temp,
          trial_id = brood_uid,
          eclose = emerged)

source(here::here("scripts/01_04_scp_models.R"))

discolor_predictions <- predict(field_model_discolor,
        newdata = field_dat,
        allow.new.levels = TRUE,
        type = "response")

eclose_predictions <- predict(field_model_eclose,
                                newdata = field_dat,
                                allow.new.levels = TRUE,
                                type = "response")

field_dat <- bind_cols(field_dat,
                 discolor_predictions = discolor_predictions,
                 eclose_predictions = eclose_predictions)

model_predictions <- field_dat %>%
   group_by(brood_uid) %>%
   slice_sample(n = 1) %>% 
   mutate(discolor_difference = discolor_predictions - prop_success,
          eclose_difference = eclose_predictions - prop_success)

pred_interval_discolor <- ggpredict(field_model_discolor, "temp_pulled_grouped")

pred_interval_eclose <- ggpredict(field_model_eclose, "temp_pulled_grouped")


ggplot(model_predictions, aes(x = min_temp)) +
   geom_point(aes(y = prop_success), color = "black") +
   geom_line(data = pred_interval, aes(x = min_temp, y = fit)) +
   geom_ribbon(data = pred_interval, aes(ymin = lwr, ymax = upr), alpha = 0.075) +
   geom_line(data = pred_interval_discolor,
             aes(x = x, y = predicted),
             color = "green") +
   geom_ribbon(data = pred_interval_discolor,
               aes(x = x, ymin = conf.low, ymax = conf.high),
               color = "green",
               fill = "green",
               alpha = 0.075) +
   geom_line(data = pred_interval_eclose,
             aes(x = x, y = predicted),
             color = "blue") +
   geom_ribbon(data = pred_interval_eclose,
               aes(x = x, ymin = conf.low, ymax = conf.high),
               color = "blue",
               fill = "blue",
               alpha = 0.075) +
   scale_x_reverse(limits = c(-8, -30.5)) +
   theme(axis.text = element_text(size = 14),
         axis.title = element_text(size = 20)) +
   labs(y = "Proportion emerging",
        x = "Minimum temperature (C)")



model_predictions %>%
   ungroup() %>% 
   summarise(MSE_discolor = mean(discolor_difference^2, na.rm = TRUE),
             MSE_diff_eclose = mean(eclose_difference^2, na.rm = TRUE))

ggplot(model_predictions, aes(x = min_temp)) +
   geom_point(aes(y = discolor_difference), color = "black") +
   geom_point(aes(y = eclose_difference), color = "blue") +
   scale_x_reverse()

