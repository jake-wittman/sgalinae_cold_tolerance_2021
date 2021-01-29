# Var specifies lower, upper, or fitted value for creating map
maps_fun <- function(df, var) {
   df %>% 
      filter(pred_type == !!var) %>% 
      ggplot() +
      geom_tile(aes(x = x, y = y, fill = predictions)) +
      theme_nothing(legend = TRUE) +
      coord_fixed(ratio = 1.3) +
      scale_fill_viridis(
         direction = -1,
         limits = c(0.0, 1.0),
         name = expression(atop(
            paste("Percent of ", italic("S. galinae "), "eclosing")
         ))
      ) +
      geom_sf(
         data = na_sf,
         fill = NA,
         color = "black"
      ) +
      theme(
         legend.title.align = 0,
         axis.line = element_blank(),
         legend.position = "bottom",
         panel.border = element_blank(),
         plot.margin = unit(rep(0.1, 4), "mm")
      )
}
endCluster()

low_mild_map <- maps_fun(win_mild_long, "lwr") 
fit_mild_map <- maps_fun(win_mild_long, "fit")
upper_mild_map <- maps_fun(win_mild_long, "upr")

low_severe_map <- maps_fun(win_severe_long, "lwr") 
fit_severe_map <- maps_fun(win_severe_long, "fit")
upper_severe_map <- maps_fun(win_severe_long, "upr")


# Old ms plots of survival
low_mild_map + 
   fit_mild_map + 
   upper_mild_map +
   low_severe_map +
   fit_severe_map +
   upper_severe_map +
   plot_layout(nrow = 3,
               ncol = 2,
               byrow = FALSE,
               guides = "collect") +
   plot_annotation(tag_levels = "A") & theme(legend.position = "bottom")

# Fit the model as a GEE
# Unstructured correlation structure gives absurdly high parameter estimates.
unstr_gee2 <- dat %>% 
   filter(!is.na(temp_pulled_grouped)) %>% 
   geeglm(eclose ~ temp_pulled_grouped * degree_min_continuous,
          data = .,
          family = binomial(link = "logit"),
          id = trial_id,
          corstr = "unstructured")
summary(unstr_gee2)

# Exchangeable results seem a bit better
exch_gee2 <- dat %>% 
   filter(!is.na(temp_pulled_grouped)) %>% 
   geeglm(eclose ~ temp_pulled_grouped * degree_min_continuous,
          data = .,
          family = binomial(link = "logit"),
          id = trial_id,
          corstr = "exchangeable")
summary(exch_gee2)

pred_prob2 <- predict(exch_gee2,
                      newdata = dat[,c("temp_pulled_grouped",
                                       "degree_min_continuous",
                                       "trial_id")],
                      type = "link")
sub_dat2 <- dat[, c("temp_pulled_grouped",
                    "degree_min_continuous",
                    "trial_id",
                    "eclose")] %>% 
   filter(!is.na(temp_pulled_grouped))

boots2 <- bootstraps(sub_dat2, times = 10000)
fit_gee_bootstrap2 <- function(dat) {
   geeglm(eclose ~ temp_pulled_grouped * degree_min_continuous,
          data = analysis(dat),
          family = binomial(link = "logit"),
          id = trial_id,
          corstr = "exchangeable")
}

boot_preds2 <- boots2 %>% 
   mutate(predictions = map(splits, function(.x) {
      temp_mod <- fit_gee_bootstrap2(.x)
      pred_prob2 <- predict(temp_mod,
                            newdata = assessment(.x))
      df <- data.frame(assessment(.x))
      df <- cbind(df, pred_prob2)
      return(df)
   }))

boot_dat2 <- bind_rows(boot_preds2$predictions)
boot_dat2 <- bind_rows(boot_preds2$predictions) %>% 
   group_by(degree_min_continuous, temp_pulled_grouped) %>% 
   mutate(pred_prob2 = plogis(pred_prob2)) %>% 
   summarise(min_ci = quantile(pred_prob2, 0.05),
             max_ci = quantile(pred_prob2, 0.95),
             mean = mean(pred_prob2, na.rm = T))

boot_dat2 <- left_join(x = dat, y = boot_dat2, by = c("degree_min_continuous",
                                                      "temp_pulled_grouped"))

set.seed(1)
boot_dat2 %>%
   mutate(degree_min_cat = as.factor(degree_min_continuous)) %>% 
   ggplot(aes(x = temp_pulled_grouped,
              y = mean,
              group = degree_min)) +
   geom_jitter(aes(x = temp_pulled_grouped,
                   y = eclose,
                   fill = degree_min),
               width = 0.09, height = 0.02, pch = 21, colour = "black") +
   geom_smooth(aes(x = temp_pulled_grouped,
                   y = mean,
                   colour = degree_min),
               size = 1.05,
               se = F) +
   geom_ribbon(aes(x = temp_pulled_grouped,
                   ymin = min_ci,
                   ymax = max_ci,
                   fill = degree_min,
                   group = degree_min),
               inherit.aes = FALSE,
               alpha = 0.15) +
   labs(x = "Low temperature reached (C)",
        y = "Proportion of insects eclosing",
        colour = "Chill Rate (C/min)",
        fill = "Chill Rate (C/min)",
        title = "GEE model predictions and bootstrap CI") +
   scale_color_viridis_d(aesthetics = c("colour", "fill"),
                         option = "viridis") +
   scale_x_reverse() +
   coord_cartesian(ylim = c(0, 1))

# Fit the model as a GEE
# Unstructured correlation structure gives absurdly high parameter estimates.
unstr_gee3 <- dat %>% 
   filter(!is.na(temp_pulled_grouped) &
             !is.na(discolor_3day)) %>% 
   geeglm(eclose ~ temp_pulled_grouped * degree_min_continuous + discolor_3day,
          data = .,
          family = binomial(link = "logit"),
          id = trial_id,
          corstr = "unstructured")
summary(unstr_gee3)

# Exchangeable results seem a bit better
exch_gee3 <- dat %>% 
   filter(!is.na(temp_pulled_grouped) &
             !is.na(discolor_3day)) %>% 
   geeglm(eclose ~ temp_pulled_grouped * degree_min_continuous + discolor_3day,
          data = .,
          family = binomial(link = "logit"),
          id = trial_id,
          corstr = "exchangeable")
summary(exch_gee3)

pred_prob3 <- predict(exch_gee3,
                      newdata = dat[,c("temp_pulled_grouped",
                                       "degree_min_continuous",
                                       "trial_id",
                                       "discolor_3day")],
                      type = "link")
sub_dat3 <- dat[, c("temp_pulled_grouped",
                    "degree_min_continuous",
                    "trial_id",
                    "discolor_3day",
                    "eclose")] %>% 
   filter(!is.na(temp_pulled_grouped) &
             !is.na(discolor_3day))

boots3 <- bootstraps(sub_dat3, times = 10000)
fit_gee_bootstrap3 <- function(dat) {
   geeglm(eclose ~ temp_pulled_grouped * degree_min_continuous + discolor_3day,
          data = analysis(dat),
          family = binomial(link = "logit"),
          id = trial_id,
          corstr = "exchangeable")
}

boot_preds3 <- boots3 %>% 
   mutate(predictions = map(splits, function(.x) {
      temp_mod <- fit_gee_bootstrap3(.x)
      pred_prob3 <- predict(temp_mod,
                            newdata = assessment(.x))
      df <- data.frame(assessment(.x))
      df <- cbind(df, pred_prob3)
      return(df)
   }))

boot_dat3 <- bind_rows(boot_preds3$predictions)
boot_dat3 <- bind_rows(boot_preds3$predictions) %>% 
   group_by(degree_min_continuous, temp_pulled_grouped) %>% 
   mutate(pred_prob3 = plogis(pred_prob3)) %>% 
   summarise(min_ci = quantile(pred_prob3, 0.05),
             max_ci = quantile(pred_prob3, 0.95),
             mean = mean(pred_prob3, na.rm = T))

boot_dat3 <- left_join(x = dat, y = boot_dat3, by = c("degree_min_continuous",
                                                      "temp_pulled_grouped"))

set.seed(1)
boot_dat3 %>%
   ggplot(aes(x = temp_pulled_grouped,
              y = mean,
              group = degree_min)) +
   geom_jitter(aes(x = temp_pulled_grouped,
                   y = eclose,
                   fill = degree_min),
               width = 0.09, height = 0.02, pch = 21, colour = "black") +
   geom_smooth(aes(x = temp_pulled_grouped,
                   y = mean,
                   colour = degree_min),
               size = 1.05,
               se = F) +
   geom_ribbon(aes(x = temp_pulled_grouped,
                   ymin = min_ci,
                   ymax = max_ci,
                   fill = degree_min,
                   group = degree_min),
               inherit.aes = FALSE,
               alpha = 0.15) +
   labs(x = "Low temperature reached (C)",
        y = "Proportion of insects eclosing",
        colour = "Chill Rate (C/min)",
        fill = "Chill Rate (C/min)",
        title = "GEE model predictions and bootstrap CI") +
   scale_color_viridis_d(aesthetics = c("colour", "fill"),
                         option = "viridis") +
   scale_x_reverse() +
   coord_cartesian(ylim = c(0, 1))


# Fit the model as a GEE
# Unstructured correlation structure gives absurdly high parameter estimates.
unstr_gee <- dat %>% 
   filter(!is.na(temp_pulled_grouped)) %>% 
   geeglm(discolor_3day ~ temp_pulled_grouped * degree_min_continuous,
          data = .,
          family = binomial(link = "logit"),
          id = trial_id,
          corstr = "unstructured")
summary(unstr_gee)

# Exchangeable results seem a bit better
exch_gee <- dat %>% 
   filter(!is.na(temp_pulled_grouped)) %>% 
   geeglm(discolor_3day ~ temp_pulled_grouped * degree_min_continuous,
          data = .,
          family = binomial(link = "logit"),
          id = trial_id,
          corstr = "exchangeable")
summary(exch_gee)

pred_prob <- predict(exch_gee,
                     newdata = dat[,c("temp_pulled_grouped",
                                      "degree_min_continuous",
                                      "trial_id")],
                     type = "link")
sub_dat <- dat[, c("temp_pulled_grouped",
                   "degree_min_continuous",
                   "trial_id",
                   "discolor_3day")] %>% 
   filter(!is.na(temp_pulled_grouped))

boots <- bootstraps(sub_dat, times = 10000)
fit_gee_bootstrap <- function(dat) {
   geeglm(discolor_3day ~ temp_pulled_grouped * degree_min_continuous,
          data = analysis(dat),
          family = binomial(link = "logit"),
          id = trial_id,
          corstr = "exchangeable")
}

boot_preds <- boots %>% 
   mutate(predictions = map(splits, function(.x) {
      temp_mod <- fit_gee_bootstrap(.x)
      pred_prob <- predict(temp_mod,
                           newdata = assessment(.x))
      df <- data.frame(assessment(.x))
      df <- cbind(df, pred_prob)
      return(df)
   }))

boot_dat <- bind_rows(boot_preds$predictions)
boot_dat <- bind_rows(boot_preds$predictions) %>% 
   group_by(degree_min_continuous, temp_pulled_grouped) %>% 
   mutate(pred_prob = plogis(pred_prob)) %>% 
   summarise(min_ci = quantile(pred_prob, 0.05),
             max_ci = quantile(pred_prob, 0.95),
             mean = mean(pred_prob, na.rm = T))

boot_dat <- left_join(x = dat, y = boot_dat, by = c("degree_min_continuous",
                                                    "temp_pulled_grouped"))

set.seed(1)
boot_dat %>%
   mutate(degree_min_cat = as.factor(degree_min_continuous)) %>% 
   ggplot(aes(x = temp_pulled_grouped,
              y = mean,
              group = degree_min)) +
   geom_jitter(aes(x = temp_pulled_grouped,
                   y = discolor_3day,
                   fill = degree_min),
               width = 0.09, height = 0.02, pch = 21, colour = "black") +
   geom_smooth(aes(x = temp_pulled_grouped,
                   y = mean,
                   colour = degree_min),
               size = 1.05,
               se = F) +
   geom_ribbon(aes(x = temp_pulled_grouped,
                   ymin = min_ci,
                   ymax = max_ci,
                   fill = degree_min,
                   group = degree_min),
               inherit.aes = FALSE,
               alpha = 0.15) +
   labs(x = "Low temperature reached (C)",
        y = "Proportion of insects not discolored",
        colour = "Chill Rate (C/min)",
        fill = "Chill Rate (C/min)",
        title = "GEE model predictions and bootstrap CI") +
   scale_color_viridis_d(aesthetics = c("colour", "fill"),
                         option = "viridis") +
   scale_x_reverse() +
   coord_cartesian(ylim = c(0, 1))

