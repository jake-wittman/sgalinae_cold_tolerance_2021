# Libraries and other setup ----------------------------------------------------

if (!require(pacman)) install.packages("pacman")
if (!require(rio)) install.packages("rio")
pacman::p_load(tidyverse)
pacman::p_load(lubridate)
pacman::p_load(here)
pacman::p_load(survminer)
pacman::p_load(survival)
pacman::p_load(doBy)
pacman::p_load(ggbeeswarm)
conflicted::conflict_prefer("here", "here")
conflicted::conflict_prefer("filter", "dplyr")

# Source files
source("scripts/01_01_clean_scp.R")

# Set ggplot theme
theme_set(theme_classic())

# Exploratory data analysis -----------------------------------------------

# Proportion of controls surviving
dat %>% 
   filter(channel == "control") %>% 
   group_by(degree_min) %>% 
   summarise(prop = mean(discolor_3day),
             total = length(discolor_3day),
             eclose_prop = mean(eclose, na.rm = T),
             total_eclose = length(eclose))


# Discoloration summary ---------------------------------------------------



# Proportion surviving at day 3 by temperature pulled and cooling rate
prop_surv_dat <- dat %>% 
   group_by(degree_min, temp_pulled) %>% 
   summarise(prop = mean(discolor_3day),
             total = length(discolor_3day))

prop_surv_dat_grouped <- dat %>% 
   group_by(degree_min, temp_pulled_grouped) %>% 
   summarise(prop = mean(discolor_3day),
             total = length(discolor_3day))

prop_surv_dat %>% 
   ggplot(aes(x = temp_pulled, y = prop, colour = degree_min)) +
   geom_point() +
   scale_x_reverse() +
   geom_smooth()

prop_surv_dat_grouped %>% 
   ggplot(aes(x = temp_pulled_grouped, y = prop, colour = degree_min)) +
   geom_point() +
   scale_x_reverse() +
   geom_smooth(aes(colour = degree_min))

# Raw survival (0 or 1) with smoother
dat %>% 
   ggplot(aes(x = temp_pulled_grouped, y = discolor_3day, colour = degree_min)) +
   geom_jitter(width = 0.3, height = 0.05) +
   scale_x_reverse() +
   geom_smooth() 
   

# Eclosion summary --------------------------------------------------------
prop_eclose <- dat %>% 
   group_by(degree_min, temp_pulled) %>% 
   summarise(prop = mean(eclose),
             total = length(eclose))

prop_eclose_grouped <- dat %>% 
   group_by(degree_min, temp_pulled_grouped) %>% 
   summarise(prop = mean(eclose),
             total = length(eclose))

prop_eclose %>% 
   ggplot(aes(x = temp_pulled, y = prop, colour = degree_min)) +
   geom_point() +
   scale_x_reverse() +
   geom_smooth() +
   scale_y_continuous(labels = seq(0, 1, 0.25),
                      breaks = seq(0, 1, 0.25),
                      limits = c(0, 1))

prop_eclose_grouped %>% 
   ggplot(aes(x = temp_pulled_grouped, y = prop, colour = degree_min)) +
   geom_point() +
   scale_x_reverse() +
   geom_smooth(aes(colour = degree_min)) +
   scale_y_continuous(labels = seq(0, 1, 0.25),
                      breaks = seq(0, 1, 0.25),
                      limits = c(0, 1))


# SCP summary -------------------------------------------------------------

# SCP summary
dat %>% 
   group_by(degree_min) %>% 
   summarise(mean_scp = mean(est_scp, na.rm = TRUE))
mean(dat$est_scp, na.rm = TRUE)

# SCP plot
dat %>% 
   ggplot(aes(x = degree_min, y = est_scp)) +
   geom_boxplot() +
   geom_jitter(width = 0.03, height = 0.03, alpha = 0.5)

scp_facet_plot <- dat %>% 
   filter(est_scp < -18) %>% 
   ggplot(aes(x = degree_min, y = est_scp)) +
   geom_boxplot() +
   geom_point(position = position_beeswarm(cex = 1), alpha = 0.5) +
   labs(x = "Chill Rate (°C/min)", y = "Estimated supercooling point (°C)") +
   theme(axis.text = element_text(size = 23, colour = "black"),
         axis.title = element_text(size = 25, colour = "black"),
         axis.title.y = element_text(hjust = 0.75))

# ggsave(here::here("figures/scp_facet_plot.tiff"),
#        scp_facet_plot,
#        height = 6,
#        width = 7,
#        units = "in",
#        dpi = 300)

dat %>% 
   filter(est_scp < -18) %>% 
   ggplot() +
   geom_density(aes(x = est_scp)) +
   scale_x_reverse() +
   facet_wrap(~degree_min)

scp_plot <- dat %>%  filter(est_scp < -18) %>% 
   ggplot(aes(x = "", y = est_scp)) +
   geom_boxplot() +
   geom_point(position = position_beeswarm(cex = 1), alpha = 0.5) +
   labs(x = NULL, y = "Estimated supercooling point (°C)") +
   theme(axis.text = element_text(size = 23, colour = "black"),
         axis.title = element_text(size = 25, colour = "black"),
         axis.ticks.x = element_blank(),
         axis.title.y = element_text(hjust = 0.75))
# ggsave(here::here("figures/scp_plot.tiff"),
#        scp_plot,
#        height = 6,
#        width = 7,
#        units = "in",
#        dpi = 300)

# Columns are discolor, rows are eclose
# 0 discolor = discolored, 1 = normal colored,
# 0 eclose = no eclosion, 1 = yes eclosion
total_eclose_discolor <- janitor::tabyl(dat, discolor_3day, eclose, show_na = FALSE) %>% 
   as.tibble()

eclose_discolor_plot <- ggplot(dat, aes(x = discolor_3day, y = eclose)) +
   geom_jitter(width = 0.025, height = 0.025) +
   theme_classic() +
   labs(x = "Larvae normal colored", y = "Adult eclosed") +
   scale_x_continuous(breaks = c(0, 1),
                    labels = c("No", "Yes")) +
   scale_y_continuous(breaks = c(0, 1),
                      labels = c("No", "Yes")) +
   annotate("text",
            x = 0.1,
            y = 0.15,
            label = paste0("n = ", total_eclose_discolor[1, 2]),
            size = 18) +
   annotate("text",
            x = 0.1,
            y = 0.85,
            label = paste0("n = ", total_eclose_discolor[1, 3]),
            size = 18) +
   annotate("text",
            x = 0.9,
            y = 0.85,
            label = paste0("n = ", total_eclose_discolor[2, 3]),
            size = 18) +
   annotate("text",
            x = 0.9,
            y = 0.15,
            label = paste0("n = ", total_eclose_discolor[2, 2]),
            size = 18) +
   theme(axis.text = element_text(size = 26),
         axis.title = element_text(size = 26))

# ggsave(here::here("figures/eclose_discolor_plot.tiff"),
#        eclose_discolor_plot,
#        height = 6,
#        width = 6,
#        units = "in",
#        dpi = 300)

write_csv(dat, file = here::here("data/cleaned_scp_data.csv"))
