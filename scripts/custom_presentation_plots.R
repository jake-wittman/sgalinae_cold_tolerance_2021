if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse)
pacman::p_load(viridis)
pacman::p_load(gganimate)

conflicted::conflict_prefer("animate", "gganimate")

# Source relevant scripts
source(here::here("scripts/02_01_scp.R"))

# Cooling rate graphs -----------------------------------------------------

cooling_rate <- data.frame(
   temp = rep(seq(2, -32, -2), 3),
   cool_rate = rep(c(1, 0.5, 0.1), each = 18),
   time = c(seq(0, 34, by = 2),
            seq(0, 68, by = 4),
            seq(0, 340, by = 20))
)
cooling_rate$cool_rate_factor <- as.factor(cooling_rate$cool_rate)

cool_rate_plot <- ggplot(cooling_rate,
                         aes(
                            x = time,
                            y = temp,
                            group = cool_rate_factor,
                            colour = cool_rate_factor
                         )) +
   geom_line(size = 1.07) +
   theme_classic() +
   labs(y = "Temperature (°C)", x = "Time Elapsed (minutes)") +
   scale_colour_viridis_d(name = "Chill Rate (°C/min)") +
   theme(
      axis.text = element_text(size = 23),
      axis.title = element_text(size = 26),
      legend.title = element_text(size = 30),
      legend.text = element_text(size = 28),
      legend.position = c(0.7, 0.8)
   ) + transition_reveal(time)

cool_rate_animation <- animate(
   cool_rate_plot,
   height = 800,
   width = 1400,
   fps = 20,
   nframes = 1000,
   end_pause = 600
)
anim_save(here::here("presentations/cool_rate_animation.gif"),
          cool_rate_animation)


# Pull insects gif --------------------------------------------------------
insect_pull_dat <- scp_files_long$`data/2019_supercooling_winter/12_3_2019_ct_trial_c.csv` %>% 
   filter(channel %in% c("channel10",
                         "channel11",
                         "channel14",
                         "channel15",
                         "channel3",
                         "channel4",
                         "channel5",
                         "channel7",
                         "channel9"))

insect_pull_dat$channel_f <- factor(insect_pull_dat$channel,
                                    levels = c("channel7",
                                               "channel10",
                                               "channel11",
                                               "channel14",
                                               "channel15",
                                               "channel5",
                                               "channel3",
                                               "channel9",
                                               "channel4")) 

insect_pull_plot <- ggplot(insect_pull_dat, aes(x = seconds, y = temp, group = channel_f, colour = channel_f)) +
   geom_line() +
   labs(x = "Time elapsed (seconds)", y = "Temperature (°C)") +
   scale_y_continuous(limits = c(-20, 3)) +
   theme_bw() +
   facet_wrap(~channel_f) +
   theme(legend.position = "none",
         strip.background = element_blank(),
         strip.text = element_blank(),
         axis.text = element_text(size = 23, colour = "black"),
         axis.title = element_text(size = 26, colour = "black"))  +
   transition_reveal(seconds)

insect_pull_animation <- animate(
   insect_pull_plot,
   height = 800,
   width = 1400,
   fps = 20,
   nframes = 1000,
   end_pause = 600
)

anim_save(here::here("presentations/insect_pull_animation.gif"),
          insect_pull_animation)

# Exotherm gif -----------------------------------------------------------

exo_dat <- scp_files_long$`data/2019_supercooling_winter/12_3_2019_ct_trial_f.csv` %>% 
   filter(sample_number < 5000) %>%
   filter(temp <= -15) %>% 
   filter(channel %in% c("channel15",
                         "channel11",
                         "channel3",
                         "channel4",
                         "channel7",
                         "channel9"))


exotherm_plot <- ggplot(exo_dat, aes(x = seconds, y = temp, group = channel, colour = channel)) +
   geom_line() +
   labs(x = "Time elapsed (seconds)", y = "Temperature (°C)") +
   scale_y_continuous(limits = c(-32, -15)) +
   theme_bw() +
   facet_wrap(~channel) +
   theme(legend.position = "none",
         strip.background = element_blank(),
         strip.text = element_blank(),
         axis.text = element_text(size = 23, colour = "black"),
         axis.title = element_text(size = 26, colour = "black")) +
   transition_reveal(seconds) 

exotherm_animation <- animate(
   exotherm_plot,
   height = 800,
   width = 1400,
   fps = 20,
   nframes = 1000,
   end_pause = 600
)
anim_save(here::here("presentations/exotherm_animation.gif"),
          exotherm_animation)



