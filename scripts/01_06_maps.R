
# Source files------------------------------------------------------------------

source("scripts/01_04_scp_models.R")


# Libraries ---------------------------------------------------------------
# Winter mintemp data from: https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1328
# Daymet - Daily surface weather data

if (!require(pacman)) install.packages("pacman")
library(raster) # Doesn't like p_load for some reason
pacman::p_load(tidyverse)
pacman::p_load(ggmap)
pacman::p_load(prism)
pacman::p_load(maps)
pacman::p_load(sp)
pacman::p_load(sf)
pacman::p_load(rgdal)
pacman::p_load(broom)
pacman::p_load(viridis)



# Load processed rasters
# Each of these is a nested list. list[[tree_spp]][[month]]
winter_2013_2014 <- readRDS(here::here("data/processed_rasters/wc_winter_2013_2014.RDS"))
winter_2017_2018 <- readRDS(here::here("data/processed_rasters/wc_winter_2017_2018.RDS"))
CRS_prism_rasters <- crs(winter_2013_2014[[1]][[1]])

# Get shapefiles
na_sf <- readRDS(here::here("data/processed_rasters/na_sf_polygons.RDS"))

# Convert to data.frame
winter_2013_2014 <- data.frame(rasterToPoints(winter_2013_2014))
names(winter_2013_2014) <- c("x",
                             "y",
                             "temp_pulled_grouped")

winter_2017_2018 <- data.frame(rasterToPoints(winter_2017_2018))
names(winter_2017_2018) <- c("x",
                             "y",
                             "temp_pulled_grouped")



# Find minimum winter temperature for a given coordinate

# Temp_pulled_group = minimum winter temp. Named for use later in getting 
# predictions.
winter_2013_2014$degree_min_continuous <- 1


winter_2017_2018$degree_min_continuous <- 1

# Plot temperature data
ggplot()+
   geom_tile(data = winter_2013_2014, aes(x = x, y = y, fill = temp_pulled_grouped)) +
   theme_nothing(legend = TRUE) +
   coord_fixed(ratio = 1.3) +
   labs(title = "Lowest temperature (C) seen Oct. 2013 - Mar. 2014") +
   #scale_fill_viridis(limits = c(-30, 20)) +
   geom_sf(data = na_sf, fill = NA)

ggplot()+
   geom_tile(data = winter_2017_2018, aes(x = x, y = y, fill = temp_pulled_grouped)) +
   theme_nothing(legend = TRUE) +
   coord_fixed(ratio = 1.3) +
   labs(title = "Lowest temperature (C) seen Oct. 2017 - Mar. 2018") +
   #scale_fill_viridis(limits = c(-30, 20)) +
   geom_sf(data = na_sf, fill = NA)

# Get predicted values based on model results
# Get predicted results

winter_2013_2014$degree_min <- as.factor(1)
winter_2013_2014$degree_min_continuous <- 1
pred_dat <- predict(map_model,
               newdata = winter_2013_2014,
               type = "response",
               re.form = NA)
winter_2013_2014$fitted <- pred_dat


winter_2017_2018$degree_min <- as.factor(1)
winter_2017_2018$degree_min_continuous <- 1
pred_dat <- predict(map_model,
                    newdata = winter_2017_2018,
                    type = "response",
                    re.form = NA)
winter_2017_2018$fitted <- pred_dat


eclosion_2013_2014 <- ggplot()+
   geom_tile(data = winter_2013_2014, aes(x = x, y = y, fill = fitted)) +
   theme_nothing(legend = TRUE) +
   coord_fixed(ratio = 1.3) +
   scale_fill_viridis(direction = -1,
                      limits = c(0.0, 1.0),
                      name = expression(atop(paste("Percent ", italic("S. galinae ")), "surviving 2013 - 2014"))) +
   geom_sf(data = na_sf, color = "black", fill = NA) +
   theme(legend.title.align = 0,
         legend.position = c(0.9, 0.3),
         axis.line = element_blank())

eclosion_2017_2018 <- ggplot()+
   geom_tile(data = winter_2017_2018, aes(x = x, y = y, fill = fitted)) +
   theme_nothing(legend = TRUE) +
   coord_fixed(ratio = 1.3) +
   scale_fill_viridis(direction = -1,
                      limits = c(0.0, 1.0),
                      name = expression(atop(paste("Percent ", italic("S. galinae ")), "surviving 2013 - 2014"))) +
   geom_sf(data = na_sf, color = "black", fill = NA) +
   theme(legend.title.align = 0,
         legend.position = c(0.9, 0.3),
         axis.line = element_blank())

eclosion_2013_2014 + eclosion_2017_2018


mortality_plots_17_18 <- lapply(winter_2017_2018, function(x) {
   ggplot()+
      geom_tile(data = x, aes(x = x, y = y, fill = fitted)) +
      theme_nothing(legend = TRUE) +
      coord_fixed(ratio = 1.3) +
      scale_fill_viridis(direction = -1,
                         limits = c(0.0, 1.0),
                         name = expression(atop(paste("Percent ", italic("S. galinae ")), "surviving 2017 - 2018"))) +
      geom_polygon(data = usa,
                   aes(x = long, y = lat, group = group),
                   fill = "transparent",
                   color = "black") +
      theme(legend.title.align = 0,
            legend.position = c(0.9, 0.3),
            axis.line = element_blank())
})

endCluster()

# Save maps
ggsave(here::here("figures/winter20132014_mortality.tiff"),
       mortality_plots_13_14[[1]],
       height = 6,
       width = 10,
       units = "in",
       dpi = 300)

ggsave(here::here("figures/winter20172018_mortality.tiff"),
       mortality_plots_17_18[[1]],
       height = 6,
       width = 10,
       units = "in",
       dpi = 300)
