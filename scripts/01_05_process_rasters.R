
# Libraries --------------------------------------------------------------
if (!require(pacman)) install.packages("pacman")
library(raster) # Doesn't like p_load for some reason
pacman::p_load(tidyverse)
pacman::p_load(ggmap)
pacman::p_load(prism)
pacman::p_load(maps)
pacman::p_load(sp)
pacman::p_load(rgdal)
pacman::p_load(broom)
pacman::p_load(viridis)
pacman::p_load(sf)
pacman::p_load(gdalUtils)
pacman::p_load(parallel)

conflicted::conflict_prefer("origin", "raster")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("extract", "raster")

beginCluster(n = (detectCores() / 2) - 6)

# Get north american shapefiles
state_prov <- rnaturalearth::ne_states(c("united states of america", "canada"))
state_prov <- subset(state_prov, name != "Alaska" & name != "Hawaii") 
test <- spTransform(state_prov, CRS("+init=epsg:9820"))
plot(test)
# If you need to rerun this procecssing code set run == TRUE
run = FALSE
if (run == TRUE) {
# Load ash rasters
raster_files <- fs::dir_ls(path = here::here("data/raster_files"),
                           glob = "*.img")


names(raster_files) <- c("fraxinus_spp",
                         "fraxinus_americana",
                         "fraxinus_latifolia",
                         "fraxinus_nigra",
                         "fraxinus_pennsylvanica",
                         "fraxinus_profunda",
                         "fraxinus_quadrangulata",
                         "fraxinus_caroliniana",
                         "fraxinus_texensis")

raster_files <- raster_files[[1]]

# Read in canada biomass data
raster_list <- list.files(path = here::here("data/canadian_forest_data"),
                          pattern = ".tif$",
                          all.files = TRUE,
                          full.names = TRUE,
                          recursive = TRUE)

# Canada ash biomass
canada_ash_biomass <- raster::stack(raster_list)

canada_ash_biomass <- addLayer(canada_ash_biomass,
                               sum(canada_ash_biomass))

# Just canada ash all spp biomass
canada_ash_biomass <- canada_ash_biomass[[4]]
# Too many rasters right now - lets just focus on the genus
usa_ash <- reclassify(raster(raster_files), c(-Inf, 0.001, 0))

# Convert to a non-list
usa_ash <- usa_ash[[1]]

# Reproject
# Get polygons and ash to the same crs
state_prov <- spTransform(state_prov, crs(usa_ash))
usa_polygons <- subset(state_prov, iso_a2 == "US")
can_polygons <- subset(state_prov, iso_a2 == "CA")
canada_ash_biomass <- projectRaster(canada_ash_biomass, crs = crs(usa_ash), res = 250)
can_ash <- canada_ash_biomass

# Crop and mask USA ash data
usa_ash <- crop(usa_ash, usa_polygons)
usa_ash <- mask(usa_ash, usa_polygons)

# Saving as intermediate cause this takes too long for me to keep rerunning
writeRaster(usa_ash, here::here("data/processed_rasters/usa_ash_proc.nc"))
writeRaster(can_ash, here::here("data/processed_rasters/can_ash_proc.nc"))
}

usa_ash <- raster(here::here("data/processed_rasters/usa_ash_proc.nc"))
can_ash <- raster(here::here("data/processed_rasters/can_ash_proc.nc"))

# Daymet rasters for temp
# These are stacks of rasters, with 365 layers (1 for each day).
# Each layer has the minimum air temp for that day. Need to find minimum air temp
# over the three "winter" months of the year.
tmin_2014 <- stack("F:/Downloads/daymet_v3_tmin_2014_na.nc4")
tmin_2018 <- stack("F:/Downloads/daymet_v3_tmin_2018_na.nc4")

# Sumamrize the stacks into a single layer. Just need to go over the first ~90 days
# tmin_2014 <- min(tmin_2014[[1:90]])
# tmin_2018 <- min(tmin_2018[[1:90]])
# tmin <- stack(tmin_2014, tmin_2018)
# names(tmin) <- c("tmin_2014", "tmin_2018")
# tmin <- projectRaster(tmin, crs = crs(state_prov))
# writeRaster(tmin, "data/processed_rasters/daymet_tmin.nc")
tmin <- stack(here::here("data/processed_rasters/daymet_tmin.nc"))

# Reclassify ash rasters to presence/absence
usa_ash <- reclassify(usa_ash, c(0.001, Inf, 1,
                              -Inf, 0, NA))
can_ash <- reclassify(can_ash, c(0.001, Inf, 1,
                                 -Inf, 0, NA))

# Convert ash rasters to data frames for tempearture extraction
usa_ash_df <- SpatialPoints(data.frame(rasterToPoints(usa_ash)))
can_ash_df <- SpatialPoints(data.frame(rasterToPoints(can_ash)))

# Index temperature raster cells that contain ash, set any cell that doesn't to NA
# Separate into canadian and 
ii <- extract(tmin, usa_ash_df, cellnumbers = TRUE)[, "cells"]
jj <- extract(tmin, can_ash_df, cellnumbers = TRUE)[, "cells"]
index <- c(ii, jj)
tmin_ash <- tmin
tmin_ash[-index] <- NA
plot(tmin_ash)
names(tmin_ash) <- names(tmin)
tmin_df <- data.frame(rasterToPoints(tmin_ash[[1]]))
names(tmin_df) <- c("x", "y", "value")

winter_2013_2014 <- tmin_ash[[1]]
winter_2017_2018 <- tmin_ash[[2]]

na_sf <- st_as_sf(state_prov)

ggplot(tmin_df, aes(x = x, y = y, fill = value)) +
   geom_raster() +
   geom_sf(data = na_sf, inherit.aes = FALSE, fill = NA, color = "black")

# # Save output
# writeRaster(winter_2013_2014,
#         filename = here::here("data/processed_rasters/daymet_winter_2013_2014.nc"),
#         overwrite = TRUE)
# writeRaster(winter_2017_2018,
#         filename = here::here("data/processed_rasters/daymet_winter_2017_2018.nc"),
#         overwrite = TRUE)
# saveRDS(na_sf,
#         here::here("data/processed_rasters/na_sf_polygons.RDS"))


endCluster()




# Old code ----------------------------------------------------------------
# World clim rasters
wc_rasters <- fs::dir_ls(path = here::here("data/worldclim_data"),
                         glob = "*.tif")

# Get appropriate years
wc_rasters <- wc_rasters[str_detect(wc_rasters, "2017") |
                            str_detect(wc_rasters, "2018") |
                            str_detect(wc_rasters, "2013") |
                            str_detect(wc_rasters, "2014")]
# Get appropriate months
wc_rasters <- wc_rasters[str_detect(wc_rasters, "-10") |
                            str_detect(wc_rasters, "-11") |
                            str_detect(wc_rasters, "-12") |
                            str_detect(wc_rasters, "-01") |
                            str_detect(wc_rasters, "-02") |
                            str_detect(wc_rasters, "-03")]

wc_rasters <- raster::stack(wc_rasters)


wc_rasters <- crop(wc_rasters, c(-180, -50, 20, 90))


# Change CRS
wc_rasters_reproj <- projectRaster(wc_rasters, crs = crs(state_prov))

wc_rasters_reproj <- crop(wc_rasters_reproj, state_prov)
wc_rasters_reproj <- mask(wc_rasters_reproj, state_prov)

# extract values
ii <- extract(wc_rasters_reproj, usa_ash_df, cellnumbers = TRUE)[, "cells"]
jj <- extract(wc_rasters_reproj, can_ash_df, cellnumbers = TRUE)[, "cells"]
index <- c(ii, jj)
tmin_ash <- wc_rasters_reproj
tmin_ash[-index] <- NA
plot(tmin_ash)
names(tmin_ash) <- names(wc_rasters_reproj)
tmin_df <- data.frame(rasterToPoints(tmin_ash[[1]]))
names(tmin_df) <- c("x", "y", "value")
plot(tmin_ash)

winter_2013_2014 <- tmin_ash[[4:9]]
winter_2017_2018 <- tmin_ash[[16:21]]
winter_2013_2014 <- min(winter_2013_2014)
winter_2017_2018 <- min(winter_2017_2018)

na_sf <- st_as_sf(state_prov)

ggplot(tmin_df, aes(x = x, y = y, fill = value)) +
   geom_raster() +
   geom_sf(data = na_sf, inherit.aes = FALSE, fill = NA, color = "black")

# # Save output
writeRaster(winter_2013_2014,
            filename = here::here("data/processed_rasters/wc_winter_2013_2014.nc"),
            overwrite = TRUE)
writeRaster(winter_2017_2018,
            filename = here::here("data/processed_rasters/wc_winter_2017_2018.nc"),
            overwrite = TRUE)
