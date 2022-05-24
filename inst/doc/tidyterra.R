## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.align = "center"
)

## ----setup, message=TRUE------------------------------------------------------
# Disable message with:
# Sys.setenv(tidyterra.quiet = TRUE)
library(tidyterra)

## -----------------------------------------------------------------------------
library(terra)
f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")

temp <- rast(f)

temp

mod <- temp %>%
  select(-1) %>%
  mutate(newcol = tavg_06 - tavg_05) %>%
  relocate(newcol, .before = 1) %>%
  replace_na(list(newcol = 3)) %>%
  rename(difference = newcol)

mod
plot(mod)

## ----terraplots---------------------------------------------------------------

library(ggplot2)

# A faceted SpatRaster

ggplot() +
  geom_spatraster(data = temp) +
  facet_wrap(~lyr) +
  scale_fill_terrain_c(direction = -1)


# Contour lines for a specific layer

ggplot() +
  geom_spatraster(data = temp, aes(fill = tavg_06)) +
  geom_spatraster_contour(
    data = temp, aes(z = tavg_06),
    color = "black",
    breaks = seq(7.5, 22.5, 2.5)
  ) +
  scale_fill_gradientn(
    colours = hcl.colors(10, "RdBu", rev = TRUE),
    na.value = "white"
  ) +
  coord_sf(expand = FALSE) +
  labs(fill = "tavg_06")


# Contour filled for a specific layer

ggplot() +
  geom_spatraster_contour_filled(
    data = temp, aes(z = tavg_06),
    breaks = seq(7.5, 22.5, 2.5)
  ) +
  scale_fill_viridis_d() +
  coord_sf(expand = FALSE) +
  labs(fill = "tavg_06")

## ----rgb----------------------------------------------------------------------

# Read a vector

f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
v <- vect(f_v)

# Read a tile
f_rgb <- system.file("extdata/cyl_tile.tif", package = "tidyterra")

r_rgb <- rast(f_rgb)

rgb_plot <- ggplot() +
  geom_spatraster_rgb(data = r_rgb) +
  geom_spatvector(data = v, fill = NA, size = 1)

rgb_plot
# Change CRS automatically

rgb_plot +
  coord_sf(crs = 3035)

## -----------------------------------------------------------------------------
lux <- system.file("ex/lux.shp", package = "terra")

spatvector <- terra::vect(lux)

ggplot() +
  geom_spatvector(data = spatvector, aes(fill = POP)) +
  geom_spatvector_text(
    data = spatvector, aes(label = NAME_2),
    color = "grey90"
  ) +
  scale_fill_binned(labels = scales::number_format()) +
  coord_sf(crs = 3857)

## ----terra_sf-----------------------------------------------------------------
lux <- system.file("ex/lux.shp", package = "terra")

spatvector <- terra::vect(lux)
spatvector

terra::plot(spatvector, main = "SpatVector", axes = TRUE)


# To sf
sfobj <- sf::st_as_sf(spatvector)
head(sfobj, 3)

plot(sfobj$geometry, main = "sf", axes = TRUE)

# Back to terra

spatvector2 <- terra::vect(sfobj)

spatvector2

## ----full_spatvector----------------------------------------------------------
library(dplyr)
library(terra)
library(sf)

spat_summ <- spatvector2 %>%
  # to sf
  st_as_sf() %>%
  # dplyr
  group_by(ID_1) %>%
  summarise(TOTPOP = sum(POP)) %>%
  # back to terra
  vect()

spat_summ

terra::plot(spat_summ, "TOTPOP")

