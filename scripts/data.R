# paquetes ---------------------------------------------------------------

library(terra)

# vectores ---------------------------------------------------------------

# Aconcagua
ac <- vect(
  data.frame(lon = -70.011667, lat = -32.653056),
  geom = c("lon", "lat"),
  crs = "EPSG:4326"
)

# Las Heras
lh <- vect(
  data.frame(lon = -68.840447, lat = -32.850355),
  geom = c("lon", "lat"),
  crs = "EPSG:4326"
)

p <- rbind(ac, lh)
p$label <- c("Aconcagua (6960 m)", "Las Heras")
p_sf <- sf::st_as_sf(p)

# Argentina
arg <- vect("vectores/dptos_pcias_continental.gpkg") |>
  project("EPSG:4326")

# Mendoza
mdz <- arg[arg$provincia == "Mendoza"] |>
  aggregate()

#Las Heras
lh_v <- arg[arg$provincia == "Mendoza" & arg$departamentos == "Las Heras"]

# Mendoza menos Las Heras
lh_no_v <- arg[arg$provincia == "Mendoza" & arg$departamentos != "Las Heras"] |>
  aggregate()

mdz_lh <- union(lh_v, lh_no_v)

# ROI
roi <- ext(lh_v)

# elevación --------------------------------------------------------------

r <- marmap::getNOAA.bathy(
  roi$xmin,
  roi$xmax,
  roi$ymin,
  roi$ymax,
  resolution = .01
) |>
  marmap::as.raster() |>
  rast() |>
  project(crs(lh_v)) |>
  mask(lh_v)

# temperatura ------------------------------------------------------------

# Argentina
tavg <- geodata::worldclim_country(
  country = "Argentina",
  var = "tavg",
  res = .5
)

# meses como nombres de bandas
t_hl_meses <- paste0("2020-", 1:12, "-01") |>
  as.Date() |>
  format("%B") |>
  stringr::str_to_sentence()

names(tavg) <- t_hl_meses

# recorto a Las Heras
t_lh <- crop(tavg, lh_v, mask = TRUE)

# color real -------------------------------------------------------------

mt <- maptiles::get_tiles(
  x = r,
  provider = "Esri.WorldImagery",
  zoom = 10,
  crop = TRUE
)
