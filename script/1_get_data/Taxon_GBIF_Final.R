## Bivalvia 

library(data.table)
library(plyr)
library(dplyr)
library(raster)
library(readr)
library(rgdal)
library(dplyr)
library(sf)
library(sp)
library(tidyr)
library(tmap)
library(tidyverse)

## Read Shape

shpfile = readOGR("data/shape/buffer_hydro_global/b_join_river_country.shp")

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

gbifraw = data.table::fread("data-raw/bivalvia/0166864-220831081235567.csv") 

gbif = gbifraw |> 
  dplyr::as_tibble() |> 
  dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

xy = gbif |> 
  dplyr::select(decimalLatitude, decimalLongitude)

pts = SpatialPointsDataFrame(coords = xy, data = gbif, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

gb = st_as_sf(pts, coords = xy, st_crs(4326)) |> 
  dplyr::rename(ISO_A2 = countryCode)

shp = st_as_sf(shpfile, st_crs(4326))

good_points = st_filter(gb, shp)

#plot(st_geometry(good_points))

cntshp = st_as_sf(countryshp, st_crs(4326))

bivalviajoin = st_join(cntshp, good_points) |> 
   as_tibble()

bivalvia_n_sp = bivalviajoin |>
  tidyr::drop_na(c(decimalLatitude, decimalLongitude, species, ISO_A2.x)) |> 
  dplyr::select(species, ISO_A2.x) |>
  dplyr::distinct(species, ISO_A2.x) |> 
  dplyr::group_by(species,ISO_A2.x) #|> 
  #raster::subset(ISO_A2.x != -99) 

bivalvia_N_SPP = plyr::count(bivalvia_n_sp$ISO_A2.x) 
bivalvia_N_SPP = dplyr::rename(bivalvia_N_SPP, ISO_A2.x = x)

bivalvia_N_SP = merge(bivalviajoin, bivalvia_N_SPP, by= "ISO_A2.x")

## Count occ per country 

bivalvia_N_OCC =  bivalvia_N_SP |> 
  dplyr::group_by(ISO_A2.x) |> 
  dplyr::summarise(gbifID = n()) 

## Merge  

bivalvia_final = merge(bivalvia_N_SP, bivalvia_N_OCC, by= "ISO_A2.x") |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

## Plot

bivalviaMap = bivalvia_final  |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)  

bivalviaMap2 = cntshp |> 
  left_join(bivalviaMap, by = "ADMIN") #|> 
  #mutate_at(vars(N_Species:N_Occurrences), ~replace(., is.na(.), 0))

bbox_new = st_bbox(bivalviaMap2) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

png(file="Bivalvia",
    width=9, height=7, units="in", res=300)
tm_shape(bivalviaMap2,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 4,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Bivalvia",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()
rm(list = ls())
.rs.restartR()

## Porifera

library(data.table)
library(plyr)
library(dplyr)
library(raster)
library(readr)
library(rgdal)
library(dplyr)
library(sf)
library(sp)
library(tidyr)
library(tmap)
library(tidyverse)

## Read Shape

shpfile = readOGR("data/shape/buffer_hydro_global/b_join_river_country.shp")

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")


gbifraw = data.table::fread("data-raw/porifera/0166867-220831081235567.csv") 

gbif = gbifraw |> 
  dplyr::as_tibble() |> 
  dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

xy = gbif |> 
  dplyr::select(decimalLatitude, decimalLongitude)

pts = SpatialPointsDataFrame(coords = xy, data = gbif, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

gb = st_as_sf(pts, coords = xy, st_crs(4326)) |> 
  dplyr::rename(ISO_A2 = countryCode)

shp = st_as_sf(shpfile, st_crs(4326))

good_points = st_filter(gb, shp)

#plot(st_geometry(good_points))

cntshp = st_as_sf(countryshp, st_crs(4326))

poriferajoin = st_join(cntshp, good_points) |> 
  as_tibble()

porifera_n_sp = poriferajoin |>
  tidyr::drop_na(c(decimalLatitude, decimalLongitude, species, ISO_A2.x)) |> 
  dplyr::select(species, ISO_A2.x) |>
  dplyr::distinct(species, ISO_A2.x) |> 
  dplyr::group_by(species,ISO_A2.x) #|> 
#raster::subset(ISO_A2.x != -99) 

porifera_N_SPP = plyr::count(porifera_n_sp$ISO_A2.x) 
porifera_N_SPP = dplyr::rename(porifera_N_SPP, ISO_A2.x = x)

porifera_N_SP = merge(poriferajoin, porifera_N_SPP, by= "ISO_A2.x")

## Count occ per country 

porifera_N_OCC =  porifera_N_SP |> 
  dplyr::group_by(ISO_A2.x) |> 
  dplyr::summarise(gbifID = n()) 

## Merge  

porifera_final = merge(porifera_N_SP, porifera_N_OCC, by= "ISO_A2.x") |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

## Plot

poriferaMap = porifera_final  |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)  

poriferaMap2 = cntshp |> 
  left_join(poriferaMap, by = "ADMIN") #|> 
  #mutate_at(vars(N_Species:N_Occurrences), ~replace(., is.na(.), 0))

bbox_new = st_bbox(poriferaMap2) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

png(file="Porifera",
    width=9, height=7, units="in", res=300)
tm_shape(poriferaMap2,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 4,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Porifera",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()
rm(list = ls())
.rs.restartR()

## Bryozoa

library(data.table)
library(plyr)
library(dplyr)
library(raster)
library(readr)
library(rgdal)
library(dplyr)
library(sf)
library(sp)
library(tidyr)
library(tmap)
library(tidyverse)

## Read Shape

shpfile = readOGR("data/shape/buffer_hydro_global/b_join_river_country.shp")

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")


gbifraw = data.table::fread("data-raw/bryozoa/0166881-220831081235567.csv") 

gbif = gbifraw |> 
  dplyr::as_tibble() |> 
  dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

xy = gbif |> 
  dplyr::select(decimalLatitude, decimalLongitude)

pts = SpatialPointsDataFrame(coords = xy, data = gbif, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

gb = st_as_sf(pts, coords = xy, st_crs(4326)) |> 
  dplyr::rename(ISO_A2 = countryCode)

shp = st_as_sf(shpfile, st_crs(4326))

good_points = st_filter(gb, shp)

#plot(st_geometry(good_points))

cntshp = st_as_sf(countryshp, st_crs(4326))

bryozoajoin = st_join(cntshp, good_points) |> 
  as_tibble()

bryozoa_n_sp = bryozoajoin |>
  tidyr::drop_na(c(decimalLatitude, decimalLongitude, species, ISO_A2.x)) |> 
  dplyr::select(species, ISO_A2.x) |>
  dplyr::distinct(species, ISO_A2.x) |> 
  dplyr::group_by(species,ISO_A2.x) #|> 
#raster::subset(ISO_A2.x != -99) 

bryozoa_N_SPP = plyr::count(bryozoa_n_sp$ISO_A2.x) 
bryozoa_N_SPP = dplyr::rename(bryozoa_N_SPP, ISO_A2.x = x)

bryozoa_N_SP = merge(bryozoajoin, bryozoa_N_SPP, by= "ISO_A2.x")

## Count occ per country 

bryozoa_N_OCC =  bryozoa_N_SP |> 
  dplyr::group_by(ISO_A2.x) |> 
  dplyr::summarise(gbifID = n()) 

## Merge  

bryozoa_final = merge(bryozoa_N_SP, bryozoa_N_OCC, by= "ISO_A2.x") |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

## Plot

bryozoaMap = bryozoa_final  |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)  

bryozoaMap2 = cntshp |> 
  left_join(bryozoaMap, by = "ADMIN") #|> 
  #mutate_at(vars(N_Species:N_Occurrences), ~replace(., is.na(.), 0))

bbox_new = st_bbox(bryozoaMap2) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

png(file="Bryozoa",
    width=9, height=7, units="in", res=300)
tm_shape(bryozoaMap2,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 4,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Bryozoa",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()
rm(list = ls())
.rs.restartR()


## Cnidaria

library(data.table)
library(plyr)
library(dplyr)
library(raster)
library(readr)
library(rgdal)
library(dplyr)
library(sf)
library(sp)
library(tidyr)
library(tmap)
library(tidyverse)

## Read Shape

shpfile = readOGR("data/shape/buffer_hydro_global/b_join_river_country.shp")

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")


gbifraw = data.table::fread("data-raw/cnidaria/0166895-220831081235567.csv") 

gbif = gbifraw |> 
  dplyr::as_tibble() |> 
  dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

xy = gbif |> 
  dplyr::select(decimalLatitude, decimalLongitude)

pts = SpatialPointsDataFrame(coords = xy, data = gbif, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

gb = st_as_sf(pts, coords = xy, st_crs(4326)) |> 
  dplyr::rename(ISO_A2 = countryCode)

shp = st_as_sf(shpfile, st_crs(4326))

good_points = st_filter(gb, shp)

#plot(st_geometry(good_points))

cntshp = st_as_sf(countryshp, st_crs(4326))

cnidariajoin = st_join(cntshp, good_points) |> 
  as_tibble()

cnidaria_n_sp = cnidariajoin |>
  tidyr::drop_na(c(decimalLatitude, decimalLongitude, species, ISO_A2.x)) |> 
  dplyr::select(species, ISO_A2.x) |>
  dplyr::distinct(species, ISO_A2.x) |> 
  dplyr::group_by(species,ISO_A2.x) #|> 
#raster::subset(ISO_A2.x != -99) 

cnidaria_N_SPP = plyr::count(cnidaria_n_sp$ISO_A2.x) 
cnidaria_N_SPP = dplyr::rename(cnidaria_N_SPP, ISO_A2.x = x)

cnidaria_N_SP = merge(cnidariajoin, cnidaria_N_SPP, by= "ISO_A2.x")

## Count occ per country 

cnidaria_N_OCC =  cnidaria_N_SP |> 
  dplyr::group_by(ISO_A2.x) |> 
  dplyr::summarise(gbifID = n()) 

## Merge  

cnidaria_final = merge(cnidaria_N_SP, cnidaria_N_OCC, by= "ISO_A2.x") |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

## Plot

cnidariaMap = cnidaria_final  |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)  

cnidariaMap2 = cntshp |> 
  left_join(cnidariaMap, by = "ADMIN") #|> 
#mutate_at(vars(N_Species:N_Occurrences), ~replace(., is.na(.), 0))

bbox_new = st_bbox(cnidariaMap2) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

png(file="Cnidaria",
    width=9, height=7, units="in", res=300)
tm_shape(cnidariaMap2,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 4,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Cnidaria",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()
rm(list = ls())
.rs.restartR()

## Simuliidae

library(data.table)
library(plyr)
library(dplyr)
library(raster)
library(readr)
library(rgdal)
library(dplyr)
library(sf)
library(sp)
library(tidyr)
library(tmap)
library(tidyverse)

## Read Shape

shpfile = readOGR("data/shape/buffer_hydro_global/b_join_river_country.shp")

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")


gbifraw = data.table::fread("data-raw/simuliidae/0166900-220831081235567.csv") 

gbif = gbifraw |> 
  dplyr::as_tibble() |> 
  dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

xy = gbif |> 
  dplyr::select(decimalLatitude, decimalLongitude)

pts = SpatialPointsDataFrame(coords = xy, data = gbif, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

gb = st_as_sf(pts, coords = xy, st_crs(4326)) |> 
  dplyr::rename(ISO_A2 = countryCode)

shp = st_as_sf(shpfile, st_crs(4326))

good_points = st_filter(gb, shp)

#plot(st_geometry(good_points))

cntshp = st_as_sf(countryshp, st_crs(4326))

simuliidaejoin = st_join(cntshp, good_points) |> 
  as_tibble()

simuliidae_n_sp = simuliidaejoin |>
  tidyr::drop_na(c(decimalLatitude, decimalLongitude, species, ISO_A2.x)) |> 
  dplyr::select(species, ISO_A2.x) |>
  dplyr::distinct(species, ISO_A2.x) |> 
  dplyr::group_by(species,ISO_A2.x) #|> 
#raster::subset(ISO_A2.x != -99) 

simuliidae_N_SPP = plyr::count(simuliidae_n_sp$ISO_A2.x) 
simuliidae_N_SPP = dplyr::rename(simuliidae_N_SPP, ISO_A2.x = x)

simuliidae_N_SP = merge(simuliidaejoin, simuliidae_N_SPP, by= "ISO_A2.x")

## Count occ per country 

simuliidae_N_OCC =  simuliidae_N_SP |> 
  dplyr::group_by(ISO_A2.x) |> 
  dplyr::summarise(gbifID = n()) 

## Merge  

simuliidae_final = merge(simuliidae_N_SP, simuliidae_N_OCC, by= "ISO_A2.x") |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

## Plot

simuliidaeMap = simuliidae_final  |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)  

simuliidaeMap2 = cntshp |> 
  left_join(simuliidaeMap, by = "ADMIN") #|> 
#mutate_at(vars(N_Species:N_Occurrences), ~replace(., is.na(.), 0))

bbox_new = st_bbox(simuliidaeMap2) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

png(file="Simuliidae",
    width=9, height=7, units="in", res=300)
tm_shape(simuliidaeMap2,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 4,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Simuliidae",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()
rm(list = ls())
.rs.restartR()


## Culicidae

library(data.table)
library(plyr)
library(dplyr)
library(raster)
library(readr)
library(rgdal)
library(dplyr)
library(sf)
library(sp)
library(tidyr)
library(tmap)
library(tidyverse)

## Read Shape

shpfile = readOGR("data/shape/buffer_hydro_global/b_join_river_country.shp")

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")


gbifraw = data.table::fread("data-raw/culicidae/0166907-220831081235567.csv") 

gbif = gbifraw |> 
  dplyr::as_tibble() |> 
  dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

xy = gbif |> 
  dplyr::select(decimalLatitude, decimalLongitude)

pts = SpatialPointsDataFrame(coords = xy, data = gbif, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

gb = st_as_sf(pts, coords = xy, st_crs(4326)) |> 
  dplyr::rename(ISO_A2 = countryCode)

shp = st_as_sf(shpfile, st_crs(4326))

good_points = st_filter(gb, shp)

#plot(st_geometry(good_points))

cntshp = st_as_sf(countryshp, st_crs(4326))

culicidaejoin = st_join(cntshp, good_points) |> 
  as_tibble()

culicidae_n_sp = culicidaejoin |>
  tidyr::drop_na(c(decimalLatitude, decimalLongitude, species, ISO_A2.x)) |> 
  dplyr::select(species, ISO_A2.x) |>
  dplyr::distinct(species, ISO_A2.x) |> 
  dplyr::group_by(species,ISO_A2.x) #|> 
#raster::subset(ISO_A2.x != -99) 

culicidae_N_SPP = plyr::count(culicidae_n_sp$ISO_A2.x) 
culicidae_N_SPP = dplyr::rename(culicidae_N_SPP, ISO_A2.x = x)

culicidae_N_SP = merge(culicidaejoin, culicidae_N_SPP, by= "ISO_A2.x")

## Count occ per country 

culicidae_N_OCC =  culicidae_N_SP |> 
  dplyr::group_by(ISO_A2.x) |> 
  dplyr::summarise(gbifID = n()) 

## Merge  

culicidae_final = merge(culicidae_N_SP, culicidae_N_OCC, by= "ISO_A2.x") |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

## Plot

culicidaeMap = culicidae_final  |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)  

culicidaeMap2 = cntshp |> 
  left_join(culicidaeMap, by = "ADMIN") #|> 
#mutate_at(vars(N_Species:N_Occurrences), ~replace(., is.na(.), 0))

bbox_new = st_bbox(culicidaeMap2) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

png(file="Culicidae",
    width=9, height=7, units="in", res=300)
tm_shape(culicidaeMap2,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 4,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Culicidae",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()
rm(list = ls())
.rs.restartR()

## Nematoda

library(data.table)
library(plyr)
library(dplyr)
library(raster)
library(readr)
library(rgdal)
library(dplyr)
library(sf)
library(sp)
library(tidyr)
library(tmap)
library(tidyverse)

## Read Shape

shpfile = readOGR("data/shape/buffer_hydro_global/b_join_river_country.shp")

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")


gbifraw = data.table::fread("data-raw/nematoda/0166915-220831081235567.csv") 

gbif = gbifraw |> 
  dplyr::as_tibble() |> 
  dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

xy = gbif |> 
  dplyr::select(decimalLatitude, decimalLongitude)

pts = SpatialPointsDataFrame(coords = xy, data = gbif, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

gb = st_as_sf(pts, coords = xy, st_crs(4326)) |> 
  dplyr::rename(ISO_A2 = countryCode)

shp = st_as_sf(shpfile, st_crs(4326))

good_points = st_filter(gb, shp)

#plot(st_geometry(good_points))

cntshp = st_as_sf(countryshp, st_crs(4326))

nematodajoin = st_join(cntshp, good_points) |> 
  as_tibble()

nematoda_n_sp = nematodajoin |>
  tidyr::drop_na(c(decimalLatitude, decimalLongitude, species, ISO_A2.x)) |> 
  dplyr::select(species, ISO_A2.x) |>
  dplyr::distinct(species, ISO_A2.x) |> 
  dplyr::group_by(species,ISO_A2.x) #|> 
#raster::subset(ISO_A2.x != -99) 

nematoda_N_SPP = plyr::count(nematoda_n_sp$ISO_A2.x) 
nematoda_N_SPP = dplyr::rename(nematoda_N_SPP, ISO_A2.x = x)

nematoda_N_SP = merge(nematodajoin, nematoda_N_SPP, by= "ISO_A2.x")

## Count occ per country 

nematoda_N_OCC =  nematoda_N_SP |> 
  dplyr::group_by(ISO_A2.x) |> 
  dplyr::summarise(gbifID = n()) 

## Merge  

nematoda_final = merge(nematoda_N_SP, nematoda_N_OCC, by= "ISO_A2.x") |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

## Plot

nematodaMap = nematoda_final  |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)  

nematodaMap2 = cntshp |> 
  left_join(nematodaMap, by = "ADMIN") #|> 
#mutate_at(vars(N_Species:N_Occurrences), ~replace(., is.na(.), 0))

bbox_new = st_bbox(nematodaMap2) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

png(file="Nematoda",
    width=9, height=7, units="in", res=300)
tm_shape(nematodaMap2,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 4,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Nematoda",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()
rm(list = ls())
.rs.restartR()

## Nematomorpha

library(data.table)
library(plyr)
library(dplyr)
library(raster)
library(readr)
library(rgdal)
library(dplyr)
library(sf)
library(sp)
library(tidyr)
library(tmap)
library(tidyverse)

## Read Shape

shpfile = readOGR("data/shape/buffer_hydro_global/b_join_river_country.shp")

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")


gbifraw = data.table::fread("data-raw/nematomorpha/0166934-220831081235567.csv") 

gbif = gbifraw |> 
  dplyr::as_tibble() |> 
  dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

xy = gbif |> 
  dplyr::select(decimalLatitude, decimalLongitude)

pts = SpatialPointsDataFrame(coords = xy, data = gbif, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

gb = st_as_sf(pts, coords = xy, st_crs(4326)) |> 
  dplyr::rename(ISO_A2 = countryCode)

shp = st_as_sf(shpfile, st_crs(4326))

good_points = st_filter(gb, shp)

#plot(st_geometry(good_points))

cntshp = st_as_sf(countryshp, st_crs(4326))

nematomorphajoin = st_join(cntshp, good_points) |> 
  as_tibble()

nematomorpha_n_sp = nematomorphajoin |>
  tidyr::drop_na(c(decimalLatitude, decimalLongitude, species, ISO_A2.x)) |> 
  dplyr::select(species, ISO_A2.x) |>
  dplyr::distinct(species, ISO_A2.x) |> 
  dplyr::group_by(species,ISO_A2.x) #|> 
#raster::subset(ISO_A2.x != -99) 

nematomorpha_N_SPP = plyr::count(nematomorpha_n_sp$ISO_A2.x) 
nematomorpha_N_SPP = dplyr::rename(nematomorpha_N_SPP, ISO_A2.x = x)

nematomorpha_N_SP = merge(nematomorphajoin, nematomorpha_N_SPP, by= "ISO_A2.x")

## Count occ per country 

nematomorpha_N_OCC =  nematomorpha_N_SP |> 
  dplyr::group_by(ISO_A2.x) |> 
  dplyr::summarise(gbifID = n()) 

## Merge  

nematomorpha_final = merge(nematomorpha_N_SP, nematomorpha_N_OCC, by= "ISO_A2.x") |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

## Plot

nematomorphaMap = nematomorpha_final  |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)  

nematomorphaMap2 = cntshp |> 
  left_join(nematomorphaMap, by = "ADMIN") #|> 
#mutate_at(vars(N_Species:N_Occurrences), ~replace(., is.na(.), 0))

bbox_new = st_bbox(nematomorphaMap2) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

png(file="Nematomorpha",
    width=9, height=7, units="in", res=300)
tm_shape(nematomorphaMap2,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 4,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Nematomorpha",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()
rm(list = ls())
.rs.restartR()

## Tubellaria

library(data.table)
library(plyr)
library(dplyr)
library(raster)
library(readr)
library(rgdal)
library(dplyr)
library(sf)
library(sp)
library(tidyr)
library(tmap)
library(tidyverse)

## Read Shape

shpfile = readOGR("data/shape/buffer_hydro_global/b_join_river_country.shp")

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")


gbifraw = data.table::fread("data-raw/tubellaria/0166953-220831081235567.csv") 

gbif = gbifraw |> 
  dplyr::as_tibble() |> 
  dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

xy = gbif |> 
  dplyr::select(decimalLatitude, decimalLongitude)

pts = SpatialPointsDataFrame(coords = xy, data = gbif, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

gb = st_as_sf(pts, coords = xy, st_crs(4326)) |> 
  dplyr::rename(ISO_A2 = countryCode)

shp = st_as_sf(shpfile, st_crs(4326))

good_points = st_filter(gb, shp)

#plot(st_geometry(good_points))

cntshp = st_as_sf(countryshp, st_crs(4326))

tubellariajoin = st_join(cntshp, good_points) |> 
  as_tibble()

tubellaria_n_sp = tubellariajoin |>
  tidyr::drop_na(c(decimalLatitude, decimalLongitude, species, ISO_A2.x)) |> 
  dplyr::select(species, ISO_A2.x) |>
  dplyr::distinct(species, ISO_A2.x) |> 
  dplyr::group_by(species,ISO_A2.x) #|> 
#raster::subset(ISO_A2.x != -99) 

tubellaria_N_SPP = plyr::count(tubellaria_n_sp$ISO_A2.x) 
tubellaria_N_SPP = dplyr::rename(tubellaria_N_SPP, ISO_A2.x = x)

tubellaria_N_SP = merge(tubellariajoin, tubellaria_N_SPP, by= "ISO_A2.x")

## Count occ per country 

tubellaria_N_OCC =  tubellaria_N_SP |> 
  dplyr::group_by(ISO_A2.x) |> 
  dplyr::summarise(gbifID = n()) 

## Merge  

tubellaria_final = merge(tubellaria_N_SP, tubellaria_N_OCC, by= "ISO_A2.x") |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

## Plot

tubellariaMap = tubellaria_final  |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)  

tubellariaMap2 = cntshp |> 
  left_join(tubellariaMap, by = "ADMIN") #|> 
#mutate_at(vars(N_Species:N_Occurrences), ~replace(., is.na(.), 0))

bbox_new = st_bbox(tubellariaMap2) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

png(file="Tubellaria",
    width=9, height=7, units="in", res=300)
tm_shape(tubellariaMap2,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 4,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Tubellaria",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()
rm(list = ls())
.rs.restartR()

## Cephalocaridae

library(data.table)
library(plyr)
library(dplyr)
library(raster)
library(readr)
library(rgdal)
library(dplyr)
library(sf)
library(sp)
library(tidyr)
library(tmap)
library(tidyverse)

## Read Shape

shpfile = readOGR("data/shape/buffer_hydro_global/b_join_river_country.shp")

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")


gbifraw = data.table::fread("data-raw/crustacea/cephalocaridae/0167054-220831081235567.csv") 

gbif = gbifraw |> 
  dplyr::as_tibble() |> 
  dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

xy = gbif |> 
  dplyr::select(decimalLatitude, decimalLongitude)

pts = SpatialPointsDataFrame(coords = xy, data = gbif, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

gb = st_as_sf(pts, coords = xy, st_crs(4326)) |> 
  dplyr::rename(ISO_A2 = countryCode)

shp = st_as_sf(shpfile, st_crs(4326))

good_points = st_filter(gb, shp)

#plot(st_geometry(good_points))

cntshp = st_as_sf(countryshp, st_crs(4326))

cephalocaridaejoin = st_join(cntshp, good_points) |> 
  as_tibble()

cephalocaridae_n_sp = cephalocaridaejoin |>
  tidyr::drop_na(c(decimalLatitude, decimalLongitude, species, ISO_A2.x)) |> 
  dplyr::select(species, ISO_A2.x) |>
  dplyr::distinct(species, ISO_A2.x) |> 
  dplyr::group_by(species,ISO_A2.x) #|> 
#raster::subset(ISO_A2.x != -99) 

cephalocaridae_N_SPP = plyr::count(cephalocaridae_n_sp$ISO_A2.x) 
cephalocaridae_N_SPP = dplyr::rename(cephalocaridae_N_SPP, ISO_A2.x = x)

cephalocaridae_N_SP = merge(cephalocaridaejoin, cephalocaridae_N_SPP, by= "ISO_A2.x")

## Count occ per country 

cephalocaridae_N_OCC =  cephalocaridae_N_SP |> 
  dplyr::group_by(ISO_A2.x) |> 
  dplyr::summarise(gbifID = n()) 

## Merge  

cephalocaridae_final = merge(cephalocaridae_N_SP, cephalocaridae_N_OCC, by= "ISO_A2.x") |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

## Plot

cephalocaridaeMap = cephalocaridae_final  |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)  

cephalocaridaeMap2 = cntshp |> 
  left_join(cephalocaridaeMap, by = "ADMIN") #|> 
#mutate_at(vars(N_Species:N_Occurrences), ~replace(., is.na(.), 0))

bbox_new = st_bbox(cephalocaridaeMap2) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

png(file="Cephalocaridae",
    width=9, height=7, units="in", res=300)
tm_shape(cephalocaridaeMap2,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 4,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Cephalocaridae",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()
rm(list = ls())
.rs.restartR()

## Diplostraca

library(data.table)
library(plyr)
library(dplyr)
library(raster)
library(readr)
library(rgdal)
library(dplyr)
library(sf)
library(sp)
library(tidyr)
library(tmap)
library(tidyverse)

## Read Shape

shpfile = readOGR("data/shape/buffer_hydro_global/b_join_river_country.shp")

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")


gbifraw = data.table::fread("data-raw/crustacea/diplostraca/0167039-220831081235567.csv") 

gbif = gbifraw |> 
  dplyr::as_tibble() |> 
  dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

xy = gbif |> 
  dplyr::select(decimalLatitude, decimalLongitude)

pts = SpatialPointsDataFrame(coords = xy, data = gbif, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

gb = st_as_sf(pts, coords = xy, st_crs(4326)) |> 
  dplyr::rename(ISO_A2 = countryCode)

shp = st_as_sf(shpfile, st_crs(4326))

good_points = st_filter(gb, shp)

#good_points = st_as_sf(gb)

#plot(st_geometry(good_points))

cntshp = st_as_sf(countryshp, st_crs(4326))

diplostracajoin = st_join(cntshp, good_points) |> 
  as_tibble()

diplostraca_n_sp = diplostracajoin |>
  tidyr::drop_na(c(decimalLatitude, decimalLongitude, species,ISO_A2.x)) |> 
  dplyr::select(species, ISO_A2.x) |>
  dplyr::distinct(species, ISO_A2.x) |> 
  dplyr::group_by(species,ISO_A2.x) #|> 
#raster::subset(ISO_A2.x != -99) 

diplostraca_N_SPP = plyr::count(diplostraca_n_sp$ISO_A2.x) 
diplostraca_N_SPP = dplyr::rename(diplostraca_N_SPP, ISO_A2.x = x)

diplostraca_N_SP = merge(diplostracajoin, diplostraca_N_SPP, by= "ISO_A2.x")

## Count occ per country 

diplostraca_N_OCC =  diplostraca_N_SP |> 
  dplyr::group_by(ISO_A2.x) |> 
  dplyr::summarise(gbifID = n()) 

## Merge  

diplostraca_final = merge(diplostraca_N_SP, diplostraca_N_OCC, by= "ISO_A2.x") |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

## Plot

diplostracaMap = diplostraca_final  |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)  

diplostracaMap2 = cntshp |> 
  left_join(diplostracaMap, by = "ADMIN") #|> 
#mutate_at(vars(N_Species:N_Occurrences), ~replace(., is.na(.), 0))

bbox_new = st_bbox(diplostracaMap2) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

png(file="Diplostraca",
    width=9, height=7, units="in", res=300)
tm_shape(diplostracaMap2,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 4,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Diplostraca",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()
rm(list = ls())
.rs.restartR()


## Ostracoda

library(data.table)
library(plyr)
library(dplyr)
library(raster)
library(readr)
library(rgdal)
library(dplyr)
library(sf)
library(sp)
library(tidyr)
library(tmap)
library(tidyverse)

## Read Shape

shpfile = readOGR("data/shape/buffer_hydro_global/b_join_river_country.shp")

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")


gbifraw = data.table::fread("data-raw/crustacea/ostracoda/0167045-220831081235567.csv") 

gbif = gbifraw |> 
  dplyr::as_tibble() |> 
  dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

xy = gbif |> 
  dplyr::select(decimalLatitude, decimalLongitude)

pts = SpatialPointsDataFrame(coords = xy, data = gbif, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

gb = st_as_sf(pts, coords = xy, st_crs(4326)) |> 
  dplyr::rename(ISO_A2 = countryCode)

shp = st_as_sf(shpfile, st_crs(4326))

good_points = st_filter(gb, shp)

#plot(st_geometry(good_points))

cntshp = st_as_sf(countryshp, st_crs(4326))

ostracodajoin = st_join(cntshp, good_points) |> 
  as_tibble()

ostracoda_n_sp = ostracodajoin |>
  tidyr::drop_na(c(decimalLatitude, decimalLongitude, species, ISO_A2.x)) |> 
  dplyr::select(species, ISO_A2.x) |>
  dplyr::distinct(species, ISO_A2.x) |> 
  dplyr::group_by(species,ISO_A2.x) #|> 
#raster::subset(ISO_A2.x != -99) 

ostracoda_N_SPP = plyr::count(ostracoda_n_sp$ISO_A2.x) 
ostracoda_N_SPP = dplyr::rename(ostracoda_N_SPP, ISO_A2.x = x)

ostracoda_N_SP = merge(ostracodajoin, ostracoda_N_SPP, by= "ISO_A2.x")

## Count occ per country 

ostracoda_N_OCC =  ostracoda_N_SP |> 
  dplyr::group_by(ISO_A2.x) |> 
  dplyr::summarise(gbifID = n()) 

## Merge  

ostracoda_final = merge(ostracoda_N_SP, ostracoda_N_OCC, by= "ISO_A2.x") |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

## Plot

ostracodaMap = ostracoda_final  |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)  

ostracodaMap2 = cntshp |> 
  left_join(ostracodaMap, by = "ADMIN") #|> 
#mutate_at(vars(N_Species:N_Occurrences), ~replace(., is.na(.), 0))

bbox_new = st_bbox(ostracodaMap2) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

png(file="Ostracoda",
    width=9, height=7, units="in", res=300)
tm_shape(ostracodaMap2,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 4,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Ostracoda",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()
rm(list = ls())
.rs.restartR()

## Isopoda

library(data.table)
library(plyr)
library(dplyr)
library(raster)
library(readr)
library(rgdal)
library(dplyr)
library(sf)
library(sp)
library(tidyr)
library(tmap)
library(tidyverse)

## Read Shape

shpfile = readOGR("data/shape/buffer_hydro_global/b_join_river_country.shp")

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")


gbifraw = data.table::fread("data-raw/isopoda/0167073-220831081235567.csv") 

gbif = gbifraw |> 
  dplyr::as_tibble() |> 
  dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

xy = gbif |> 
  dplyr::select(decimalLatitude, decimalLongitude)

pts = SpatialPointsDataFrame(coords = xy, data = gbif, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

gb = st_as_sf(pts, coords = xy, st_crs(4326)) |> 
  dplyr::rename(ISO_A2 = countryCode)

shp = st_as_sf(shpfile, st_crs(4326))

good_points = st_filter(gb, shp)

#plot(st_geometry(good_points))

cntshp = st_as_sf(countryshp, st_crs(4326))

isopodajoin = st_join(cntshp, good_points) |> 
  as_tibble()

isopoda_n_sp = isopodajoin |>
  tidyr::drop_na(c(decimalLatitude, decimalLongitude, species, ISO_A2.x)) |> 
  dplyr::select(species, ISO_A2.x) |>
  dplyr::distinct(species, ISO_A2.x) |> 
  dplyr::group_by(species,ISO_A2.x) #|> 
#raster::subset(ISO_A2.x != -99) 

isopoda_N_SPP = plyr::count(isopoda_n_sp$ISO_A2.x) 
isopoda_N_SPP = dplyr::rename(isopoda_N_SPP, ISO_A2.x = x)

isopoda_N_SP = merge(isopodajoin, isopoda_N_SPP, by= "ISO_A2.x")

## Count occ per country 

isopoda_N_OCC =  isopoda_N_SP |> 
  dplyr::group_by(ISO_A2.x) |> 
  dplyr::summarise(gbifID = n()) 

## Merge  

isopoda_final = merge(isopoda_N_SP, isopoda_N_OCC, by= "ISO_A2.x") |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

## Plot

isopodaMap = isopoda_final  |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)  

isopodaMap2 = cntshp |> 
  left_join(isopodaMap, by = "ADMIN") #|> 
#mutate_at(vars(N_Species:N_Occurrences), ~replace(., is.na(.), 0))

bbox_new = st_bbox(isopodaMap2) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

png(file="Isopoda",
    width=9, height=7, units="in", res=300)
tm_shape(isopodaMap2,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 4,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Isopoda",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()
rm(list = ls())
.rs.restartR()

## Maxillopoda

library(data.table)
library(plyr)
library(dplyr)
library(raster)
library(readr)
library(rgdal)
library(dplyr)
library(sf)
library(sp)
library(tidyr)
library(tmap)
library(tidyverse)

## Read Shape

shpfile = readOGR("data/shape/buffer_hydro_global/b_join_river_country.shp")

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")


gbifraw = data.table::fread("data-raw/crustacea/maxillopoda/0167051-220831081235567.csv") 

gbif = gbifraw |> 
  dplyr::as_tibble() |> 
  dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

xy = gbif |> 
  dplyr::select(decimalLatitude, decimalLongitude)

pts = SpatialPointsDataFrame(coords = xy, data = gbif, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

gb = st_as_sf(pts, coords = xy, st_crs(4326)) |> 
  dplyr::rename(ISO_A2 = countryCode)

shp = st_as_sf(shpfile, st_crs(4326))

good_points = st_filter(gb, shp)

#plot(st_geometry(good_points))

cntshp = st_as_sf(countryshp, st_crs(4326))

maxillopodajoin = st_join(cntshp, good_points) |> 
  as_tibble()

maxillopoda_n_sp = maxillopodajoin |>
  tidyr::drop_na(c(decimalLatitude, decimalLongitude, species, ISO_A2.x)) |> 
  dplyr::select(species, ISO_A2.x) |>
  dplyr::distinct(species, ISO_A2.x) |> 
  dplyr::group_by(species,ISO_A2.x) #|> 
#raster::subset(ISO_A2.x != -99) 

maxillopoda_N_SPP = plyr::count(maxillopoda_n_sp$ISO_A2.x) 
maxillopoda_N_SPP = dplyr::rename(maxillopoda_N_SPP, ISO_A2.x = x)

maxillopoda_N_SP = merge(maxillopodajoin, maxillopoda_N_SPP, by= "ISO_A2.x")

## Count occ per country 

maxillopoda_N_OCC =  maxillopoda_N_SP |> 
  dplyr::group_by(ISO_A2.x) |> 
  dplyr::summarise(gbifID = n()) 

## Merge  

maxillopoda_final = merge(maxillopoda_N_SP, maxillopoda_N_OCC, by= "ISO_A2.x") |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

## Plot

maxillopodaMap = maxillopoda_final  |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)  

maxillopodaMap2 = cntshp |> 
  left_join(maxillopodaMap, by = "ADMIN") #|> 
#mutate_at(vars(N_Species:N_Occurrences), ~replace(., is.na(.), 0))

bbox_new = st_bbox(maxillopodaMap2) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

png(file="Maxillopoda",
    width=9, height=7, units="in", res=300)
tm_shape(maxillopodaMap2,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 4,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Maxillopoda",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()
rm(list = ls())
.rs.restartR()

## Remipedia

library(data.table)
library(plyr)
library(dplyr)
library(raster)
library(readr)
library(rgdal)
library(dplyr)
library(sf)
library(sp)
library(tidyr)
library(tmap)
library(tidyverse)

## Read Shape

shpfile = readOGR("data/shape/buffer_hydro_global/b_join_river_country.shp")

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")


gbifraw = data.table::fread("data-raw/crustacea/remipedia/0167056-220831081235567.csv") 

gbif = gbifraw |> 
  dplyr::as_tibble() |> 
  dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

xy = gbif |> 
  dplyr::select(decimalLatitude, decimalLongitude)

pts = SpatialPointsDataFrame(coords = xy, data = gbif, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

gb = st_as_sf(pts, coords = xy, st_crs(4326)) |> 
  dplyr::rename(ISO_A2 = countryCode)

shp = st_as_sf(shpfile, st_crs(4326))

good_points = st_filter(gb, shp)

#plot(st_geometry(good_points))

cntshp = st_as_sf(countryshp, st_crs(4326))

remipediajoin = st_join(cntshp, good_points) |> 
  as_tibble()

remipedia_n_sp = remipediajoin |>
  tidyr::drop_na(c(decimalLatitude, decimalLongitude, species, ISO_A2.x)) |> 
  dplyr::select(species, ISO_A2.x) |>
  dplyr::distinct(species, ISO_A2.x) |> 
  dplyr::group_by(species,ISO_A2.x) #|> 
#raster::subset(ISO_A2.x != -99) 

remipedia_N_SPP = plyr::count(remipedia_n_sp$ISO_A2.x) 
remipedia_N_SPP = dplyr::rename(remipedia_N_SPP, ISO_A2.x = x)

remipedia_N_SP = merge(remipediajoin, remipedia_N_SPP, by= "ISO_A2.x")

## Count occ per country 

remipedia_N_OCC =  remipedia_N_SP |> 
  dplyr::group_by(ISO_A2.x) |> 
  dplyr::summarise(gbifID = n()) 

## Merge  

remipedia_final = merge(remipedia_N_SP, remipedia_N_OCC, by= "ISO_A2.x") |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

## Plot

remipediaMap = remipedia_final  |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)  

remipediaMap2 = cntshp |> 
  left_join(remipediaMap, by = "ADMIN") #|> 
#mutate_at(vars(N_Species:N_Occurrences), ~replace(., is.na(.), 0))

bbox_new = st_bbox(remipediaMap2) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

png(file="Remipedia",
    width=9, height=7, units="in", res=300)
tm_shape(remipediaMap2,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 4,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Remipedia",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()
rm(list = ls())
.rs.restartR()

## Megaloptera

library(data.table)
library(plyr)
library(dplyr)
library(raster)
library(readr)
library(rgdal)
library(dplyr)
library(sf)
library(sp)
library(tidyr)
library(tmap)
library(tidyverse)

## Read Shape

shpfile = readOGR("data/shape/buffer_hydro_global/b_join_river_country.shp")

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")


gbifraw = data.table::fread("data-raw/megaloptera/0167088-220831081235567.csv") 

gbif = gbifraw |> 
  dplyr::as_tibble() |> 
  dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

xy = gbif |> 
  dplyr::select(decimalLatitude, decimalLongitude)

pts = SpatialPointsDataFrame(coords = xy, data = gbif, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

gb = st_as_sf(pts, coords = xy, st_crs(4326)) |> 
  dplyr::rename(ISO_A2 = countryCode)

shp = st_as_sf(shpfile, st_crs(4326))

good_points = st_filter(gb, shp)

#plot(st_geometry(good_points))

cntshp = st_as_sf(countryshp, st_crs(4326))

megalopterajoin = st_join(cntshp, good_points) |> 
  as_tibble()

megaloptera_n_sp = megalopterajoin |>
  tidyr::drop_na(c(decimalLatitude, decimalLongitude, species, ISO_A2.x)) |> 
  dplyr::select(species, ISO_A2.x) |>
  dplyr::distinct(species, ISO_A2.x) |> 
  dplyr::group_by(species,ISO_A2.x) #|> 
#raster::subset(ISO_A2.x != -99) 

megaloptera_N_SPP = plyr::count(megaloptera_n_sp$ISO_A2.x) 
megaloptera_N_SPP = dplyr::rename(megaloptera_N_SPP, ISO_A2.x = x)

megaloptera_N_SP = merge(megalopterajoin, megaloptera_N_SPP, by= "ISO_A2.x")

## Count occ per country 

megaloptera_N_OCC =  megaloptera_N_SP |> 
  dplyr::group_by(ISO_A2.x) |> 
  dplyr::summarise(gbifID = n()) 

## Merge  

megaloptera_final = merge(megaloptera_N_SP, megaloptera_N_OCC, by= "ISO_A2.x") |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

## Plot

megalopteraMap = megaloptera_final  |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)  

megalopteraMap2 = cntshp |> 
  left_join(megalopteraMap, by = "ADMIN") #|> 
#mutate_at(vars(N_Species:N_Occurrences), ~replace(., is.na(.), 0))

bbox_new = st_bbox(megalopteraMap2) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

png(file="Megaloptera",
    width=9, height=7, units="in", res=300)
tm_shape(megalopteraMap2,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 4,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Megaloptera",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()
rm(list = ls())
.rs.restartR()


## Trichoptera

library(data.table)
library(plyr)
library(dplyr)
library(raster)
library(readr)
library(rgdal)
library(dplyr)
library(sf)
library(sp)
library(tidyr)
library(tmap)
library(tidyverse)

## Read Shape

shpfile = readOGR("data/shape/buffer_hydro_global/b_join_river_country.shp")

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")


gbifraw = data.table::fread("data-raw/trichoptera/0167160-220831081235567.csv") 

gbif = gbifraw |> 
  dplyr::as_tibble() |> 
  dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

xy = gbif |> 
  dplyr::select(decimalLatitude, decimalLongitude)

pts = SpatialPointsDataFrame(coords = xy, data = gbif, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

gb = st_as_sf(pts, coords = xy, st_crs(4326)) |> 
  dplyr::rename(ISO_A2 = countryCode)

shp = st_as_sf(shpfile, st_crs(4326))

good_points = st_filter(gb, shp)

#plot(st_geometry(good_points))

cntshp = st_as_sf(countryshp, st_crs(4326))

trichopterajoin = st_join(cntshp, good_points) |> 
  as_tibble()

trichoptera_n_sp = trichopterajoin |>
  tidyr::drop_na(c(decimalLatitude, decimalLongitude, species, ISO_A2.x)) |> 
  dplyr::select(species, ISO_A2.x) |>
  dplyr::distinct(species, ISO_A2.x) |> 
  dplyr::group_by(species,ISO_A2.x) #|> 
#raster::subset(ISO_A2.x != -99) 

trichoptera_N_SPP = plyr::count(trichoptera_n_sp$ISO_A2.x) 
trichoptera_N_SPP = dplyr::rename(trichoptera_N_SPP, ISO_A2.x = x)

trichoptera_N_SP = merge(trichopterajoin, trichoptera_N_SPP, by= "ISO_A2.x")

## Count occ per country 

trichoptera_N_OCC =  trichoptera_N_SP |> 
  dplyr::group_by(ISO_A2.x) |> 
  dplyr::summarise(gbifID = n()) 

## Merge  

trichoptera_final = merge(trichoptera_N_SP, trichoptera_N_OCC, by= "ISO_A2.x") |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

## Plot

trichopteraMap = trichoptera_final  |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)  

trichopteraMap2 = cntshp |> 
  left_join(trichopteraMap, by = "ADMIN") #|> 
#mutate_at(vars(N_Species:N_Occurrences), ~replace(., is.na(.), 0))

bbox_new = st_bbox(trichopteraMap2) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

png(file="Trichoptera",
    width=9, height=7, units="in", res=300)
tm_shape(trichopteraMap2,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 4,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Trichoptera",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()
rm(list = ls())
.rs.restartR()

## Hemiptera

library(data.table)
library(plyr)
library(dplyr)
library(raster)
library(readr)
library(rgdal)
library(dplyr)
library(sf)
library(sp)
library(tidyr)
library(tmap)
library(tidyverse)

## Read Shape

shpfile = readOGR("data/shape/buffer_hydro_global/b_join_river_country.shp")

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")


gbifraw = data.table::fread("data-raw/hemiptera/0167098-220831081235567.csv") 

gbif = gbifraw |> 
  dplyr::as_tibble() |> 
  dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

xy = gbif |> 
  dplyr::select(decimalLatitude, decimalLongitude)

pts = SpatialPointsDataFrame(coords = xy, data = gbif, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

gb = st_as_sf(pts, coords = xy, st_crs(4326)) |> 
  dplyr::rename(ISO_A2 = countryCode)

shp = st_as_sf(shpfile, st_crs(4326))

good_points = st_filter(gb, shp)

#plot(st_geometry(good_points))

cntshp = st_as_sf(countryshp, st_crs(4326))

hemipterajoin = st_join(cntshp, good_points) |> 
  as_tibble()

hemiptera_n_sp = hemipterajoin |>
  tidyr::drop_na(c(decimalLatitude, decimalLongitude, species, ISO_A2.x)) |> 
  dplyr::select(species, ISO_A2.x) |>
  dplyr::distinct(species, ISO_A2.x) |> 
  dplyr::group_by(species,ISO_A2.x) #|> 
#raster::subset(ISO_A2.x != -99) 

hemiptera_N_SPP = plyr::count(hemiptera_n_sp$ISO_A2.x) 
hemiptera_N_SPP = dplyr::rename(hemiptera_N_SPP, ISO_A2.x = x)

hemiptera_N_SP = merge(hemipterajoin, hemiptera_N_SPP, by= "ISO_A2.x")

## Count occ per country 

hemiptera_N_OCC =  hemiptera_N_SP |> 
  dplyr::group_by(ISO_A2.x) |> 
  dplyr::summarise(gbifID = n()) 

## Merge  

hemiptera_final = merge(hemiptera_N_SP, hemiptera_N_OCC, by= "ISO_A2.x") |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

## Plot

hemipteraMap = hemiptera_final  |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)  

hemipteraMap2 = cntshp |> 
  left_join(hemipteraMap, by = "ADMIN") #|> 
#mutate_at(vars(N_Species:N_Occurrences), ~replace(., is.na(.), 0))

bbox_new = st_bbox(hemipteraMap2) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

png(file="Hemiptera",
    width=9, height=7, units="in", res=300)
tm_shape(hemipteraMap2,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 4,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Hemiptera",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()
rm(list = ls())
.rs.restartR()

## Plecoptera

library(data.table)
library(plyr)
library(dplyr)
library(raster)
library(readr)
library(rgdal)
library(dplyr)
library(sf)
library(sp)
library(tidyr)
library(tmap)
library(tidyverse)

## Read Shape

shpfile = readOGR("data/shape/buffer_hydro_global/b_join_river_country.shp")

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")


gbifraw = data.table::fread("data-raw/plecoptera/0167161-220831081235567.csv") 

gbif = gbifraw |> 
  dplyr::as_tibble() |> 
  dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

xy = gbif |> 
  dplyr::select(decimalLatitude, decimalLongitude)

pts = SpatialPointsDataFrame(coords = xy, data = gbif, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

gb = st_as_sf(pts, coords = xy, st_crs(4326)) |> 
  dplyr::rename(ISO_A2 = countryCode)

shp = st_as_sf(shpfile, st_crs(4326))

good_points = st_filter(gb, shp)

#plot(st_geometry(good_points))

cntshp = st_as_sf(countryshp, st_crs(4326))

plecopterajoin = st_join(cntshp, good_points) |> 
  as_tibble()

plecoptera_n_sp = plecopterajoin |>
  tidyr::drop_na(c(decimalLatitude, decimalLongitude, species, ISO_A2.x)) |> 
  dplyr::select(species, ISO_A2.x) |>
  dplyr::distinct(species, ISO_A2.x) |> 
  dplyr::group_by(species,ISO_A2.x) #|> 
#raster::subset(ISO_A2.x != -99) 

plecoptera_N_SPP = plyr::count(plecoptera_n_sp$ISO_A2.x) 
plecoptera_N_SPP = dplyr::rename(plecoptera_N_SPP, ISO_A2.x = x)

plecoptera_N_SP = merge(plecopterajoin, plecoptera_N_SPP, by= "ISO_A2.x")

## Count occ per country 

plecoptera_N_OCC =  plecoptera_N_SP |> 
  dplyr::group_by(ISO_A2.x) |> 
  dplyr::summarise(gbifID = n()) 

## Merge  

plecoptera_final = merge(plecoptera_N_SP, plecoptera_N_OCC, by= "ISO_A2.x") |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

## Plot

plecopteraMap = plecoptera_final  |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)  

plecopteraMap2 = cntshp |> 
  left_join(plecopteraMap, by = "ADMIN") #|> 
#mutate_at(vars(N_Species:N_Occurrences), ~replace(., is.na(.), 0))

bbox_new = st_bbox(plecopteraMap2) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

png(file="Plecoptera",
    width=9, height=7, units="in", res=300)
tm_shape(plecopteraMap2,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 4,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Plecoptera",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()
rm(list = ls())
.rs.restartR()


## Ephemeroptera

library(data.table)
library(plyr)
library(dplyr)
library(raster)
library(readr)
library(rgdal)
library(dplyr)
library(sf)
library(sp)
library(tidyr)
library(tmap)
library(tidyverse)

## Read Shape

shpfile = readOGR("data/shape/buffer_hydro_global/b_join_river_country.shp")

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")


gbifraw = data.table::fread("data-raw/ephemeroptera/0167200-220831081235567.csv") 

gbif = gbifraw |> 
  dplyr::as_tibble() |> 
  dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

xy = gbif |> 
  dplyr::select(decimalLatitude, decimalLongitude)

pts = SpatialPointsDataFrame(coords = xy, data = gbif, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

gb = st_as_sf(pts, coords = xy, st_crs(4326)) |> 
  dplyr::rename(ISO_A2 = countryCode)

shp = st_as_sf(shpfile, st_crs(4326))

good_points = st_filter(gb, shp)

#plot(st_geometry(good_points))

cntshp = st_as_sf(countryshp, st_crs(4326))

ephemeropterajoin = st_join(cntshp, good_points) |> 
  as_tibble()

ephemeroptera_n_sp = ephemeropterajoin |>
  tidyr::drop_na(c(decimalLatitude, decimalLongitude, species, ISO_A2.x)) |> 
  dplyr::select(species, ISO_A2.x) |>
  dplyr::distinct(species, ISO_A2.x) |> 
  dplyr::group_by(species,ISO_A2.x) #|> 
#raster::subset(ISO_A2.x != -99) 

ephemeroptera_N_SPP = plyr::count(ephemeroptera_n_sp$ISO_A2.x) 
ephemeroptera_N_SPP = dplyr::rename(ephemeroptera_N_SPP, ISO_A2.x = x)

ephemeroptera_N_SP = merge(ephemeropterajoin, ephemeroptera_N_SPP, by= "ISO_A2.x")

## Count occ per country 

ephemeroptera_N_OCC =  ephemeroptera_N_SP |> 
  dplyr::group_by(ISO_A2.x) |> 
  dplyr::summarise(gbifID = n()) 

## Merge  

ephemeroptera_final = merge(ephemeroptera_N_SP, ephemeroptera_N_OCC, by= "ISO_A2.x") |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

## Plot

ephemeropteraMap = ephemeroptera_final  |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)  

ephemeropteraMap2 = cntshp |> 
  left_join(ephemeropteraMap, by = "ADMIN") #|> 
#mutate_at(vars(N_Species:N_Occurrences), ~replace(., is.na(.), 0))

bbox_new = st_bbox(ephemeropteraMap2) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

png(file="Ephemeroptera",
    width=9, height=7, units="in", res=300)
tm_shape(ephemeropteraMap2,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 4,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Ephemeroptera",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()
rm(list = ls())
.rs.restartR()


## Crocodylia

library(data.table)
library(plyr)
library(dplyr)
library(raster)
library(readr)
library(rgdal)
library(dplyr)
library(sf)
library(sp)
library(tidyr)
library(tmap)
library(tidyverse)

## Read Shape

shpfile = readOGR("data/shape/buffer_hydro_global/b_join_river_country.shp")

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")


gbifraw = data.table::fread("data-raw/crocodylia/0167202-220831081235567.csv") 

gbif = gbifraw |> 
  dplyr::as_tibble() |> 
  dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

xy = gbif |> 
  dplyr::select(decimalLatitude, decimalLongitude)

pts = SpatialPointsDataFrame(coords = xy, data = gbif, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

gb = st_as_sf(pts, coords = xy, st_crs(4326)) |> 
  dplyr::rename(ISO_A2 = countryCode)

shp = st_as_sf(shpfile, st_crs(4326))

good_points = st_filter(gb, shp)

#plot(st_geometry(good_points))

cntshp = st_as_sf(countryshp, st_crs(4326))

crocodyliajoin = st_join(cntshp, good_points) |> 
  as_tibble()

crocodylia_n_sp = crocodyliajoin |>
  tidyr::drop_na(c(decimalLatitude, decimalLongitude, species, ISO_A2.x)) |> 
  dplyr::select(species, ISO_A2.x) |>
  dplyr::distinct(species, ISO_A2.x) |> 
  dplyr::group_by(species,ISO_A2.x) #|> 
#raster::subset(ISO_A2.x != -99) 

crocodylia_N_SPP = plyr::count(crocodylia_n_sp$ISO_A2.x) 
crocodylia_N_SPP = dplyr::rename(crocodylia_N_SPP, ISO_A2.x = x)

crocodylia_N_SP = merge(crocodyliajoin, crocodylia_N_SPP, by= "ISO_A2.x")

## Count occ per country 

crocodylia_N_OCC =  crocodylia_N_SP |> 
  dplyr::group_by(ISO_A2.x) |> 
  dplyr::summarise(gbifID = n()) 

## Merge  

crocodylia_final = merge(crocodylia_N_SP, crocodylia_N_OCC, by= "ISO_A2.x") |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

## Plot

crocodyliaMap = crocodylia_final  |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)  

crocodyliaMap2 = cntshp |> 
  left_join(crocodyliaMap, by = "ADMIN") #|> 
#mutate_at(vars(N_Species:N_Occurrences), ~replace(., is.na(.), 0))

bbox_new = st_bbox(crocodyliaMap2) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

png(file="Crocodylia",
    width=9, height=7, units="in", res=300)
tm_shape(crocodyliaMap2,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 4,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Crocodylia",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()
rm(list = ls())
.rs.restartR()

## Testudines

library(data.table)
library(plyr)
library(dplyr)
library(raster)
library(readr)
library(rgdal)
library(dplyr)
library(sf)
library(sp)
library(tidyr)
library(tmap)
library(tidyverse)

## Read Shape

shpfile = readOGR("data/shape/buffer_hydro_global/b_join_river_country.shp")

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")


gbifraw = data.table::fread("data-raw/testudines/0167204-220831081235567.csv") 

gbif = gbifraw |> 
  dplyr::as_tibble() |> 
  dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

xy = gbif |> 
  dplyr::select(decimalLatitude, decimalLongitude)

pts = SpatialPointsDataFrame(coords = xy, data = gbif, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

gb = st_as_sf(pts, coords = xy, st_crs(4326)) |> 
  dplyr::rename(ISO_A2 = countryCode)

shp = st_as_sf(shpfile, st_crs(4326))

good_points = st_filter(gb, shp)

#plot(st_geometry(good_points))

cntshp = st_as_sf(countryshp, st_crs(4326))

testudinesjoin = st_join(cntshp, good_points) |> 
  as_tibble()

testudines_n_sp = testudinesjoin |>
  tidyr::drop_na(c(decimalLatitude, decimalLongitude, species, ISO_A2.x)) |> 
  dplyr::select(species, ISO_A2.x) |>
  dplyr::distinct(species, ISO_A2.x) |> 
  dplyr::group_by(species,ISO_A2.x) #|> 
#raster::subset(ISO_A2.x != -99) 

testudines_N_SPP = plyr::count(testudines_n_sp$ISO_A2.x) 
testudines_N_SPP = dplyr::rename(testudines_N_SPP, ISO_A2.x = x)

testudines_N_SP = merge(testudinesjoin, testudines_N_SPP, by= "ISO_A2.x")

## Count occ per country 

testudines_N_OCC =  testudines_N_SP |> 
  dplyr::group_by(ISO_A2.x) |> 
  dplyr::summarise(gbifID = n()) 

## Merge  

testudines_final = merge(testudines_N_SP, testudines_N_OCC, by= "ISO_A2.x") |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

## Plot

testudinesMap = testudines_final  |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)  

testudinesMap2 = cntshp |> 
  left_join(testudinesMap, by = "ADMIN") #|> 
#mutate_at(vars(N_Species:N_Occurrences), ~replace(., is.na(.), 0))

bbox_new = st_bbox(testudinesMap2) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

png(file="Testudines",
    width=9, height=7, units="in", res=300)
tm_shape(testudinesMap2,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 4,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Testudines",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()
rm(list = ls())
.rs.restartR()

## Euglenozoa

library(data.table)
library(plyr)
library(dplyr)
library(raster)
library(readr)
library(rgdal)
library(dplyr)
library(sf)
library(sp)
library(tidyr)
library(tmap)
library(tidyverse)

## Read Shape

shpfile = readOGR("data/shape/buffer_hydro_global/b_join_river_country.shp")

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")


gbifraw = data.table::fread("data-raw/euglenozoa/0168078-220831081235567.csv") 

gbif = gbifraw |> 
  dplyr::as_tibble() |> 
  dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

xy = gbif |> 
  dplyr::select(decimalLatitude, decimalLongitude)

pts = SpatialPointsDataFrame(coords = xy, data = gbif, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

gb = st_as_sf(pts, coords = xy, st_crs(4326)) |> 
  dplyr::rename(ISO_A2 = countryCode)

shp = st_as_sf(shpfile, st_crs(4326))

good_points = st_filter(gb, shp)

#plot(st_geometry(good_points))

cntshp = st_as_sf(countryshp, st_crs(4326))  
 
euglenozoajoin = st_join(cntshp, good_points) |> 
  as_tibble() 

euglenozoa_n_sp = euglenozoajoin |>
  tidyr::drop_na(c(decimalLatitude, decimalLongitude, species, ISO_A2.x)) |> 
  dplyr::select(species, ISO_A2.x) |>
  dplyr::distinct(species, ISO_A2.x) |> 
  dplyr::group_by(species,ISO_A2.x) #|> 
  #raster::subset(ISO_A2.x != -99) 

euglenozoa_N_SPP = plyr::count(euglenozoa_n_sp$ISO_A2.x) 
euglenozoa_N_SPP = dplyr::rename(euglenozoa_N_SPP, ISO_A2.x = x)

euglenozoa_N_SP = merge(euglenozoajoin, euglenozoa_N_SPP, by= "ISO_A2.x")

## Count occ per country 

euglenozoa_N_OCC =  euglenozoa_N_SP |> 
  dplyr::group_by(ISO_A2.x) |> 
  dplyr::summarise(gbifID = n()) 

## Merge  

euglenozoa_final = merge(euglenozoa_N_SP, euglenozoa_N_OCC, by= "ISO_A2.x")  |> 
  #dplyr::right_join(cntshp, by= "ADMIN") |> 
  #replace(is.na(.), 0) |> 
  #mutate_at(vars(freq:gbifID.y), ~replace(., is.na(.), 0)) 
  #mutate_if(is.integer, ~replace(., is.na(.), 0)) |> 
  #mutate_if(is.numeric, ~replace(., is.na(.), 0)) |> 
  #mutate_if(is.character, ~replace(., is.na(.), 0)) |> 
  #mutate_if(is.numeric, ~replace(., -99, 0))  |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)


## Plot N species per country

euglenozoaMap = euglenozoa_final  |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)  
 
euglenozoaMap2 = cntshp |> 
  left_join(euglenozoaMap, by = "ADMIN") #|> 
  #mutate_at(vars(N_Species:N_Occurrences), ~replace(., is.na(.), 0))

bbox_new = st_bbox(euglenozoaMap2) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

png(file="Euglenozoa",
    width=9, height=7, units="in", res=300)
tm_shape(euglenozoaMap2,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 4,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Euglenozoa",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()
rm(list = ls())
.rs.restartR()