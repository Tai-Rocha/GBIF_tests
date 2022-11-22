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

bivalvia_final = merge(bivalvia_N_SP, bivalvia_N_OCC, by= "ISO_A2.x")

#st_write(bivalvia_final, "results/bivalvia.shp")

## Plot N species per country

bivalviaMap = st_as_sf(bivalvia_final) |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

png(file="Bivalvia.png",
    width=9, height=7, units="in", res=300)
tm_shape(bivalviaMap) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 5,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("left", "bottom"),
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

porifera_final = merge(porifera_N_SP, porifera_N_OCC, by= "ISO_A2.x")

## Plot N species per country

poriferaMap = st_as_sf(porifera_final) |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

png(file="Porifera.png",
    width=9, height=7, units="in", res=300)
tm_shape(poriferaMap) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 5,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("left", "bottom"),
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

bryozoa_final = merge(bryozoa_N_SP, bryozoa_N_OCC, by= "ISO_A2.x")

## Plot N species per country

bryozoaMap = st_as_sf(bryozoa_final) |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

png(file="Bryozoa.png",
    width=9, height=7, units="in", res=300)
tm_shape(bryozoaMap) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 5,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("left", "bottom"),
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

cnidaria_final = merge(cnidaria_N_SP, cnidaria_N_OCC, by= "ISO_A2.x")

## Plot N species per country

cnidariaMap = st_as_sf(cnidaria_final) |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

png(file="Cnidaria.png",
    width=9, height=7, units="in", res=300)
tm_shape(cnidariaMap) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 5,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("left", "bottom"),
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

simuliidae_final = merge(simuliidae_N_SP, simuliidae_N_OCC, by= "ISO_A2.x")

## Plot N species per country

simuliidaeMap = st_as_sf(simuliidae_final) |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

png(file="Simuliidae.png",
    width=9, height=7, units="in", res=300)
tm_shape(simuliidaeMap) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 5,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("left", "bottom"),
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

culicidae_final = merge(culicidae_N_SP, culicidae_N_OCC, by= "ISO_A2.x")

## Plot N species per country

culicidaeMap = st_as_sf(culicidae_final) |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

png(file="Culicidae.png",
    width=9, height=7, units="in", res=300)
tm_shape(culicidaeMap) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 5,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("left", "bottom"),
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

nematoda_final = merge(nematoda_N_SP, nematoda_N_OCC, by= "ISO_A2.x")

## Plot N species per country

nematodaMap = st_as_sf(nematoda_final) |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

png(file="Nematoda.png",
    width=9, height=7, units="in", res=300)
tm_shape(nematodaMap) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 5,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("left", "bottom"),
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

nematomorpha_final = merge(nematomorpha_N_SP, nematomorpha_N_OCC, by= "ISO_A2.x")

## Plot N species per country

nematomorphaMap = st_as_sf(nematomorpha_final) |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

png(file="Nematomorpha.png",
    width=9, height=7, units="in", res=300)
tm_shape(nematomorphaMap) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 5,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("left", "bottom"),
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

tubellaria_final = merge(tubellaria_N_SP, tubellaria_N_OCC, by= "ISO_A2.x")

## Plot N species per country

tubellariaMap = st_as_sf(tubellaria_final) |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

png(file="Tubellaria.png",
    width=9, height=7, units="in", res=300)
tm_shape(tubellariaMap) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 5,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("left", "bottom"),
            main.title = "Tubellaria",
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

ostracoda_final = merge(ostracoda_N_SP, ostracoda_N_OCC, by= "ISO_A2.x")

## Plot N species per country

ostracodaMap = st_as_sf(ostracoda_final) |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

png(file="Ostracoda",
    width=9, height=7, units="in", res=300)
tm_shape(ostracodaMap) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 5,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("left", "bottom"),
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

isopoda_final = merge(isopoda_N_SP, isopoda_N_OCC, by= "ISO_A2.x")

## Plot N species per country

isopodaMap = st_as_sf(isopoda_final) |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

png(file="Isopoda",
    width=9, height=7, units="in", res=300)
tm_shape(isopodaMap) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 5,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("left", "bottom"),
            main.title = "Isopoda",
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

megaloptera_final = merge(megaloptera_N_SP, megaloptera_N_OCC, by= "ISO_A2.x")

## Plot N species per country

megalopteraMap = st_as_sf(megaloptera_final) |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

png(file="Megaloptera",
    width=9, height=7, units="in", res=300)
tm_shape(megalopteraMap) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 5,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("left", "bottom"),
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

trichoptera_final = merge(trichoptera_N_SP, trichoptera_N_OCC, by= "ISO_A2.x")

## Plot N species per country

trichopteraMap = st_as_sf(trichoptera_final) |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

png(file="Trichoptera",
    width=9, height=7, units="in", res=300)
tm_shape(trichopteraMap) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 5,
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

hemiptera_final = merge(hemiptera_N_SP, hemiptera_N_OCC, by= "ISO_A2.x")

## Plot N species per country

hemipteraMap = st_as_sf(hemiptera_final) |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

png(file="Hemiptera",
    width=9, height=7, units="in", res=300)
tm_shape(hemipteraMap) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 5,
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

plecoptera_final = merge(plecoptera_N_SP, plecoptera_N_OCC, by= "ISO_A2.x")

## Plot N species per country

plecopteraMap = st_as_sf(plecoptera_final) |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

png(file="Plecoptera",
    width=9, height=7, units="in", res=300)
tm_shape(plecopteraMap) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 5,
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

ephemeroptera_final = merge(ephemeroptera_N_SP, ephemeroptera_N_OCC, by= "ISO_A2.x")

## Plot N species per country

ephemeropteraMap = st_as_sf(ephemeroptera_final) |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

png(file="Ephemeroptera",
    width=9, height=7, units="in", res=300)
tm_shape(ephemeropteraMap) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 5,
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

crocodylia_final = merge(crocodylia_N_SP, crocodylia_N_OCC, by= "ISO_A2.x")

## Plot N species per country

crocodyliaMap = st_as_sf(crocodylia_final) |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

png(file="Crocodylia",
    width=9, height=7, units="in", res=300)
tm_shape(crocodyliaMap) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 5,
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

testudines_final = merge(testudines_N_SP, testudines_N_OCC, by= "ISO_A2.x")

## Plot N species per country

testudinesMap = st_as_sf(testudines_final) |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

png(file="Testudines",
    width=9, height=7, units="in", res=300)
tm_shape(testudinesMap) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 5,
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

euglenozoa_final = merge(euglenozoa_N_SP, euglenozoa_N_OCC, by= "ISO_A2.x")

## Plot N species per country

euglenozoaMap = st_as_sf(euglenozoa_final) |> 
  dplyr::rename(N_Species = gbifID.y) |> 
  dplyr::rename(N_Occurrences = freq)

png(file="Euglenozoa",
    width=9, height=7, units="in", res=300)
tm_shape(euglenozoaMap) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          n = 5,
          style = "jenks",
          palette = 'Blues') +
  tm_layout(legend.position = c("left", "bottom"),
            main.title = "Euglenozoa",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()
rm(list = ls())
.rs.restartR()