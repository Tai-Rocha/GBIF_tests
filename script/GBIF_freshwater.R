## Odonata 

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

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  st_as_sf() |> 
  dplyr::rename(countryCod =ISO_A2)

gbif_aquac = st_read("data/shape/taxon_in_aqua/odonata.shp") 

## Count SPs per country 

odonata_N_sp = gbif_aquac |>
  dplyr::distinct(species, countryCod) |>
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(species = n()) |> 
  dplyr::rename(N_Species = species)


## Count occ per country 

odonata_N_OC =  gbif_aquac |> 
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(gbifID = n()) |> 
  dplyr::rename(N_Occurrences = gbifID)

## Merge 1 

odonata_SP_OC = merge(odonata_N_sp, odonata_N_OC, by= "countryCod") |> 
  st_as_sf()


## Merge 2
countryDF = as.data.frame(countryshp)

odonata_final = dplyr::left_join(countryDF, odonata_SP_OC, by= "countryCod" ) 


## Plot

odonataMap = odonata_final  |> 
  st_as_sf() |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)
  

bbox_new = st_bbox(odonataMap) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

Breaks <- c(0, 100, 200, 600, 1000)
Labels <- c("0 - 100", "100 - 200", "200 - 600", "600- >1000")
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")


png(file="Odonata",
    width=9, height=7, units="in", res=300)
tm_shape(odonataMap,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          #convert2density = TRUE,
          palette = MyPalette,
          breaks = Breaks, labels = Labels) +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Odonata",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()

rm(list = ls())
.rs.restartR()

## Rotifera 

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

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  st_as_sf() |> 
  dplyr::rename(countryCod =ISO_A2)

gbif_aquac = st_read("data/shape/taxon_in_aqua/rotifera.shp") 

## Count SPs per country 

rotifera_N_sp = gbif_aquac |>
  dplyr::distinct(species, countryCod) |>
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(species = n()) |> 
  dplyr::rename(N_Species = species)


## Count occ per country 

rotifera_N_OC =  gbif_aquac |> 
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(gbifID = n()) |> 
  dplyr::rename(N_Occurrences = gbifID)

## Merge 1 

rotifera_SP_OC = merge(rotifera_N_sp, rotifera_N_OC, by= "countryCod") |> 
  st_as_sf()


## Merge 2
countryDF = as.data.frame(countryshp)

rotifera_final = dplyr::left_join(countryDF, rotifera_SP_OC, by= "countryCod" ) 


## Plot

rotiferaMap = rotifera_final  |> 
  st_as_sf() |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)


bbox_new = st_bbox(rotiferaMap) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

Breaks <- c(0, 50, 100, 200, 1000)
Labels <- c("0 - 50", "50 - 100", "100 - 200", "200- >1000")
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")


png(file="Rotifera",
    width=9, height=7, units="in", res=300)
tm_shape(rotiferaMap,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          #convert2density = TRUE,
          palette = MyPalette,
          breaks = Breaks, labels = Labels) +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Rotifera",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()

rm(list = ls())
.rs.restartR()

## bivalvia 

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

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  st_as_sf() |> 
  dplyr::rename(countryCod =ISO_A2)

gbif_aquac = st_read("data/shape/taxon_in_aqua/bivalvia.shp") 

## Count SPs per country 

bivalvia_N_sp = gbif_aquac |>
  dplyr::distinct(species, countryCod) |>
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(species = n()) |> 
  dplyr::rename(N_Species = species)


## Count occ per country 

bivalvia_N_OC =  gbif_aquac |> 
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(gbifID = n()) |> 
  dplyr::rename(N_Occurrences = gbifID)

## Merge 1 

bivalvia_SP_OC = merge(bivalvia_N_sp, bivalvia_N_OC, by= "countryCod") |> 
  st_as_sf()


## Merge 2
countryDF = as.data.frame(countryshp)

bivalvia_final = dplyr::left_join(countryDF, bivalvia_SP_OC, by= "countryCod" ) 


## Plot

bivalviaMap = bivalvia_final  |> 
  st_as_sf() |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)


bbox_new = st_bbox(bivalviaMap) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

Breaks <- c(0, 100, 200, 600, 1000)
Labels <- c("0 - 100", "100 - 200", "200 - 600", "600- >1000")
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")


png(file="Bivalvia",
    width=9, height=7, units="in", res=300)
tm_shape(bivalviaMap,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          #convert2density = TRUE,
          palette = MyPalette,
          breaks = Breaks, labels = Labels) +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Bivalvia",
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

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  st_as_sf() |> 
  dplyr::rename(countryCod =ISO_A2)

gbif_aquac = st_read("data/shape/taxon_in_aqua/bryozoa.shp") 

## Count SPs per country 

bryozoa_N_sp = gbif_aquac |>
  dplyr::distinct(species, countryCod) |>
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(species = n()) |> 
  dplyr::rename(N_Species = species)


## Count occ per country 

bryozoa_N_OC =  gbif_aquac |> 
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(gbifID = n()) |> 
  dplyr::rename(N_Occurrences = gbifID)

## Merge 1 

bryozoa_SP_OC = merge(bryozoa_N_sp, bryozoa_N_OC, by= "countryCod") |> 
  st_as_sf()


## Merge 2
countryDF = as.data.frame(countryshp)

bryozoa_final = dplyr::left_join(countryDF, bryozoa_SP_OC, by= "countryCod" ) 


## Plot

bryozoaMap = bryozoa_final  |> 
  st_as_sf() |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)


bbox_new = st_bbox(bryozoaMap) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

Breaks <- c(0, 50, 100, 200, 1000)
Labels <- c("0 - 50", "50 - 100", "100 - 200", "200- >1000")
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")


png(file="Bryozoa",
    width=9, height=7, units="in", res=300)
tm_shape(bryozoaMap,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          #convert2density = TRUE,
          palette = MyPalette,
          breaks = Breaks, labels = Labels) +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Bryozoa",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()

rm(list = ls())
.rs.restartR()



## Chironomidae 

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

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  st_as_sf() |> 
  dplyr::rename(countryCod =ISO_A2)

gbif_aquac = st_read("data/shape/taxon_in_aqua/chironomidae.shp") 

## Count SPs per country 

chironomidae_N_sp = gbif_aquac |>
  dplyr::distinct(species, countryCod) |>
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(species = n()) |> 
  dplyr::rename(N_Species = species)


## Count occ per country 

chironomidae_N_OC =  gbif_aquac |> 
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(gbifID = n()) |> 
  dplyr::rename(N_Occurrences = gbifID)

## Merge 1 

chironomidae_SP_OC = merge(chironomidae_N_sp, chironomidae_N_OC, by= "countryCod") |> 
  st_as_sf()


## Merge 2
countryDF = as.data.frame(countryshp)

chironomidae_final = dplyr::left_join(countryDF, chironomidae_SP_OC, by= "countryCod" ) 


## Plot

chironomidaeMap = chironomidae_final  |> 
  st_as_sf() |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)


bbox_new = st_bbox(chironomidaeMap) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

Breaks <- c(0, 100, 200, 600, 1000)
Labels <- c("0 - 100", "100 - 200", "200 - 600", "600- >1000")
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")


png(file="Chironomidae",
    width=9, height=7, units="in", res=300)
tm_shape(chironomidaeMap,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          #convert2density = TRUE,
          palette = MyPalette,
          breaks = Breaks, labels = Labels) +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Chironomidae",
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

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  st_as_sf() |> 
  dplyr::rename(countryCod =ISO_A2)

gbif_aquac = st_read("data/shape/taxon_in_aqua/cnidaria.shp") 

## Count SPs per country 

cnidaria_N_sp = gbif_aquac |>
  dplyr::distinct(species, countryCod) |>
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(species = n()) |> 
  dplyr::rename(N_Species = species)


## Count occ per country 

cnidaria_N_OC =  gbif_aquac |> 
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(gbifID = n()) |> 
  dplyr::rename(N_Occurrences = gbifID)

## Merge 1 

cnidaria_SP_OC = merge(cnidaria_N_sp, cnidaria_N_OC, by= "countryCod") |> 
  st_as_sf()


## Merge 2
countryDF = as.data.frame(countryshp)

cnidaria_final = dplyr::left_join(countryDF, cnidaria_SP_OC, by= "countryCod" ) 


## Plot

cnidariaMap = cnidaria_final  |> 
  st_as_sf() |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)


bbox_new = st_bbox(cnidariaMap) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

Breaks <- c(0, 100, 200, 600, 1000)
Labels <- c("0 - 100", "100 - 200", "200 - 600", "600- >1000")
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")


png(file="Cnidaria",
    width=9, height=7, units="in", res=300)
tm_shape(cnidariaMap,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          #convert2density = TRUE,
          palette = MyPalette,
          breaks = Breaks, labels = Labels) +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Cnidaria",
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

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  st_as_sf() |> 
  dplyr::rename(countryCod =ISO_A2)

gbif_aquac = st_read("data/shape/taxon_in_aqua/crocodylia.shp") 

## Count SPs per country 

crocodylia_N_sp = gbif_aquac |>
  dplyr::distinct(species, countryCod) |>
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(species = n()) |> 
  dplyr::rename(N_Species = species)


## Count occ per country 

crocodylia_N_OC =  gbif_aquac |> 
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(gbifID = n()) |> 
  dplyr::rename(N_Occurrences = gbifID)

## Merge 1 

crocodylia_SP_OC = merge(crocodylia_N_sp, crocodylia_N_OC, by= "countryCod") |> 
  st_as_sf()


## Merge 2
countryDF = as.data.frame(countryshp)

crocodylia_final = dplyr::left_join(countryDF, crocodylia_SP_OC, by= "countryCod" ) 


## Plot

crocodyliaMap = crocodylia_final  |> 
  st_as_sf() |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)


bbox_new = st_bbox(crocodyliaMap) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

Breaks <- c(0, 50, 100, 200, 1000)
Labels <- c("0 - 50", "50 - 100", "100 - 200", "200- >1000")
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")


png(file="Crocodylia",
    width=9, height=7, units="in", res=300)
tm_shape(crocodyliaMap,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          #convert2density = TRUE,
          palette = MyPalette,
          breaks = Breaks, labels = Labels) +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Crocodylia",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()

rm(list = ls())
.rs.restartR()


## Decapoda (Crustácea)

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

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  st_as_sf() |> 
  dplyr::rename(countryCod =ISO_A2)

gbif_aquac = st_read("data/shape/taxon_in_aqua/decapoda.shp") 

## Count SPs per country 

decapoda_N_sp = gbif_aquac |>
  dplyr::distinct(species, countryCod) |>
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(species = n()) |> 
  dplyr::rename(N_Species = species)


## Count occ per country 

decapoda_N_OC =  gbif_aquac |> 
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(gbifID = n()) |> 
  dplyr::rename(N_Occurrences = gbifID)

## Merge 1 

decapoda_SP_OC = merge(decapoda_N_sp, decapoda_N_OC, by= "countryCod") |> 
  st_as_sf()


## Merge 2
countryDF = as.data.frame(countryshp)

decapoda_final = dplyr::left_join(countryDF, decapoda_SP_OC, by= "countryCod" ) 


## Plot

decapodaMap = decapoda_final  |> 
  st_as_sf() |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)


bbox_new = st_bbox(decapodaMap) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

Breaks <- c(0, 100, 200, 600, 1000)
Labels <- c("0 - 100", "100 - 200", "200 - 600", "600- >1000")
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")


png(file="Decapoda",
    width=9, height=7, units="in", res=300)
tm_shape(decapodaMap,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          #convert2density = TRUE,
          palette = MyPalette,
          breaks = Breaks, labels = Labels) +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "decapoda",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()

rm(list = ls())
.rs.restartR()


## Diplostraca (Curstácea) 

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

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  st_as_sf() |> 
  dplyr::rename(countryCod =ISO_A2)

gbif_aquac = st_read("data/shape/taxon_in_aqua/diplostraca.shp") 

## Count SPs per country 

diplostraca_N_sp = gbif_aquac |>
  dplyr::distinct(species, countryCod) |>
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(species = n()) |> 
  dplyr::rename(N_Species = species)


## Count occ per country 

diplostraca_N_OC =  gbif_aquac |> 
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(gbifID = n()) |> 
  dplyr::rename(N_Occurrences = gbifID)

## Merge 1 

diplostraca_SP_OC = merge(diplostraca_N_sp, diplostraca_N_OC, by= "countryCod") |> 
  st_as_sf()


## Merge 2
countryDF = as.data.frame(countryshp)

diplostraca_final = dplyr::left_join(countryDF, diplostraca_SP_OC, by= "countryCod" ) 


## Plot

diplostracaMap = diplostraca_final  |> 
  st_as_sf() |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)


bbox_new = st_bbox(diplostracaMap) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

Breaks <- c(0, 100, 200, 600, 1000)
Labels <- c("0 - 100", "100 - 200", "200 - 600", "600- >1000")
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")


png(file="Diplostraca",
    width=9, height=7, units="in", res=300)
tm_shape(diplostracaMap,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          #convert2density = TRUE,
          palette = MyPalette,
          breaks = Breaks, labels = Labels) +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Diplostraca",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()

rm(list = ls())
.rs.restartR()

## Maxillopoda (Crustácea)

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

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  st_as_sf() |> 
  dplyr::rename(countryCod =ISO_A2)

gbif_aquac = st_read("data/shape/taxon_in_aqua/maxillopoda.shp") 

## Count SPs per country 

maxillopoda_N_sp = gbif_aquac |>
  dplyr::distinct(species, countryCod) |>
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(species = n()) |> 
  dplyr::rename(N_Species = species)


## Count occ per country 

maxillopoda_N_OC =  gbif_aquac |> 
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(gbifID = n()) |> 
  dplyr::rename(N_Occurrences = gbifID)

## Merge 1 

maxillopoda_SP_OC = merge(maxillopoda_N_sp, maxillopoda_N_OC, by= "countryCod") |> 
  st_as_sf()


## Merge 2
countryDF = as.data.frame(countryshp)

maxillopoda_final = dplyr::left_join(countryDF, maxillopoda_SP_OC, by= "countryCod" ) 


## Plot

maxillopodaMap = maxillopoda_final  |> 
  st_as_sf() |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)


bbox_new = st_bbox(maxillopodaMap) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

Breaks <- c(0, 50, 100, 200, 1000)
Labels <- c("0 - 50", "50 - 100", "100 - 200", "200 - 1000")
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")


png(file="Maxillopoda",
    width=9, height=7, units="in", res=300)
tm_shape(maxillopodaMap,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          #convert2density = TRUE,
          palette = MyPalette,
          breaks = Breaks, labels = Labels) +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Maxillopoda",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()

rm(list = ls())
.rs.restartR()


## Ostracoda (Crustácea)

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

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  st_as_sf() |> 
  dplyr::rename(countryCod =ISO_A2)

gbif_aquac = st_read("data/shape/taxon_in_aqua/ostracoda.shp") 

## Count SPs per country 

ostracoda_N_sp = gbif_aquac |>
  dplyr::distinct(species, countryCod) |>
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(species = n()) |> 
  dplyr::rename(N_Species = species)


## Count occ per country 

ostracoda_N_OC =  gbif_aquac |> 
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(gbifID = n()) |> 
  dplyr::rename(N_Occurrences = gbifID)

## Merge 1 

ostracoda_SP_OC = merge(ostracoda_N_sp, ostracoda_N_OC, by= "countryCod") |> 
  st_as_sf()


## Merge 2
countryDF = as.data.frame(countryshp)

ostracoda_final = dplyr::left_join(countryDF, ostracoda_SP_OC, by= "countryCod" ) 


## Plot

ostracodaMap = ostracoda_final  |> 
  st_as_sf() |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)


bbox_new = st_bbox(ostracodaMap) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

Breaks <- c(0, 100, 200, 600, 1000)
Labels <- c("0 - 100", "100 - 200", "200 - 600", "600- >1000")
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")


png(file="Ostracoda",
    width=9, height=7, units="in", res=300)
tm_shape(ostracodaMap,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          #convert2density = TRUE,
          palette = MyPalette,
          breaks = Breaks, labels = Labels) +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Ostracoda",
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

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  st_as_sf() |> 
  dplyr::rename(countryCod =ISO_A2)

gbif_aquac = st_read("data/shape/taxon_in_aqua/culicidae.shp") 

## Count SPs per country 

culicidae_N_sp = gbif_aquac |>
  dplyr::distinct(species, countryCod) |>
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(species = n()) |> 
  dplyr::rename(N_Species = species)


## Count occ per country 

culicidae_N_OC =  gbif_aquac |> 
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(gbifID = n()) |> 
  dplyr::rename(N_Occurrences = gbifID)

## Merge 1 

culicidae_SP_OC = merge(culicidae_N_sp, culicidae_N_OC, by= "countryCod") |> 
  st_as_sf()


## Merge 2
countryDF = as.data.frame(countryshp)

culicidae_final = dplyr::left_join(countryDF, culicidae_SP_OC, by= "countryCod" ) 


## Plot

culicidaeMap = culicidae_final  |> 
  st_as_sf() |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)


bbox_new = st_bbox(culicidaeMap) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

Breaks <- c(0, 100, 200, 600, 1000)
Labels <- c("0 - 100", "100 - 200", "200 - 600", "600- >1000")
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")


png(file="Culicidae",
    width=9, height=7, units="in", res=300)
tm_shape(culicidaeMap,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          #convert2density = TRUE,
          palette = MyPalette,
          breaks = Breaks, labels = Labels) +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Culicidae",
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

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  st_as_sf() |> 
  dplyr::rename(countryCod =ISO_A2)

gbif_aquac = st_read("data/shape/taxon_in_aqua/ephemeroptera.shp") 

## Count SPs per country 

ephemeroptera_N_sp = gbif_aquac |>
  dplyr::distinct(species, countryCod) |>
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(species = n()) |> 
  dplyr::rename(N_Species = species)


## Count occ per country 

ephemeroptera_N_OC =  gbif_aquac |> 
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(gbifID = n()) |> 
  dplyr::rename(N_Occurrences = gbifID)

## Merge 1 

ephemeroptera_SP_OC = merge(ephemeroptera_N_sp, ephemeroptera_N_OC, by= "countryCod") |> 
  st_as_sf()


## Merge 2
countryDF = as.data.frame(countryshp)

ephemeroptera_final = dplyr::left_join(countryDF, ephemeroptera_SP_OC, by= "countryCod" ) 


## Plot

ephemeropteraMap = ephemeroptera_final  |> 
  st_as_sf() |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)


bbox_new = st_bbox(ephemeropteraMap) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

Breaks <- c(0, 100, 200, 600, 1000)
Labels <- c("0 - 100", "100 - 200", "200 - 600", "600- >1000")
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")


png(file="Ephemeroptera",
    width=9, height=7, units="in", res=300)
tm_shape(ephemeropteraMap,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          #convert2density = TRUE,
          palette = MyPalette,
          breaks = Breaks, labels = Labels) +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Ephemeroptera",
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

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  st_as_sf() |> 
  dplyr::rename(countryCod =ISO_A2)

gbif_aquac = st_read("data/shape/taxon_in_aqua/euglenozoa.shp") 

## Count SPs per country 

euglenozoa_N_sp = gbif_aquac |>
  dplyr::distinct(species, countryCod) |>
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(species = n()) |> 
  dplyr::rename(N_Species = species)


## Count occ per country 

euglenozoa_N_OC =  gbif_aquac |> 
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(gbifID = n()) |> 
  dplyr::rename(N_Occurrences = gbifID)

## Merge 1 

euglenozoa_SP_OC = merge(euglenozoa_N_sp, euglenozoa_N_OC, by= "countryCod") |> 
  st_as_sf()


## Merge 2
countryDF = as.data.frame(countryshp)

euglenozoa_final = dplyr::left_join(countryDF, euglenozoa_SP_OC, by= "countryCod" ) 


## Plot

euglenozoaMap = euglenozoa_final  |> 
  st_as_sf() |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)


bbox_new = st_bbox(euglenozoaMap) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

Breaks <- c(0, 100, 200, 600, 1000)
Labels <- c("0 - 100", "100 - 200", "200 - 600", "600- >1000")
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")


png(file="Euglenozoa",
    width=9, height=7, units="in", res=300)
tm_shape(euglenozoaMap,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          #convert2density = TRUE,
          palette = MyPalette,
          breaks = Breaks, labels = Labels) +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Euglenozoa",
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

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  st_as_sf() |> 
  dplyr::rename(countryCod =ISO_A2)

gbif_aquac = st_read("data/shape/taxon_in_aqua/hemiptera.shp") 

## Count SPs per country 

hemiptera_N_sp = gbif_aquac |>
  dplyr::distinct(species, countryCod) |>
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(species = n()) |> 
  dplyr::rename(N_Species = species)


## Count occ per country 

hemiptera_N_OC =  gbif_aquac |> 
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(gbifID = n()) |> 
  dplyr::rename(N_Occurrences = gbifID)

## Merge 1 

hemiptera_SP_OC = merge(hemiptera_N_sp, hemiptera_N_OC, by= "countryCod") |> 
  st_as_sf()


## Merge 2
countryDF = as.data.frame(countryshp)

hemiptera_final = dplyr::left_join(countryDF, hemiptera_SP_OC, by= "countryCod" ) 


## Plot

hemipteraMap = hemiptera_final  |> 
  st_as_sf() |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)


bbox_new = st_bbox(hemipteraMap) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

Breaks <- c(0, 100, 200, 600, 1000)
Labels <- c("0 - 100", "100 - 200", "200 - 600", "600- >1000")
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")


png(file="Hemiptera",
    width=9, height=7, units="in", res=300)
tm_shape(hemipteraMap,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          #convert2density = TRUE,
          palette = MyPalette,
          breaks = Breaks, labels = Labels) +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Hemiptera",
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

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  st_as_sf() |> 
  dplyr::rename(countryCod =ISO_A2)

gbif_aquac = st_read("data/shape/taxon_in_aqua/isopoda.shp") 

## Count SPs per country 

isopoda_N_sp = gbif_aquac |>
  dplyr::distinct(species, countryCod) |>
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(species = n()) |> 
  dplyr::rename(N_Species = species)


## Count occ per country 

isopoda_N_OC =  gbif_aquac |> 
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(gbifID = n()) |> 
  dplyr::rename(N_Occurrences = gbifID)

## Merge 1 

isopoda_SP_OC = merge(isopoda_N_sp, isopoda_N_OC, by= "countryCod") |> 
  st_as_sf()


## Merge 2
countryDF = as.data.frame(countryshp)

isopoda_final = dplyr::left_join(countryDF, isopoda_SP_OC, by= "countryCod" ) 


## Plot

isopodaMap = isopoda_final  |> 
  st_as_sf() |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)


bbox_new = st_bbox(isopodaMap) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

Breaks <- c(0, 50, 100, 200, 1000)
Labels <- c("0 - 50", "50 - 100", "100 - 200", "200- >1000")
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")


png(file="Isopoda",
    width=9, height=7, units="in", res=300)
tm_shape(isopodaMap,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          #convert2density = TRUE,
          palette = MyPalette,
          breaks = Breaks, labels = Labels) +
  tm_layout(legend.position = c("right", "bottom"),
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

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  st_as_sf() |> 
  dplyr::rename(countryCod =ISO_A2)

gbif_aquac = st_read("data/shape/taxon_in_aqua/megaloptera.shp") 

## Count SPs per country 

megaloptera_N_sp = gbif_aquac |>
  dplyr::distinct(species, countryCod) |>
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(species = n()) |> 
  dplyr::rename(N_Species = species)


## Count occ per country 

megaloptera_N_OC =  gbif_aquac |> 
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(gbifID = n()) |> 
  dplyr::rename(N_Occurrences = gbifID)

## Merge 1 

megaloptera_SP_OC = merge(megaloptera_N_sp, megaloptera_N_OC, by= "countryCod") |> 
  st_as_sf()


## Merge 2
countryDF = as.data.frame(countryshp)

megaloptera_final = dplyr::left_join(countryDF, megaloptera_SP_OC, by= "countryCod" ) 


## Plot

megalopteraMap = megaloptera_final  |> 
  st_as_sf() |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)


bbox_new = st_bbox(megalopteraMap) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

Breaks <- c(0, 50, 100, 200, 1000)
Labels <- c("0 - 50", "50 - 100", "100 - 200", "200- >1000")
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")


png(file="Megaloptera",
    width=9, height=7, units="in", res=300)
tm_shape(megalopteraMap,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          #convert2density = TRUE,
          palette = MyPalette,
          breaks = Breaks, labels = Labels) +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Megaloptera",
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

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  st_as_sf() |> 
  dplyr::rename(countryCod =ISO_A2)

gbif_aquac = st_read("data/shape/taxon_in_aqua/nematoda.shp") 

## Count SPs per country 

nematoda_N_sp = gbif_aquac |>
  dplyr::distinct(species, countryCod) |>
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(species = n()) |> 
  dplyr::rename(N_Species = species)


## Count occ per country 

nematoda_N_OC =  gbif_aquac |> 
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(gbifID = n()) |> 
  dplyr::rename(N_Occurrences = gbifID)

## Merge 1 

nematoda_SP_OC = merge(nematoda_N_sp, nematoda_N_OC, by= "countryCod") |> 
  st_as_sf()


## Merge 2
countryDF = as.data.frame(countryshp)

nematoda_final = dplyr::left_join(countryDF, nematoda_SP_OC, by= "countryCod" ) 


## Plot

nematodaMap = nematoda_final  |> 
  st_as_sf() |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)


bbox_new = st_bbox(nematodaMap) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

Breaks <- c(0, 50, 100, 200, 1000)
Labels <- c("0 - 50", "50 - 100", "100 - 200", "200- >1000")
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")


png(file="Nematoda",
    width=9, height=7, units="in", res=300)
tm_shape(nematodaMap,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          #convert2density = TRUE,
          palette = MyPalette,
          breaks = Breaks, labels = Labels) +
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

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  st_as_sf() |> 
  dplyr::rename(countryCod =ISO_A2)

gbif_aquac = st_read("data/shape/taxon_in_aqua/nematomorpha.shp") 

## Count SPs per country 

nematomorpha_N_sp = gbif_aquac |>
  dplyr::distinct(species, countryCod) |>
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(species = n()) |> 
  dplyr::rename(N_Species = species)


## Count occ per country 

nematomorpha_N_OC =  gbif_aquac |> 
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(gbifID = n()) |> 
  dplyr::rename(N_Occurrences = gbifID)

## Merge 1 

nematomorpha_SP_OC = merge(nematomorpha_N_sp, nematomorpha_N_OC, by= "countryCod") |> 
  st_as_sf()


## Merge 2
countryDF = as.data.frame(countryshp)

nematomorpha_final = dplyr::left_join(countryDF, nematomorpha_SP_OC, by= "countryCod" ) 


## Plot

nematomorphaMap = nematomorpha_final  |> 
  st_as_sf() |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)


bbox_new = st_bbox(nematomorphaMap) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

Breaks <- c(0, 50, 100, 200, 1000)
Labels <- c("0 - 50", "50 - 100", "100 - 200", "200- >1000")
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")


png(file="Nematomorpha",
    width=9, height=7, units="in", res=300)
tm_shape(nematomorphaMap,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          #convert2density = TRUE,
          palette = MyPalette,
          breaks = Breaks, labels = Labels) +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Nematomorpha",
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

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  st_as_sf() |> 
  dplyr::rename(countryCod =ISO_A2)

gbif_aquac = st_read("data/shape/taxon_in_aqua/plecoptera.shp") 

## Count SPs per country 

plecoptera_N_sp = gbif_aquac |>
  dplyr::distinct(species, countryCod) |>
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(species = n()) |> 
  dplyr::rename(N_Species = species)


## Count occ per country 

plecoptera_N_OC =  gbif_aquac |> 
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(gbifID = n()) |> 
  dplyr::rename(N_Occurrences = gbifID)

## Merge 1 

plecoptera_SP_OC = merge(plecoptera_N_sp, plecoptera_N_OC, by= "countryCod") |> 
  st_as_sf()


## Merge 2
countryDF = as.data.frame(countryshp)

plecoptera_final = dplyr::left_join(countryDF, plecoptera_SP_OC, by= "countryCod" ) 


## Plot

plecopteraMap = plecoptera_final  |> 
  st_as_sf() |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)


bbox_new = st_bbox(plecopteraMap) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

Breaks <- c(0, 50, 100, 200, 1000)
Labels <- c("0 - 50", "50 - 100", "100 - 200", "200- >1000")
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")


png(file="Plecoptera",
    width=9, height=7, units="in", res=300)
tm_shape(plecopteraMap,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          #convert2density = TRUE,
          palette = MyPalette,
          breaks = Breaks, labels = Labels) +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Plecoptera",
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

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  st_as_sf() |> 
  dplyr::rename(countryCod =ISO_A2)

gbif_aquac = st_read("data/shape/taxon_in_aqua/porifera.shp") 

## Count SPs per country 

porifera_N_sp = gbif_aquac |>
  dplyr::distinct(species, countryCod) |>
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(species = n()) |> 
  dplyr::rename(N_Species = species)


## Count occ per country 

porifera_N_OC =  gbif_aquac |> 
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(gbifID = n()) |> 
  dplyr::rename(N_Occurrences = gbifID)

## Merge 1 

porifera_SP_OC = merge(porifera_N_sp, porifera_N_OC, by= "countryCod") |> 
  st_as_sf()


## Merge 2
countryDF = as.data.frame(countryshp)

porifera_final = dplyr::left_join(countryDF, porifera_SP_OC, by= "countryCod" ) 


## Plot

poriferaMap = porifera_final  |> 
  st_as_sf() |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)


bbox_new = st_bbox(poriferaMap) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

Breaks <- c(0, 50, 100, 200, 1000)
Labels <- c("0 - 50", "50 - 100", "100 - 200", "200- >1000")
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")


png(file="Porifera",
    width=9, height=7, units="in", res=300)
tm_shape(poriferaMap,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          #convert2density = TRUE,
          palette = MyPalette,
          breaks = Breaks, labels = Labels) +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Porifera",
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

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  st_as_sf() |> 
  dplyr::rename(countryCod =ISO_A2)

gbif_aquac = st_read("data/shape/taxon_in_aqua/simuliidae.shp") 

## Count SPs per country 

simuliidae_N_sp = gbif_aquac |>
  dplyr::distinct(species, countryCod) |>
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(species = n()) |> 
  dplyr::rename(N_Species = species)


## Count occ per country 

simuliidae_N_OC =  gbif_aquac |> 
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(gbifID = n()) |> 
  dplyr::rename(N_Occurrences = gbifID)

## Merge 1 

simuliidae_SP_OC = merge(simuliidae_N_sp, simuliidae_N_OC, by= "countryCod") |> 
  st_as_sf()


## Merge 2
countryDF = as.data.frame(countryshp)

simuliidae_final = dplyr::left_join(countryDF, simuliidae_SP_OC, by= "countryCod" ) 


## Plot

simuliidaeMap = simuliidae_final  |> 
  st_as_sf() |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)


bbox_new = st_bbox(simuliidaeMap) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

Breaks <- c(0, 50, 100, 200, 1000)
Labels <- c("0 - 50", "50 - 100", "100 - 200", "200- >1000")
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")


png(file="Simuliidae",
    width=9, height=7, units="in", res=300)
tm_shape(simuliidaeMap,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          #convert2density = TRUE,
          palette = MyPalette,
          breaks = Breaks, labels = Labels) +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Simuliidae",
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

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  st_as_sf() |> 
  dplyr::rename(countryCod =ISO_A2)

gbif_aquac = st_read("data/shape/taxon_in_aqua/testudines.shp") 

## Count SPs per country 

testudines_N_sp = gbif_aquac |>
  dplyr::distinct(species, countryCod) |>
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(species = n()) |> 
  dplyr::rename(N_Species = species)


## Count occ per country 

testudines_N_OC =  gbif_aquac |> 
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(gbifID = n()) |> 
  dplyr::rename(N_Occurrences = gbifID)

## Merge 1 

testudines_SP_OC = merge(testudines_N_sp, testudines_N_OC, by= "countryCod") |> 
  st_as_sf()


## Merge 2
countryDF = as.data.frame(countryshp)

testudines_final = dplyr::left_join(countryDF, testudines_SP_OC, by= "countryCod" ) 


## Plot

testudinesMap = testudines_final  |> 
  st_as_sf() |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)


bbox_new = st_bbox(testudinesMap) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

Breaks <- c(0, 100, 200, 600, 1000)
Labels <- c("0 - 100", "100 - 200", "200 - 600", "600- >1000")
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")


png(file="Testudines",
    width=9, height=7, units="in", res=300)
tm_shape(testudinesMap,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          #convert2density = TRUE,
          palette = MyPalette,
          breaks = Breaks, labels = Labels) +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Testudines",
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

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  st_as_sf() |> 
  dplyr::rename(countryCod =ISO_A2)

gbif_aquac = st_read("data/shape/taxon_in_aqua/trichoptera.shp") 

## Count SPs per country 

trichoptera_N_sp = gbif_aquac |>
  dplyr::distinct(species, countryCod) |>
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(species = n()) |> 
  dplyr::rename(N_Species = species)


## Count occ per country 

trichoptera_N_OC =  gbif_aquac |> 
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(gbifID = n()) |> 
  dplyr::rename(N_Occurrences = gbifID)

## Merge 1 

trichoptera_SP_OC = merge(trichoptera_N_sp, trichoptera_N_OC, by= "countryCod") |> 
  st_as_sf()


## Merge 2
countryDF = as.data.frame(countryshp)

trichoptera_final = dplyr::left_join(countryDF, trichoptera_SP_OC, by= "countryCod" ) 


## Plot

trichopteraMap = trichoptera_final  |> 
  st_as_sf() |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)


bbox_new = st_bbox(trichopteraMap) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

Breaks <- c(0, 50, 100, 200, 1000)
Labels <- c("0 - 50", "50 - 100", "100 - 200", "200- >1000")
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")


png(file="Trichoptera",
    width=9, height=7, units="in", res=300)
tm_shape(trichopteraMap,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          #convert2density = TRUE,
          palette = MyPalette,
          breaks = Breaks, labels = Labels) +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Trichoptera",
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

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  st_as_sf() |> 
  dplyr::rename(countryCod =ISO_A2)

gbif_aquac = st_read("data/shape/taxon_in_aqua/tubellaria.shp") 

## Count SPs per country 

tubellaria_N_sp = gbif_aquac |>
  dplyr::distinct(species, countryCod) |>
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(species = n()) |> 
  dplyr::rename(N_Species = species)


## Count occ per country 

tubellaria_N_OC =  gbif_aquac |> 
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(gbifID = n()) |> 
  dplyr::rename(N_Occurrences = gbifID)

## Merge 1 

tubellaria_SP_OC = merge(tubellaria_N_sp, tubellaria_N_OC, by= "countryCod") |> 
  st_as_sf()


## Merge 2
countryDF = as.data.frame(countryshp)

tubellaria_final = dplyr::left_join(countryDF, tubellaria_SP_OC, by= "countryCod" ) 


## Plot

tubellariaMap = tubellaria_final  |> 
  st_as_sf() |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)


bbox_new = st_bbox(tubellariaMap) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

Breaks <- c(0, 50, 100, 200, 1000)
Labels <- c("0 - 50", "50 - 100", "100 - 200", "200- >1000")
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")


png(file="Tubellaria",
    width=9, height=7, units="in", res=300)
tm_shape(tubellariaMap,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          #convert2density = TRUE,
          palette = MyPalette,
          breaks = Breaks, labels = Labels) +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Tubellaria",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()

rm(list = ls())
.rs.restartR()


## Amphibia 

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

countryshp = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") |> 
  st_as_sf() |> 
  dplyr::rename(countryCod =ISO_A2)

gbif_aquac = st_read("data/shape/taxon_in_aqua/amph.shp") 

## Count SPs per country 

amphibia_N_sp = gbif_aquac |>
  dplyr::distinct(species, countryCod) |>
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(species = n()) |> 
  dplyr::rename(N_Species = species)


## Count occ per country 

amphibia_N_OC =  gbif_aquac |> 
  dplyr::group_by(countryCod) |> 
  dplyr::summarise(gbifID = n()) |> 
  dplyr::rename(N_Occurrences = gbifID)

## Merge 1 

amphibia_SP_OC = merge(amphibia_N_sp, amphibia_N_OC, by= "countryCod") |> 
  st_as_sf()


## Merge 2
countryDF = as.data.frame(countryshp)

amphibia_final = dplyr::left_join(countryDF, amphibia_SP_OC, by= "countryCod" ) 


## Plot

amphibiaMap = amphibia_final  |> 
  st_as_sf() |> 
  dplyr::select(ADMIN, N_Species,N_Occurrences)


bbox_new = st_bbox(amphibiaMap) 
xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

Breaks <- c(0, 100, 200, 600, 1000)
Labels <- c("0 - 100", "100 - 200", "200 - 600", "600- >1000")
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")


png(file="Amphibia",
    width=9, height=7, units="in", res=300)
tm_shape(amphibiaMap,  bbox = bbox_new) +
  tm_fill(c("N_Species", "N_Occurrences"), 
          #convert2density = TRUE,
          palette = MyPalette,
          breaks = Breaks, labels = Labels) +
  tm_layout(legend.position = c("right", "bottom"),
            main.title = "Amphibia",
            main.title.position = "centre") +
  tm_borders(alpha = 0.5)

dev.off()

rm(list = ls())
.rs.restartR()

