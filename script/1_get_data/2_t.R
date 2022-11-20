library(tidyverse)
library(zoo)
library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)
library(data.table)
library(rgdal)
library(sp)
library(raster)
library(plyr)

## Count sps per country 

bivalviaSHP = st_read("results/bivalvia.shp") |> 
  as_tibble()

bivalviaSHP_n_sp = bivalviaSHP |>
  drop_na(c(dcmlLtt, dcmlLng, species, ISO_A2_x)) |> 
  dplyr::select(species, ISO_A2_x) |>
  distinct(species, ISO_A2_x) |> 
  group_by(species,ISO_A2_x) |> 
  subset(ISO_A2_x != -99)
  #mutate(N_sum = ISO_A2_x) 

bivalvia_N_SPP = plyr::count(bivalviaSHP_n_sp$N_sum) 
bivalvia_N_SPP = dplyr::rename(bivalvia_N_SPP, ISO_A2_x = x)

bivalvia_N_SP = merge(bivalviaSHP, bivalvia_N_SPP, by= "ISO_A2_x")

## Count occ per country 

bivalvia_N_OCC =  bivalvia_N_SPP |> 
  dplyr::group_by(ISO_A2_x) |> 
  dplyr::summarise(gbifID = n()) 

## Merge  

bivalvia_final = merge(bivalvia_N_SPP, bivalvia_N_OCC, by= "ISO_A2_x")

shapefile(bivalvia_final, "results/bivalvia.shp",  overwrite=TRUE) 