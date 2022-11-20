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


gbif = fread("data-raw/odonata/0011743-220831081235567.txt", sep = "\t",header = TRUE, na.strings = "\\N")

gbif_n_sp = gbif |>
  drop_na(c(decimalLatitude, decimalLongitude, species, countryCode)) |> 
  select(species, countryCode) |>
  distinct(species, countryCode) |> 
  group_by(species,countryCode) |> 
  mutate(N_sum = countryCode) 

rm(N)
N = gbif_n_sp |>
  select(species) |> 
  #mutate(species = NULL)
  count(countryCode)
  
  
write_csv(gbif_n_sp, "results/Odonata/Odonata_GBIF_sps.csv")



#############

country_shape = shapefile("results/Odonata/to_count_sp.shp") 

gbif = read.csv("results/Odonata/count_sp_Odonata_GBIF.csv")

m <- merge(country_shape, gbif, by= "countryCod")

shapefile(m, "results/Odonata/merge_test1.shp") 

