library(tidyverse)
library(zoo)
library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)
library(data.table)
library(rgdal)
library(sp)


gbif = fread("0011743-220831081235567.txt", sep = "\t",header = TRUE, na.strings = "\\N")

gbif_n_sp = gbif |>
  drop_na(c(decimalLatitude, decimalLongitude, species, countryCode)) |> 
  select(species, countryCode) |>
  distinct(species, countryCode) 
  
write_csv(gbif_n_sp, "results/Odonata/Odonata_GBIF_sps.csv")
  
 
