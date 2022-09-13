library(rocc)
library(rgdal)
library(tools)
library(rgbif)
library(stringr)
library(dplyr)
library(tidyr)
library(tibble)
############### 1 


raw_names = sf::read_sf("data/shape/world_rivers/join_river_country.shp")

unique_names = raw_names |> 
  dplyr::select(RIVER_MAP, ADMIN, ISO_A2) |> 
  dplyr::distinct(RIVER_MAP, ADMIN,ISO_A2)

#write.csv(unique_names, "data/shape/world_rivers/final_names_world_rivers.csv",sep = " ")

#### 


AF = unique_names |> 
  dplyr::filter(ADMIN == "Afghanistan") |> 
  dplyr::mutate(River_Name = RIVER_MAP) |> 
  tibble::column_to_rownames(var = "RIVER_MAP") |> 
  dplyr::distinct(River_Name, ADMIN)

AF_count = occ_search(waterBody = c(rownames(AF)), 
                      limit = 0) |> 
  unlist()|> 
  as.data.frame() |>
  tibble::rownames_to_column("River_Name") |> 
  dplyr::filter(str_detect(River_Name, 'count')) |> 
  #tidyr::pivot_wider(names_from = River_Name, values_from = unlist) |>
  dplyr::mutate(AF_sum =rowSums(across(where(is.numeric)))) |> 
  mutate(River_Name = recode(River_Name, meta.count= "Amu Darya")) 

##

DZ = unique_names |> 
  dplyr::filter(ADMIN == "Algeria") |> 
  dplyr::mutate(River_Name= RIVER_MAP) |> 
  tibble::column_to_rownames(var = "RIVER_MAP") |> 
  dplyr::distinct(River_Name, ADMIN)

DZ_count = occ_search(waterBody = c(row.names(DZ)), 
                      limit = 0) |> 
  unlist()|> 
  as.data.frame() |>
  tibble::rownames_to_column("River_Name") |> 
  dplyr::filter(str_detect(River_Name, 'count')) |> 
  #tidyr::pivot_wider(names_from = River_Name, values_from = unlist) |>
  dplyr::mutate(AF_sum =rowSums(across(where(is.numeric)))) |> 
  mutate(River_Name = recode(River_Name, meta.count= "Chelif")) 

##

AO = unique_names |> 
  dplyr::filter(ADMIN == "Angola") |> 
  dplyr::mutate(River_Name= RIVER_MAP) |> 
  tibble::column_to_rownames(var = "RIVER_MAP") |> 
  dplyr::distinct(River_Name, ADMIN)

AO_count = occ_search(waterBody = c(row.names(AO)), 
                      limit = 0)|> 
  unlist() 

AO_count = as.data.frame(AO_count) |> 
  tibble::rownames_to_column() |> 
  dplyr::filter(str_detect(rowname,'count')) |> 
  tidyr::pivot_wider(names_from = rowname, values_from = AO_count) |>
  dplyr::mutate(sum =rowSums(across(where(is.numeric))))




AR = unique_names |> 
  dplyr::filter(ADMIN == "Argentina") |> 
  dplyr::mutate(River_Name= RIVER_MAP) |> 
  tibble::column_to_rownames(var = "RIVER_MAP") |> 
  dplyr::distinct(River_Name, ADMIN)

AR_count = occ_search(waterBody = c(row.names(AR)), 
                      limit = 0)|> 
  unlist = unlist()

AM = unique_names |> 
  dplyr::filter(ADMIN == "Armenia") |> 
  dplyr::mutate(River_Name= RIVER_MAP) |> 
  tibble::column_to_rownames(var = "RIVER_MAP") |> 
  dplyr::distinct(River_Name, ADMIN)

AM_count = occ_search(waterBody = c(row.names(AM)), 
                      limit = 0)|> 
  unlist = unlist()

AU = unique_names |> 
  dplyr::filter(ADMIN == "Australia") |> 
  dplyr::mutate(River_Name= RIVER_MAP) |> 
  tibble::column_to_rownames(var = "RIVER_MAP") |> 
  dplyr::distinct(River_Name, ADMIN)

AU_count = occ_search(waterBody = c(row.names(AU)), 
                      limit = 0)|> 
  unlist = unlist()

AT = unique_names |> 
  dplyr::filter(ADMIN == "Austria") |> 
  dplyr::mutate(River_Name= RIVER_MAP) |> 
  tibble::column_to_rownames(var = "RIVER_MAP") |> 
  dplyr::distinct(River_Name, ADMIN)

AT_count = occ_search(waterBody = c(row.names(AT)), 
                      limit = 0)|> 
  unlist = unlist()

AZ = unique_names |> 
  dplyr::filter(ADMIN == "Azerbaijan") |> 
  dplyr::mutate(River_Name= RIVER_MAP) |> 
  tibble::column_to_rownames(var = "RIVER_MAP") |> 
  dplyr::distinct(River_Name, ADMIN)

AZ_count = occ_search(waterBody = c(row.names(AZ)), 
                      limit = 0)|> 
  unlist = unlist()

## Join

listaa = list(AF_count, AM_count, AO_count, AR_count, AT_count, AU_count, AZ_count, DZ_count)

join_all = do.call("rbind",listaa)



## Get record count with occ_search() by setting limit=0

us_count = occ_search(waterBody = c(row.names(US)), 
                  limit = 0)

unlist = unlist(us_count)

library(dplyr)
dat = as.data.frame(unlist) |>
  tibble::rownames_to_column("names_river") |> 
  dplyr::filter(str_detect(names_river,'count')) |> 
  tidyr::pivot_wider(names_from = names_river, values_from = unlist) |>
  dplyr::mutate(sum =rowSums(across(where(is.numeric))), .before = Colville.meta.count)

  
############################################################################################################################################################################################################################################################################################################################################################################### 

raw_names = sf::read_sf("data/shape/world_rivers/join_river_country.shp")

unique_names = raw_names |> 
  dplyr::select(RIVER_MAP, ADMIN, geometry) |> 
  dplyr::distinct(RIVER_MAP, ADMIN) 

country_names = unique_names |> 
  distinct(ADMIN) |> 
  dplyr::mutate(ADMIN_2 = ADMIN) |>
  drop_na() |> 
  tibble::column_to_rownames(var = "ADMIN")  

river_name = unique_names |> 
  distinct(River_Name) |> 
  dplyr::mutate(River_Name_2 = River_Name) |>
  drop_na() |> 
  tibble::column_to_rownames(var = "River_Name") 

#### Esta ok, não mexer 
US = unique_names |>
  dplyr::filter(ADMIN == "United States of America") |> 
  dplyr::mutate(River_Name= RIVER_MAP) |> 
  tibble::column_to_rownames(var = "RIVER_MAP") |> 
  dplyr::distinct(River_Name, ADMIN)
 
  
us_search = occ_search(waterBody = c(row.names(US)),
                       hasCoordinate=TRUE,
                       hasGeospatialIssue=FALSE,
           limit = 0) |> 
  unlist() 

US_sum = us_search |> 
  as.data.frame() |>
  tibble::rownames_to_column() |> 
  mutate(River_Name = rowname) |> 
  dplyr::select(River_Name, us_search) |> 
  dplyr::filter(str_detect(River_Name, 'count')) |> 
  tidyr::pivot_wider(names_from = River_Name, values_from = us_search) |>
  dplyr::mutate(AF_sum = rowSums(across(where(is.numeric))))






