library(tidyverse)
library(zoo)
library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)
library(data.table)
library(rgdal)
library(sp)

###########################
###########################
## Rotifera
########################### 
###########################

rotifera_all <- sf::read_sf("results/rotifera/rotifera.shp") |> 
   as.data.frame()
 
 test2 =  rotifera_all %>% 
   group_by(ISO_A2) %>%
   summarise(gbifid = n()) %>%
   mutate(N_sum = gbifid) %>% 
   ggplot+ geom_bar(aes(reorder(ISO_A2, gbifid), gbifid), stat = 'identity') +
   theme_minimal() + 
   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(
     title = 'Rotifera georref coords per country',
     x = 'rotifera_all')  +
   geom_text(aes(x=ISO_A2, y=N_sum, label= N_sum),
             hjust = -0.5, size = 2,
             position = position_dodge(width = 1))+
   coord_flip() + 
   theme_bw()
 
###########################
###########################
## Odonta
########################### 
###########################
 
## Way 3
 
country_shape_raw = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") 
 
country_shape = as_tibble(country_shape_raw)|>
  rename(countryCode = ISO_A2) |> 
  subset(countryCode != -99)

gbif = fread("0011743-220831081235567.txt", sep = "\t",header = TRUE, na.strings = "\\N")

gbif_sum = gbif |>
  drop_na(c(decimalLatitude, decimalLongitude, gbifID)) |>
  count(countryCode) |> 
  slice(-1) #|>
  #rename(v2_cc = countryCode) #|> 
  #cbind(gbif)


joined = country_shape |> 
  left_join(gbif_sum , by = "countryCode")  |>
  #subset(countryCode != -99) |> 
  select(n, countryCode)
  


write.csv(joined, "results/Odonata/255_Odonata_GBIF.csv")



nomes = c(country_shape_raw$ADMIN)

nomes = nomes |> 
  as_tibble() |> 
  rename(countries = value) |> 
  mutate(Names = countries )

nomes2 = tibble::column_to_rownames(nomes, var = "countries")

### way 2
gbif = fread("0011743-220831081235567.txt", sep = "\t",header = TRUE, na.strings = "\\N")
 
gbif_sum = gbif |>
  drop_na(c(decimalLatitude, decimalLongitude, gbifID)) |>
  count(countryCode) |> 
  slice(-1) |> 
  cbind(gbif)


write_csv(gbif_sum, "data/shape/Odonata_GBIF_ALL/Odonata_GBIF_ALL.csv")

 
### way 1_ not work well
country_shape = readOGR("data/shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") 
country_shape = st_as_sf(country_shape) 

gbif = fread("0011743-220831081235567.txt", sep = "\t",header = TRUE, na.strings = "\\N")

gbif = gbif |>
  drop_na(c(decimalLatitude, decimalLongitude, gbifID))
   
sf_gbif = st_as_sf(gbif, coords = c("decimalLongitude", "decimalLatitude"), # note the order!
          crs = 4326) 

gbif_sf_shape = sf_gbif |> 
  group_by(countryCode) |> 
  summarise(gbifID = n()) |>
  mutate(N_sum = gbifID) |> 
  st_join(country_shape, left = TRUE) |> 
  slice(-1)

to_qgis = as_data_frame(gbif_sf_shape)

write_csv(to_qgis, "data/shape/Odonata_GBIF_ALL/Odonata_GBIF_ALL.csv")

#st_write(gbif_sf_shape, "data/shape/Odonata_GBIF_ALL/Odonata_GBIF_ALL#.shp")

#ggplot() + 
#  geom_sf(data = gbif_sf_shape, aes(fill = N_sum))


 
#### GGPlot
odonata = gbif %>%
   as.data.frame() %>%
   group_by(countryCode) %>%
   summarise(gbifid = n()) %>% 
   mutate(N_sum = gbifid) %>% 
   top_n(60) %>% 
   ggplot+ geom_bar(aes(reorder(countryCode, gbifid), gbifid), stat = 'identity') +
   theme_minimal() + 
   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(
     title = 'Odonata occ records per country',
     x = 'Odonata')  +
   geom_text(aes(x=countryCode, y=N_sum, label= N_sum),
             hjust = -0.5, size = 3,
             position = position_dodge(width = 1))+
   coord_flip() + 
   theme_bw() 
 
   ggsave("odonata_60.png",
          plot = last_plot(),
          width = 18,
          height = 10,
          units = "in") 
   
   ggsave("odonata_135.png",
          plot = last_plot(),
          width = 13,
          height = 10,
          units = "in") 
   
 
 library(gridExtra)
 grid.arrange(flip, odonata) 
 
###################################
 
 test = rotifera_all %>%
   group_by(ISO_A2) %>%
   summarise(gbifid = n()) %>%
   ggplot(aes(x = (ISO_A2), y = gbifid)) +
   geom_bar(stat = "identity") +
   theme(axis.text.x=element_text(angle = -90, hjust = 0))
 
test2 +
  geom_text(aes(label=Number), position=position_dodge(width=0.9), vjust=-0.25)


### Geom_counbt
rotifera_all %>% 
  group_by(ISO_A2) %>%
  summarise(gbifid = n()) %>%
  ggplot(aes(y=gbifid, x=ISO_A2, colour = gbifid, group=ISO_A2)) +
  geom_count(alpha=0.5)


 
 
 