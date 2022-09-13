 
library(tidyverse)
library(zoo)

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


 
 
 