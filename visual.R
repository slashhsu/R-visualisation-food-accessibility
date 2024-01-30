rm(list=ls())
rm(LSOA)


install.packages("viridisLite")
library(tidyverse)
library(ggmap)
library(ggplot2)
library(plyr)
library(dplyr)
library(tmap)
library(viridisLite)

food<-read.csv("priority_places_for_food_oct22.csv",head=TRUE)
pop<-read.csv("TS006-2021-2-filtered.csv",head=TRUE)
geo<-read.csv("infuse_lsoa_lyr_2011.csv",head=TRUE)

mix_pop<-left_join(food,pop,by=c('geo_code'='Lower.Layer.Super.Output.Areas.Code'))
mix_geo<-left_join(food,geo,by=c('geo_code'='geo_code'))
mix_geonew<-left_join(mix_geo,pop,by=c('geo_code'='Lower.Layer.Super.Output.Areas.Code'))

##// UKmap with priority food
library(WDI)
library(sf)
library(RColorBrewer)
LSOA <- st_read("ew_lsoa_2021.shp")
lsoa_sf<-read_sf("ew_lsoa_2021.shp")
LSOA <- left_join(LSOA, pop, by=c('lsoa21cd'='Lower.Layer.Super.Output.Areas.Code'))
LSOA <- left_join(LSOA, mix_geo,  by=c('lsoa21cd'='geo_code'))
tm_shape(LSOA) +
  tm_polygons("Observation", title = "pop density", palette = "Oranges", style = "fisher") +
  tm_layout(legend.title.size = 0.8)+
  tm_scale_bar(position = c("RIGHT", "BOTTOM"))

tm_shape(LSOA) +
  tm_polygons("pp_dec_combined", title = "food priority", palette = "-Reds", style = "fisher") +
  tm_layout(legend.title.size = 0.8)+
  tm_scale_bar(position = c("RIGHT", "BOTTOM"))

tm_shape(LSOA) +
  tm_polygons("pp_dec_domain_socio_demographic", title = "poverty", palette = "-Reds", style = "fisher") +
  tm_layout(legend.title.size = 0.8)+
  tm_scale_bar(position = c("RIGHT", "BOTTOM"))

##// UKmap with priority food in lon and lat

ggplot() + geom_polygon(data = mix_geo, aes(x = x, y = y, group = country, fill = pp_dec_combined),
                        color = "#FFFFFF", size = 0.25)+ coord_fixed(1)
mix_geo%>% ggplot() + geom_sf(lwd = .2,color = 'black', aes(fill = n)) +
                          theme_void()  + scale_fill_viridis_c(option = 'plasma') + 
                          labs(title = "Priority Places for Food Index",  fill = 'priority food index') + 
                          theme(legend.position = 'left') +
                          theme(title = element_text(size = 12),  legend.title = element_text(size = 8))
##// poverty and shopping online, store, supermarket


ggplot(food,
       aes(x = domain_socio_demographic,
           y = combined,
           color = country)) +
  geom_point(size = 2.5) +
  scale_x_log10() +
  facet_wrap(~ country)+labs(x = "poverty",
                             y = "comnied indes for food prority",
                             color = "Cylinders",title = "poverty with combined index")+
  geom_mark_rect(aes(filter = domain_socio_demographic < 10),
                 description = "priority for food supply")

##// population density

ggplot(mutate(mix, cty = factor(country))) +
  geom_point(aes(x = Observation,
                 y = combined,
                 fill = cty),
             shape = 21,
             size = 3) +
  theme(legend.position = "top",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14,
                                  face = "bold"))+labs(x = "Population Density", y = "comnied indes for food prority",
      title = "Density with combined index")

##// pop density


ggplot(data = mix_pop, mapping = aes(x = Observation, y = combined)) +
  geom_point() + geom_abline(colour = "red")


## group by combined index

country<-group_by(food,country)
Index_country<-summarise(country,
                      count=n(),
                      avg_com=mean(combined),
                      avg_com=mean(combined) 
                      )
ggplot(data=food,x=country, y=combined, fill=country)+geom_bar(x=country, y=combined,stat = "identity", position = position_dodge()) +scale_fill_brewer(palette = "Paired")
ggplot(data = Index_country, aes(x = country, y = avg_com, fill = country)) +
  geom_bar(stat = "identity")+ scale_fill_brewer(palette = "Dark2")+labs(x = "Country", y = "average combined index",
                                                                         title = "Combined index in each country")
