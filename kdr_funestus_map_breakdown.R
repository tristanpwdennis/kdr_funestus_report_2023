#######
#plot nice map and table for kdr sample counts
#tristan dennis 22.11.23
#######

library(tidyverse)
library(sf)
library(RColorBrewer)
library("ggrepel")
library("ggspatial")
library("ggsci")


#read sample information (this is )
df_samples = fread('~/Projects/kdr_funestus_report_2023/kdr_funestus_tz_samples.csv')



#load shapefiles
gridshp = st_read('~/software/feems/feems/feems/data/grid_100.shp')
admin_shape = st_read("~/Projects/cease/cease_wp1c/analysis_data/raster/ne_10m_admin_1_states_provinces/ne_10m_admin_1_states_provinces.shp")
border_shape = st_read("~/Projects/cease/cease_wp1c/analysis_data/raster/ne_10m_admin_0_sovereignty/ne_10m_admin_0_sovereignty.shp")
river_shape = st_read("~/Projects/cease/cease_wp1c/analysis_data/raster/ne_50m_rivers_lake_centerlines/ne_50m_rivers_lake_centerlines.shp")
lake = st_read("~/Projects/cease/cease_wp1c/analysis_data/raster//ne_50m_lakes/ne_50m_lakes.shp")
city_shape = st_read("~/Projects/cease/cease_wp1c/analysis_data/raster/ne_50m_populated_places/ne_50m_populated_places.shp")
ocean_shape = st_read("~/Projects/cease/cease_wp1c/analysis_data/raster/ne_50m_ocean/ne_50m_ocean.shp")
urban_shape = st_read("~/Projects/cease/cease_wp1c/analysis_data/raster/ne_50m_urban_areas/ne_50m_urban_areas.shp")

X<-city_shape[(city_shape$FEATURECLA == 'Admin-1 capital' | city_shape$NAME == 'Dodoma') & city_shape$SOV_A3 == 'TZA',]

tzdf <- df_samples %>% select(cohort_admin1_year, longitude, latitude) %>% unique()

#tzdf <- data.frame(
#admin1_rep = c("TZ-19_fune_2019_08","TZ-16_fune_2019_02","TZ-08_fune_2019_04","TZ-25_fune_2019_04","TZ-16_fune_2019_02","TZ-21_fune_2019_05","TZ-16_fune_2017_08","TZ-16_fune_2021","TZ-05_fune_2022","TZ-17_fune_2022"),
#admin1_rep_long = c("-6.385","-7.979","-3.294","-5.227","-8.358","-11.071","-8.273","-7.984","-1.080","-10.538"),
#admin1_rep_lat = c("38.568","36.813","30.294","38.660","36.710","37.502","36.681","36.818","31.376","40.279")
#)


##########
#plot nice map of sample locations over TZ
##########

map <- ggplot()+
 geom_sf(data = border_shape, fill = ifelse(border_shape$ADM0_A3 == "TZA", gray(0.98), gray(0.85)),col =gray(0.7)) + # geom_sf(data=st_geometry(river_shape), colour = '#4a80f5', fill='#9bbff4', alpha=0.7)+
#  geom_sf(data=st_geometry(ocean_shape), colour = '#4a80f5', fill='#9bbff4')+
  geom_sf(data=st_geometry(lake),colour = '#4a80f5', fill='#9bbff4')+
  geom_point(data=tzdf, aes(x=as.numeric(longitude), y=as.numeric(latitude), colour=cohort_admin1_year),  alpha = 0.8, size=4)+#colour=query))+#size=sequenced_count))+
  geom_point(data = X, aes(x = LONGITUDE, y = LATITUDE))+
  geom_text_repel(data = X, aes(x = LONGITUDE, y = LATITUDE, label = NAME), fontface = "bold", min.segment.length = 0.4, max.overlaps = 1000)+ 
  #geom_text_repel(data = tzdf, aes(x = longitude, y = latitude, label = cohort_admin1_year), fontface = "bold", min.segment.length = 0.4, max.overlaps = 1000)+ 
  #scale_colour_manual(values = countrycols)+
  xlim(30, 40.5)+
  ylim(-11.6,-0.8)+
  labs(x="Longitude", y="Latitude", colour='Admin1_Cohort_Year')+
  theme_minimal()+
  annotation_scale(location = "bl", width_hint = 0.5) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#9bbff4"),
        panel.grid.major = element_line(colour = gray(0.5), linetype = "dashed"),
        panel.border = element_rect(fill = NA),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 14, face = "bold"), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        #legend.position = "bottom", 
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13, face = "bold")) +
  guides(fill = guide_legend(title = "Country", override.aes = list(size = 5)))

ggsave('~/Projects/kdr_funestus_report_2023/figures/map.tiff', plot=map, width = 200, height = 200, units = 'mm', device = 'tiff')


#############
#write supp table of samples included in sequencing


#############

#write table of samples broken down by location, year, count
count(df_samples,admin1_name, year, cohort_admin1_year) %>% 
  kable("html", align = "c", col.names = c('Admin. Region','Year','Cohort ID','N. Samples'), padding=-1L, digits=3) %>% 
  kable_styling("striped") 

#write table of samples broken down by location, year, count
count(df_samples,admin1_name, location, year, cohort_admin1_year) %>% 
  kable("html", align = "c", col.names = c('Admin. Region','Location','Year','Cohort ID','N. Samples'), padding=-1L, digits=3) %>% 
  kable_styling("striped") 
