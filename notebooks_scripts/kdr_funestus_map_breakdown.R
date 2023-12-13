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
library("data.table")


#read sample information (this is )
df_samples = read.csv('~/Projects/kdr_funestus_report_2023/df_samples.csv')



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

#make new lat and long
df_samples$newlong <- trunc(df_samples$longitude * 100) / 100
df_samples$newlat <- trunc(df_samples$latitude * 100) / 100

tzdf <- df_samples %>% count(cohort_admin1_year, admin1_name, newlong, newlat)

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
  geom_point(data=tzdf[tzdf$admin1_name != "Kigoma"], aes(x=as.numeric(newlong), y=as.numeric(newlat), colour=admin1_name, size=as.numeric(n)),  alpha = 0.6, size=4)+#colour=query))+#size=sequenced_count))+
  #geom_point(data = X, aes(x = LONGITUDE, y = LATITUDE))+
  #geom_text_repel(data = X, aes(x = LONGITUDE, y = LATITUDE, label = NAME), fontface = "bold", min.segment.length = 0.4, max.overlaps = 1000)+ 
  #geom_text_repel(data = tzdf, aes(x = longitude, y = latitude, label = cohort_admin1_year), fontface = "bold", min.segment.length = 0.4, max.overlaps = 1000)+ 
  #scale_colour_manual(values = countrycols)+
  xlim(30, 40.5)+
  ylim(-11.6,-0.8)+
  labs(x="Longitude", y="Latitude", colour='Admin1')+
  theme_minimal()+
  annotation_scale(location = "bl", width_hint = 0.5) +
  labs(x = "Longitude", y = "Latitude", colour="Collection Admin1 Region") +
  theme_bw() +
  scale_color_brewer(palette = "Paired")+
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
map
ggsave('~/Projects/kdr_funestus_report_2023/figures/map.tiff', plot=map, width = 200, height = 200, units = 'mm', device = 'tiff')

unique(tzdf$admin1_name)

brewer.pal(name = "Paired", n=10)


#############
#write supp table of samples included in sequencing
#############

#write table of samples broken down by location, year, count
count(df_samples,admin1_name, year, cohort_admin1_year) %>% 
  kable("html", align = "c", col.names = c('Admin. Region','Year','Cohort ID','N. Samples'), padding=-1L, digits=3) %>% 
  kable_styling("striped") 

count(df_samples,admin1_name, year, cohort_admin1_year) %>% write_csv('~/Projects/kdr_funestus_report_2023/cohort_counts_table.csv')

#write table of samples broken down by location, year, count
count(df_samples,admin1_name, location, year, cohort_admin1_year) %>% 
  kable("html", align = "c", col.names = c('Admin. Region','Location','Year','Cohort ID','N. Samples'), padding=-1L, digits=3) %>% 
  kable_styling("striped") 

##########
#heatmap time
##########


df_samples %>% select(admin1_name, admin1_iso) %>% unique()

#mak eplotting df
df_freqs <- fread('~/Projects/kdr_funestus_report_2023/kdr_vgsc_aa_freqs.csv')
df_freqs_2 <- pivot_longer(df_freqs, cols=4:17)
df_freqs_2$name <- gsub('frq_','',df_freqs_2$name)
loc_df <- df_samples %>% select(cohort_admin1_year, admin1_name, year) %>% unique()
df_freqs_2 <- left_join(df_freqs_2, loc_df, by=c('name' = 'cohort_admin1_year'))

plotheatmap <- function(admin){
  #plot initial heatmap to get labs and legend
  ggplot(df_freqs_2[df_freqs_2$admin1_name == admin,], aes(y=label, x=as.factor(year), fill=value))+
    geom_tile(colour='white', linewidth=3)+
    scale_fill_gradientn(colours = c("#fee0d2", '#fc9272','#de2d26'), breaks = c(0,0.5,1), limits = c(0,1))+
    geom_text(aes(label=round(value, digits = 2)), color = "#636363", size = 4, fontface = "bold") +
    theme_bw()+
    facet_grid(~admin1_name)+
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_text(size=15,face="bold"),
          axis.text.y=element_text(size=15,face="bold"),
          strip.text.x = element_text(size=15,face="bold"),
          plot.margin = margin(r = 0.1, l = 0.1))
          #axis.text.y = element_blank())
}

listofheatmaps <- lapply(unique(df_freqs_2$admin1_name), plotheatmap)

library(patchwork)
listofheatmaps[[1]]+scale_x_discrete(labels = "2022\n n=28")+
listofheatmaps[[2]]+theme(axis.text.y = element_blank(), axis.title.y = element_blank())+scale_x_discrete(labels = "2022\n n=32")+
listofheatmaps[[3]]+theme(axis.text.y = element_blank(), axis.title.y = element_blank())+scale_x_discrete(labels = "2022\n n=30")+
listofheatmaps[[4]]+theme(axis.text.y = element_blank(), axis.title.y = element_blank())+scale_x_discrete(labels = c("2017\n n=10","2019\n n=18","2021\n n=9","2023\n n=27"))+
listofheatmaps[[5]]+theme(axis.text.y = element_blank(), axis.title.y = element_blank())+scale_x_discrete(labels = "2022\n n=31")+
listofheatmaps[[6]]+theme(axis.text.y = element_blank(), axis.title.y = element_blank())+scale_x_discrete(labels = "2022\n n=28")+
listofheatmaps[[7]]+theme(axis.text.y = element_blank(), axis.title.y = element_blank())+scale_x_discrete(labels = "2019\n n=10")+
listofheatmaps[[8]]+theme(axis.text.y = element_blank(), axis.title.y = element_blank())+scale_x_discrete(labels = c("2019\n n=9","2022\n n=28"))+
listofheatmaps[[9]]+theme(axis.text.y = element_blank(), axis.title.y = element_blank())+scale_x_discrete(labels = "2022\n n=32")+
listofheatmaps[[10]]+theme(axis.text.y = element_blank(), axis.title.y = element_blank())+scale_x_discrete(labels = "2022\n n=33")+ 
plot_layout(widths =  c(1.2,1.2,1.2,4,1.2,1.2,1.2,2,1,1))             

df_samples %>% count(admin1_name)
df_samples %>% count(sample_set)


cowplot::plot_grid(
  listofheatmaps[[1]]+theme(axis.text.y = element_blank(), axis.title.y = element_blank()),
  listofheatmaps[[2]]+theme(axis.text.y = element_blank(), axis.title.y = element_blank()),
  listofheatmaps[[3]]+theme(axis.text.y = element_blank(), axis.title.y = element_blank()),
  listofheatmaps[[4]]+theme(axis.text.y = element_blank(), axis.title.y = element_blank()),
  listofheatmaps[[5]]+theme(axis.text.y = element_blank(), axis.title.y = element_blank()),
  listofheatmaps[[6]]+theme(axis.text.y = element_blank(), axis.title.y = element_blank()),
  listofheatmaps[[7]]+theme(axis.text.y = element_blank(), axis.title.y = element_blank()),
  nrow=1,
  rel_widths = c(1.2,1.2,1.2,4,1.2,1.2,1.2),
  labels = c('DO','KA','LI','MO','MT','MW','PW'),
  align = "v")


#Now let's take a look at ld
labels <- c('I2030V (44,115,564)','G1962V (44,115,768)','P1842S (44,116,128)','N1773S (44,116,334)','F1638Y (44,116,923)','W1557R (44,117,167)','G1144C (44,122,391)','L976F (44,125,475)')
ldmat <- read.csv('~/Projects/kdr_funestus_report_2023/data/kdrld.csv', header = FALSE)
colnames(ldmat) <- labels
ldmat$snp2 <- labels
ldmat %>% pivot_longer(cols = 1:8) %>% 
  ggplot(aes(x=name, y=snp2, fill=value))+
  geom_tile(colour='white', linewidth=3)+
  scale_fill_gradientn(colours = c("#fee0d2", '#fc9272','#de2d26'), breaks = c(0,0.5,1), limits = c(-0.1,0.99))+
  geom_text(aes(label=round(value, digits = 2)), color = "#636363", size = 4, fontface = "bold") +
  theme_bw()+
  labs(fill = 'Rogers and Huff R')+
  theme(
        axis.title.x = element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(size=15,face="bold",angle = 45,hjust=1),
        axis.text.y = element_text(size=15,face="bold",angle = 45, vjust = 0.1, hjust=1))
        