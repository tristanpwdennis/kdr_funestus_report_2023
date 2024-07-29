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
df_samples = read.csv('~/Projects/kdr_funestus_report_2023/metadata/df_samples.csv')

#load shapefiles
admin_shape = st_read("~/Projects/cease/cease_wp1c/analysis_data/raster/ne_10m_admin_1_states_provinces/ne_10m_admin_1_states_provinces.shp")
border_shape = st_read("~/Projects/cease/cease_wp1c/analysis_data/raster/ne_10m_admin_0_sovereignty/ne_10m_admin_0_sovereignty.shp")
river_shape = st_read("~/Projects/cease/cease_wp1c/analysis_data/raster/ne_50m_rivers_lake_centerlines/ne_50m_rivers_lake_centerlines.shp")
lake = st_read("~/Projects/cease/cease_wp1c/analysis_data/raster//ne_50m_lakes/ne_50m_lakes.shp")
ocean_shape = st_read("~/Projects/cease/cease_wp1c/analysis_data/raster/ne_50m_ocean/ne_50m_ocean.shp")

#make new lat and long
df_samples$newlong <- trunc(df_samples$longitude * 100) / 100
df_samples$newlat <- trunc(df_samples$latitude * 100) / 100
tzdf <- df_samples %>% count(cohort_admin1_year, admin1_name, newlong, newlat)

##########
#plot nice map of sample locations over TZ
##########

map <- ggplot()+
 geom_sf(data = border_shape, fill = ifelse(border_shape$ADM0_A3 == "TZA", gray(0.98), gray(0.85)),col =gray(0.7)) + # geom_sf(data=st_geometry(river_shape), colour = '#4a80f5', fill='#9bbff4', alpha=0.7)+
  geom_sf(data=st_geometry(lake),colour = '#4a80f5', fill='#9bbff4')+
  geom_point(data=tzdf[tzdf$admin1_name != "Kigoma",], aes(x=as.numeric(newlong), y=as.numeric(newlat), colour=admin1_name, size=as.numeric(n)),  alpha = 0.6, size=4)+#colour=query))+#size=sequenced_count))+
  xlim(30, 40.5)+
  ylim(-11.6,-0.8)+
  labs(x="Longitude", y="Latitude", colour='Admin1')+
  theme_minimal()+
  annotation_scale(location = "bl", width_hint = 0.5) +
  labs(x = "Longitude", y = "Latitude", colour="Region") +
  theme_bw() +
  scale_color_brewer(palette = "Paired")+
  theme(panel.background = element_rect(fill = "#9bbff4"),
        panel.grid.major = element_line(colour = gray(0.5), linetype = "dashed"),
        panel.border = element_rect(fill = NA),
        axis.text = element_text(size = 10),
        legend.position = "left",
        axis.title = element_text(size = 14, face = "bold"), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        #legend.position = "bottom", 
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13, face = "bold")) +
  guides(fill = guide_legend(title = "Country", override.aes = list(size = 5)))
ggsave('~/Projects/kdr_funestus_report_2023/figures/map.tiff', plot=map, width = 200, height = 200, units = 'mm', device = 'tiff')
map
##########
#G123 selection scans
##########

#read g123
g123_list <- list.files('~/Projects/kdr_funestus_report_2023/data/g123_data_admin1/', '700.csv', full.names = TRUE)
g123_data  <- lapply(g123_list, function(x) data.table(fread(x), 'name' = basename(x))) #read data

g123_data <- do.call(rbind, g123_data)
g123_data$name <- gsub('_700.csv','',g123_data$name)
palvec <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#CAB2D6","#6A3D9A")

selscan <- ggplot(g123_data, aes(x=pos, y=g123, colour=name))+
  geom_line()+
  geom_area(aes(fill=name, alpha=0.6)) +
  theme_classic()+
  labs(x='Position (bp)',y='G123', colour='Region')+
  facet_wrap(~name, ncol = 1)+
  scale_y_continuous(breaks = c(0, 0.5,1))+
  scale_color_manual(values = palvec)+
  scale_fill_manual(values = palvec)+
  annotate("segment", x = 44105643, xend = 44105644, y = 0, yend = 1, colour="#252525",alpha=0.1,linetype=2, linewidth=1)+
  #scale_colour_brewer(palette='Paired')+
  theme(legend.position = "none",
        strip.text.x = element_blank())
selscan
#plot map and selection scans
library(patchwork)
map+ ggtitle('A')+ theme(plot.title = element_text(size=22))+
  selscan+ ggtitle('B')+ theme(plot.title = element_text(size=22))
ggsave()
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
#heatmap of allele freqs
##########

df_freqs <- fread('~/Projects/kdr_funestus_report_2023/tables/kdr_vgsc_aa_freqs.csv')
df_freqs_2 <- pivot_longer(df_freqs, cols=4:17)
df_freqs_2$name <- gsub('frq_','',df_freqs_2$name)
loc_df <- df_samples %>% select(cohort_admin1_year, admin1_name, year) %>% unique()
df_freqs_2 <- left_join(df_freqs_2, loc_df, by=c('name' = 'cohort_admin1_year'))
df_freqs_2$label <- factor(df_freqs_2$label, levels = c('L976F (3RL:44,125,475 T>A)','G1144C (3RL:44,122,391 C>A)','W1557R (3RL:44,117,167 A>T)','F1638Y (3RL:44,116,923 A>T)','N1773S (3RL:44,116,334 T>C)','P1842S (3RL:44,116,128 G>A)','G1962C (3RL:44,115,768 C>A)','I2030V (3RL:44,115,564 T>C)'))
plotheatmap <- function(admin){
  #plot initial heatmap to get labs and legend
  ggplot(df_freqs_2[df_freqs_2$admin1_name == admin,], aes(y=label, x=as.factor(year), fill=value))+
    geom_tile(colour='white', linewidth=0)+
    scale_fill_gradientn(colours = c("#f7fbff", '#9ecae1','#4292c6'), breaks = c(0,0.5,1), limits = c(0,1))+
    geom_text(aes(label=round(value, digits = 2)), color = "#08306b", size = 4, fontface = "bold") +
    theme_classic()+
    facet_grid(~admin1_name)+
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(), 
          axis.ticks.y = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_text(size=15,face="bold"),
          axis.text.y=element_text(size=15,face="bold"),
          strip.text.x = element_text(size=15,face="bold"),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing = element_blank(),
          
          plot.margin = margin(r = 0.05, l = 0.05))
          #axis.text.y = element_blank())
}

listofheatmaps <- lapply(unique(df_freqs_2$admin1_name), plotheatmap)

#have had to wrangle this to get a multipanelled heatmap without faffing around in inkscape
library(patchwork)
heatmaplot <- 
listofheatmaps[[2]]+scale_x_discrete(labels = "2022\n n=32")+  ggtitle("A")+ theme(plot.title = element_text(size=30))+
listofheatmaps[[6]]+theme(axis.text.y = element_blank(), axis.title.y = element_blank())+scale_x_discrete(labels = "2022\n n=28")+
listofheatmaps[[9]]+theme(axis.text.y = element_blank(), axis.title.y = element_blank())+scale_x_discrete(labels = "2022\n n=32")+
listofheatmaps[[1]]+scale_x_discrete(labels = "2022\n n=28")+theme(axis.text.y = element_blank(), axis.title.y = element_blank())+
listofheatmaps[[10]]+theme(axis.text.y = element_blank(), axis.title.y = element_blank())+scale_x_discrete(labels = "2022\n n=33")+ 
listofheatmaps[[7]]+theme(axis.text.y = element_blank(), axis.title.y = element_blank())+scale_x_discrete(labels = "2019\n n=10")+
listofheatmaps[[4]]+theme(axis.text.y = element_blank(), axis.title.y = element_blank())+scale_x_discrete(labels = c("2017\n n=10","2019\n n=18","2021\n n=9","2023\n n=27"))+
listofheatmaps[[3]]+theme(axis.text.y = element_blank(), axis.title.y = element_blank())+scale_x_discrete(labels = "2022\n n=30")+
listofheatmaps[[5]]+theme(axis.text.y = element_blank(), axis.title.y = element_blank())+scale_x_discrete(labels = "2022\n n=31")+
listofheatmaps[[8]]+theme(axis.text.y = element_blank(), axis.title.y = element_blank())+scale_x_discrete(labels = c("2019\n n=9","2022\n n=28"))+
plot_layout(widths =  c(1.2,1.2,1.2,1.2,1.2,1.2,4,1.2,1.2,2))             

heatmaplot
ggsave(plot=heatmaplot, filename = "~/Projects/kdr_funestus_report_2023/figures/heatmaplot.tiff", device = 'tiff', width = 15, height = 6, units = "in")


####
#allele frequencies
####

af <- fread("~/Projects/kdr_funestus_report_2023/tables/kdr_freqs.csv", header = TRUE)
s <- af %>% pivot_longer(cols = 2:5)
aflplot <- s %>% filter(V1 != 'W1557R') %>% 
  pivot_wider(names_from = val) %>% 
  ggplot(aes())+
  geom_pointrange(aes(x=name,y=freq, ymin=ci_low,ymax=ci_up, colour=V1))+
  geom_line(aes(x=name,y=freq, group=V1, colour=V1))+
  theme_classic(base_family='Arial', base_size = 18)+
  scale_colour_manual(values=c("#2ca25f","#2b8cbe"))+
  theme(axis.line = element_blank(),
        plot.title = element_text(size=30),
        legend.position = "bottom")+
  labs(x='Year',y='Frequency', colour='Mutation')+
  ggtitle("B")
aflplot  
ggsave(plot=aflplot+theme(legend.position = "none"), filename = "~/Projects/kdr_funestus_report_2023/figures/frefig.tiff", device = 'tiff', width = 5, height = 5, units = "in")
cowplot::plot_grid(aflplot+theme(legend.position = "none"), tplot+theme(legend.position = "right"))
##########
#heatmap of LD
##########

#get matrix labels sorted
headerlabs <- rev(c('snp2','L976F','G1144C','W1557R','F1638Y','N1773S','P1842S','G1962C','I2030V'))
snp2labs <- rev(c('L976F','G1144C','W1557R','F1638Y','N1773S','P1842S','G1962C','I2030V'))
ldmat <- read.csv('~/Projects/kdr_funestus_report_2023/data/kdrld.csv', header = FALSE)
ldmat$snp2 <- snp2labs
colnames(ldmat) <- headerlabs
snp2labs
#pivot to longfdorm df for plotting
lddf <- ldmat %>% pivot_longer(cols = 1:8)
lddf$snp2 <- factor(lddf$snp2, levels = snp2labs)
lddf$name <- factor(lddf$name, levels = snp2labs)

ldfig <- lddf %>% 
  ggplot(aes(x=name, y=snp2, fill=value))+
  geom_tile(colour='white', linewidth=0)+
  scale_fill_gradientn(colours = c("#f7fbff", '#9ecae1','#4292c6'), breaks = c(0,0.5,1), limits = c(-0.1,0.99))+
  geom_text(aes(label=round(value, digits = 2)), color = "#08306b", size = 4, fontface = "bold") +
  scale_y_discrete(position = "right")+
  theme_classic()+
  labs(fill = 'Rogers and Huff R')+
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(), 
          axis.ticks.y = element_blank(),
          plot.title = element_text(size=30),
          axis.line = element_blank(),
          strip.text.x = element_text(size=15,face="bold"),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing = element_blank(),
          plot.margin = margin(r = 0.1, l = 0.1),
          axis.text.x = element_text(size=15,face="bold",angle = 90,hjust=1),
          axis.text.y = element_text(size=15,face="bold",angle = 0, vjust = 0.1, hjust=1))+
  ggtitle("D")
ldfig
ggsave(plot=last_plot(), filename = "~/Projects/kdr_funestus_report_2023/figures/ld_figure.tiff", device = 'tiff', width = 8, height = 8, units = "in")


