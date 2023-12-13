###plot final selection scans
library(ggsci)
library(tidyverse)

#read g123
g123_list <- list.files('~/Projects/kdr_funestus_report_2023/g123_data_admin1/', '700.csv', full.names = TRUE)
g123_data  <- lapply(g123_list, function(x) data.table(fread(x), 'name' = basename(x))) #read data

g123_data <- do.call(rbind, g123_data)
g123_data$name <- gsub('_700.csv','',g123_data$name)
palvec <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#CAB2D6","#6A3D9A")

selscan <- ggplot(g123_data, aes(x=pos, y=g123, colour=name))+
  geom_line()+
  theme_classic()+
  labs(x='Position (bp)',y='G123', colour='Region')+
  facet_wrap(~name, ncol = 1)+
  scale_y_continuous(breaks = c(0, 0.5,1))+
  scale_color_manual(values = palvec)+
  annotate("segment", x = 44125475, xend = 44125476, y = 0, yend = 1, colour="#252525",alpha=0.3,linetype=2, linewidth=1)+
  #scale_colour_brewer(palette='Paired')+
  theme(legend.position = "bottom",
        strip.text.x = element_blank())
selscan
ggsave(filename = '~/Projects/kdr_funestus_report_2023/figures/G123_scans.tiff', device = 'tiff', width = 200, height = 200, units = 'mm')
