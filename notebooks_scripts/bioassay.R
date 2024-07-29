#Insecticide resistance bioassay modelling 
#Joel O. Odero 
#updated - 28.03.2024


#installing packages
# install.packages('car')
# install.packages('arm')
# install.packages('coefplot')
# install.packages('mgcv')
# install.packages('lme4')
# install.packages('Matrix')
# install.packages('lattice')
# install.packages('lmerTest')
# install.packages('boot')
# install.packages('MASS')
 install.packages('emmeans')
 install.packages('ggeffects')
# install.packages('magrittr')
# install.packages('tidyverse')
# install.packages('ggstance')
# install.packages('lmtest')
# install.packages("dplyr")
# install.packages('mgcv')

#load libraries
library(lme4)
library(tidyverse)
library(car)
library(arm)
library(coefplot)
library(mgcv)
library(dplyr)
library(Matrix)
library(lattice)
library(lmerTest)
library(boot)
library(MASS)
library(emmeans)
library(ggeffects)
library(magrittr)
library(ggstance)
library(lmtest)
library(mgcv)
library(visreg)


setwd("~/Projects/kdr_funestus_report_2023/notebooks_scripts/R_analysis_genotyping")
dataf <- read.csv('~/Projects/kdr_funestus_report_2023/notebooks_scripts/bioassay.1.csv', sep=",", header=T, check.names = FALSE)
summary(dataf)
head(dataf)
View(dataf)

#Models
names(dataf)

dataf$Insecticide<-as.factor(dataf$Insecticide)
dataf$Insecticide<-relevel(dataf$Insecticide, ref="control")
model0 <- glmer(cbind(Dead, Alive) ~  Insecticide*Region  + (1|Insecticide:Region :Replicate) + RH + 
                   Temp , data = dataf, family = binomial)

#model1 <- glmer(cbind(Dead, Alive) ~  Insecticide + Region  + (1|Insecticide:Region :Replicate) + RH + 
#                  Temp , data = dataf, family = binomial)

#anova(model0, model1, test='Chisq') #interaction p=7.004e-13, chisq 187.4

#model2 <- glmer(cbind(Dead, Alive) ~  Insecticide*Region  + (1|Insecticide:Region :Replicate) +
#                  Temp , data = dataf, family = binomial)
#anova(model0, model2, test='Chisq') #p=0.6296,chisq=0.2326 RH

model3 <- glmer(cbind(Dead, Alive) ~  Insecticide*Region  + (1|Insecticide:Region :Replicate) , data = dataf, family = binomial)
#anova(model3, model2, test='Chisq') #p=0.4345, chisq=0.6108 Temp
#model4 <- glmer(cbind(Dead, Alive) ~  Insecticide + Region  + (1|Insecticide:Region :Replicate) , data = dataf, family = binomial)
#anova(model3, model4, test='Chisq') #p=2.2e-16 *** chisq=229.88 

#model predictions using ggemmeans
prediction_model3 <-ggemmeans(model3, terms=c('Insecticide', 'Region')) 

view(prediction_model3)
library(ggeffects)

#group.colors <- c(Kagera = "#E69F00", Katavi = "#56B4E9", Lindi ="#CC79A7", Morogoro = "#009E73", Mtwara = "#F0E442", Mwanza ='black' , Pwani ='#0072B2' , Ruvuma ='#D55E00' , Tanga ='#00AFBB')

DDT.colors <- c(Kagera = "#1f78b4", Katavi = "#b2df8a", 
                  Lindi ="#33a02c", Morogoro = "#fb9a99", 
                  Mtwara = "#e31a1c", Mwanza ='#fdbf6f' , 
                  Pwani ='#ff7f00' , Ruvuma ='#cab2d6', 
                  Tanga ='#6a3d9a' )
#################################################################################################################
#plotting predicted mortality pyrethroids DC and intensity
#selected_items <- prediction_model3[prediction_model3$x %in% c("Deltamethrin", "Deltamethrin 5x", 'Deltamethrin 10x', 'Permethrin', 'Permethrin 5x', 'Permethrin 10x', "Deltamethrin + PBO"), ]

#selected_items <- prediction_model3[prediction_model3$x %in% c("Deltamethrin", "Deltamethrin + PBO"),]
                                    
#bioassay.prediction <- ggplot(selected_items, aes(x = group, y = predicted, color = group)) +
#  geom_boxplot(width = 0.6, position = position_dodge(0.8)) +
#  geom_hline(yintercept =  0.98, linetype = "dashed", color = 'black')+
#  geom_hline(yintercept =  0.90, linetype = "dashed", color = 'red')+
#  theme(text = element_text(size =18, face = 'bold'))+
#  theme(axis.text.x =  element_text(size =12,face = 'bold',angle =0))+
#  theme(axis.text.y =  element_text(size =12,face = 'bold', angle =0))+
#  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(0.8)) +
#  labs(title = "", x = '', y = "Mortality") +
#  theme_classic ()+
#  ylim(0, NA)+
#  facet_wrap('x', ncol = 2)+
#  scale_color_manual(values = group.colors)+
#  labs(color = "Region")+
#  scale_x_discrete(labels = NULL)+
#  theme(legend.position = "bottom", legend.direction = "horizontal")
  
#bioassay.prediction 

# save the plot in a local folder
#ggsave("bioassay.PY+PBO.png", bg = 'white', plot = bioassay.prediction, dpi=300, 
#       width =180, height= 140, units = "mm")
##################################################################################################
#plotting predicted mortality deltamethrin and PBO

#selected_items <- prediction_model3[prediction_model3$x %in% c("Deltamethrin", "Deltamethrin + PBO"), ]

#bioassay.prediction <- ggplot(selected_items, aes(x = group, y = predicted, color = group)) +
#  geom_boxplot(width = 0.6, position = position_dodge(0.8)) +
#  geom_hline(yintercept =  0.98, linetype = "dashed", color = 'black')+
#  geom_hline(yintercept =  0.90, linetype = "dashed", color = 'red')+
#  theme(text = element_text(size =18, face = 'bold'))+
#  theme(axis.text.x =  element_text(size =12,face = 'bold',angle =0))+
#  theme(axis.text.y =  element_text(size =12,face = 'bold', angle =0))+
#  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(0.8)) +
#  labs(title = "", x = '', y = "Mortality") +
#  theme_classic ()+
#  ylim(0, NA)+
#  facet_wrap('x', ncol = 2)+
#  scale_color_manual(values = group.colors)+
#  labs(color = "District")+
#  scale_x_discrete(labels = NULL)+
#  theme(legend.position = "bottom", legend.direction = "horizontal")

#bioassay.prediction 

# save the plot in a local folder
#ggsave("bioassay.Delt.PBO.png", bg = 'white', plot = bioassay.prediction, dpi=300, 
#       width =150, height= 140, units = "mm")

##########################################################################################################
#plotting predicted mortality DDT, bendiocarb, pirimiphos

#selected_items <- prediction_model3[prediction_model3$x %in% c("Bendiocarb", "DDT", 'Pirimiphos-methyl'), ]

selected_items <- prediction_model3[prediction_model3$x %in% c( "DDT"), ]

bioassay.prediction <- ggplot(selected_items, aes(x = group, y = predicted, color = group)) +
  geom_boxplot(width = 0.6, position = position_dodge(0.8)) +
  geom_hline(yintercept =  0.98, linetype = "dashed", color = 'black')+
  geom_hline(yintercept =  0.90, linetype = "dashed", color = 'red')+
  theme(text = element_text(size =18, face = 'bold'))+
  theme(axis.text.x =  element_text(size =12,face = 'bold',angle =0))+
  theme(axis.text.y =  element_text(size =12,face = 'bold', angle =0))+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(0.8)) +
  labs(title = "", x = '', y = "Mortality") +
  theme_classic ()+
  ylim(0, NA)+
  theme_classic(base_family='Arial', base_size = 18)+
  facet_wrap('x', ncol = 2)+
  scale_color_manual(values = DDT.colors)+
  theme(strip.text.x = element_blank(),
        axis.line = element_blank())+
  labs(color = "Region")+
  scale_x_discrete(labels = NULL)+
  theme(legend.position = "none")

bioassay.prediction 

# save the plot in a local folder
ggsave("~/Projects/kdr_funestus_report_2023/figures/bioassay.IRS.DDT.png", bg = 'white', plot = bioassay.prediction, dpi=300, 
       width =7, height= 4)
