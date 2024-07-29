library(ggplot2)
library(emmeans)
library(ggeffects)
DDT<-read.csv("~/Projects/kdr_funestus_report_2023/tables/DDT_kdr.csv")

#models for plotting DDT
model976<-glm(Death~L967F, family=binomial, data=DDT)
model976_1<-glm(Death~1, family=binomial, data=DDT)
anova(model976_1, model976, test="Chisq")
summary(model976)

#tiff("FigureL976F_DDT", units="in", width=5, height=5, res=300)
prediction976<- ggemmeans(model976, terms=c("L967F"))
ggplot(NULL, aes())+
  geom_pointrange(data= prediction976, aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high), size=1)+
  ylim(0,1) +
  ggtitle("DDT") +
  xlab(bquote('L967F')) +
  ylab(bquote('Mortality')) +
  ylim(0,1) +
  theme_classic(base_family='Arial', base_size = 18)+
guides(color = FALSE)
#dev.off()
prediction976$insec <- "DDT"

model1842<-glm(Death~P1842S, family=binomial, data=DDT)
model1842_1<-glm(Death~1, family=binomial, data=DDT)
anova(model1842_1, model1842, test="Chisq")
summary(model1842)

#tiff("FigureP1842S_DDT", units="in", width=5, height=5, res=300)
prediction1842<- ggemmeans(model1842, terms=c("P1842S"))
ggplot(NULL, aes())+
  geom_pointrange(data= prediction1842, aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high), size=1)+
  ylim(0,1) +
  ggtitle("DDT") +
  xlab(bquote('P1842S')) +
  ylab(bquote('Mortality')) +
  ylim(0,1) +
  theme_classic(base_family='Arial', base_size = 18)+
  guides(color = FALSE)
#dev.off()

#Now Deltametrin
# Read the CSV file
Delta967 <- read.csv("~/Projects/kdr_funestus_report_2023/tables/Deltamethrin_967.csv")

model976Delta<-glm(Death~L967F, family=binomial, data=Delta967)
summary(model976Delta)


prediction976Delta<- ggemmeans(model976Delta, terms=c("L967F"))
prediction976Delta$x<- factor(prediction976Delta$x, levels = c("kdr", "heterozygote", "wt"))
prediction976Delta$insec <- "Deltamethrin"

tiff("FigureL976F_Deltamethrin", units="in", width=5, height=5, res=300)
ggplot(NULL, aes())+
  geom_pointrange(data= prediction976Delta, aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high), size=1)+
  ylim(0,1) +
  ggtitle("Deltamethrin") +
  xlab(bquote('L967F')) +
  ylab(bquote('Mortality')) +
  ylim(0,1) +
  theme_classic(base_family='Arial', base_size = 18)+
  guides(color = FALSE)
dev.off()


Delta1842 <- read.csv("~/Projects/kdr_funestus_report_2023/tables/Deltamethrin_1842.csv")

model1842Delta<-glm(Death~P1842S, family=binomial, data=Delta1842)
summary(model1842Delta)


prediction1842Delta<- ggemmeans(model1842Delta, terms=c("P1842S"))
prediction1842Delta$x<- factor(prediction1842Delta$x, levels = c("kdr", "heterozygote", "wt"))

tiff("FigureP1842S_Deltamethrin", units="in", width=5, height=5, res=300)
ggplot(NULL, aes())+
  geom_pointrange(data= prediction1842Delta, aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high), size=1)+
  ylim(0,1) +
  ggtitle("Deltamethrin") +
  xlab(bquote('P1842S')) +
  ylab(bquote('Mortality')) +
  ylim(0,1) +
  theme_classic(base_family='Arial', base_size = 18)+
  guides(color = FALSE)
dev.off()

prediction976Delta$x<- factor(prediction976Delta$x, levels = c("kdr", "heterozygote", "wt"))
prediction976$x<- factor(prediction976$x, levels = c("kdr", "heterozygote", "wt"))

kdrdf<- rbind(prediction976, prediction976Delta) 
ggplot(NULL, aes())+
  geom_pointrange(data =kdrdf, aes(x=x,y=predicted, ymin=conf.low,ymax=conf.high, colour=insec),position = position_dodge(width=0.2), size=1)+
  #ggtitle("DDT") +
  xlab(bquote('L976S')) +
  ylab(bquote('Mortality')) +
  ylim(0,1) +
  labs(colour='Insecticide')+
  theme_classic(base_family='Arial', base_size = 18)
  #guides(color = FALSE)



prediction1842$insec <- "DDT"
prediction1842Delta$insec <- "Deltamethrin"
deltadf <- rbind(prediction1842, prediction1842Delta)
deltadf$x <- factor(deltadf$x, levels = c("kdr", "heterozygote", "wt"))
ggplot(NULL, aes())+
  geom_pointrange(data =deltadf, aes(x=x,y=predicted, ymin=conf.low,ymax=conf.high, colour=insec),position = position_dodge(width=0.2), size=1)+
  #ggtitle("DDT") +
  xlab(bquote('P1842S')) +
  ylab(bquote('Mortality')) +
  ylim(0,1) +
  labs(colour='Insecticide')+
  theme_classic(base_family='Arial', base_size = 18)
#guides(color = FALSE)

deltadf$Mutation <- 'P1842S'
kdrdf$Mutation <- 'L976F'
mutdf <- rbind(deltadf, kdrdf)


tplot <- ggplot(NULL, aes())+
  geom_pointrange(data =mutdf, aes(x=x,y=predicted, ymin=conf.low,ymax=conf.high, colour=Mutation, shape=insec),position = position_dodge(width=0.2), size=1)+
  #ggtitle("DDT") +
  xlab(bquote('P1842S')) +
  ylab(bquote('Mortality')) +
  ylim(0,1) +
  labs(colour='Mutation',)+
  scale_colour_manual(values=c("#2ca25f","#2b8cbe"))+
  theme_classic(base_family='Arial', base_size = 18)+
  facet_grid(cols=vars(Mutation))+
  theme(strip.text.x = element_blank(),
        plot.title = element_text(size=30),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        axis.line = element_blank(),
        )+
  labs(shape="Insecticide")+
  ggtitle("C")
ggsave(plot = last_plot(), filename = "~/Projects/kdr_funestus_report_2023/figures/assoc.tiff", width = 8, height = 4)
