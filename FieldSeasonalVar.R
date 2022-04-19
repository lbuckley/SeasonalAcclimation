#figure of seasonal variation in thermal tolerance
library(ggplot2)
library(dplyr)
library(viridis)

setwd("/Volumes/GoogleDrive/My Drive/Buckley/Work/SeasonalAcclimation/data/")
ct= read.csv("FieldData.csv")

#assign DOY
months= c("January", "February","March","April","May","June","July","August","September","October","November","December")
doys=c(15,46,74, 105, 135, 166, 196, 227, 258, 288, 319, 349)

inds= which(is.na(ct$DOY))
match1= match(ct$Month[inds], months)
ct$DOY[inds]= doys[match1] 

#make factor
ct$SpecPop= ct$Species
inds= which(!is.na(ct$Population))
ct$SpecPop= as.factor(paste(ct$Species, ct$Population, sep=" "))

#omit LT50
ct= ct[-which(ct$Metric=="lower LT50"),]

#normalize to min
ct$group= paste(ct$SpecPop, ct$Metric, sep=" ")

ct2=ct[,c("group","Value")] %>%
  group_by(group) %>%
  mutate_all(funs(.-min(.))) 
ct$norm= ct2$Value

#include groups with move than two values
ct=ct %>%
  group_by(group) %>%
  filter(n()>2)

#adjust seasons
ct$DOYn= ct$DOY
ct$DOYn[which(ct$Hemisphere=="s")]= ct$DOYn[which(ct$Hemisphere=="s")] -182.5
ct$DOYn[which(ct$Hemisphere=="s" & ct$DOYn<0)]= ct$DOYn[which(ct$Hemisphere=="s" & ct$DOYn<0)] +365
#restrict to N
ct= ct[-which(ct$Hemisphere=="s"),]

#code aquatic and terrestrial
ct$Habitat="terrestrial"
ct$Habitat[which(ct$Taxa %in% c("crayfish","fish","sea urchin","stingray","trout"))] <-"aquatic"

#update metric names
ct$Metric[ct$Metric=="CTmax"]<-"Critical thermal maximum"
ct$Metric[ct$Metric=="CTmin"]<-"Critical thermal minimum"
ct$Metric= factor(ct$Metric, levels=c("Critical thermal minimum","Critical thermal maximum"))

fig= ggplot(data=ct, aes(x=DOYn, y = norm, color=Taxa, group=SpecPop, lty=Habitat)) + 
  geom_point()+geom_line()+  #geom_smooth(method="loess", se=FALSE)+
  facet_wrap(~Metric, ncol=2)+theme_bw()+
  ylab("Seasonal change in temperature (°C)") +xlab("Day of year")+
  scale_color_viridis(discrete=TRUE, option="turbo")+
  scale_linetype_manual(values=c("dashed","solid"))

#plot
setwd("/Volumes/GoogleDrive/My Drive/Buckley/Work/SeasonalAcclimation/figures/")
pdf("SeasonalTolerance.pdf", height = 4, width = 8)
fig
dev.off()


tiff("SeasonalTolerance.tiff", units="in", width=8, height=4, res=300)
fig
dev.off()
