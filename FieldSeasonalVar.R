#figure of seasonal variation in thermal tolerance
library(ggplot2)


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

#plot
setwd("/Volumes/GoogleDrive/My Drive/Buckley/Work/SeasonalAcclimation/figures/")
pdf("SeasonalTolerance.pdf", height = 10, width = 8)
ggplot(data=ct, aes(x=DOY, y = Value, color=Taxa, lty=Hemisphere, group=SpecPop)) + 
  geom_point()+geom_line()+  #geom_smooth(method="loess", se=FALSE)+
  facet_wrap(~Metric, scales="free", ncol=1)+theme_bw()+ylab("Temperature (C)")
dev.off()