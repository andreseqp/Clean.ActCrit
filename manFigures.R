#### Figures showing the fit between model and data ############################

library(data.table)
library(ggplot2)
library("RColorBrewer")
library("patchwork") 

fieldData<-fread(here("Data","data_ABC_site.txt"))
str(fieldData)
fieldData[,predMarket:=rep(0.5,12)]

ggplot(data = fieldData, 
       aes(x=rel.abund.cleaners, y=prob.Vis.Leav,size=market_binomial))+
  geom_point()+theme_classic()

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0,1))

obs<-ggplot(data = fieldData, 
       aes(x=rel.abund.cleaners, y=prob.Vis.Leav,color=market_binomial))+
  geom_point(size=3)+theme_classic()+ sc

pred<-ggplot(data = fieldData, 
             aes(x=rel.abund.cleaners, y=prob.Vis.Leav,color=predMarket))+
  geom_point(size=3)+theme_classic()+ sc

obs | pred

fieldData
