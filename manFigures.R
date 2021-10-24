#### Figures showing the fit between model and data ############################

library(data.table)
library(ggplot2)
library("RColorBrewer")
library("patchwork") 
library(here)
source(here("loadData.R"))
source(here("data2interp.R"))
require('akima')
source(here("aesth_par.R"))
source(here("..","R_files",'ternaryAEQP.R'))
source(here("../R_files/posPlots.R"))


scenario<-"MCMCfakedata"


predfile<-grep("round",list.files(here("Simulations",paste0(scenario,"_"))),value = T)

predictData<-fread(here("Simulations",paste0(scenario,"_"),
                      predfile))

str(predictData)

fieldatabyLoc<-predictData[,.(probVisi.data=mean(visitorChoices)/20,
                              probvisitor.pred=max(visitorChoices_pred),
                              re.abund.clean=max(rel.abund.cleaners),
                            prob.Vis.leave=max(prob.Vis.Leav)),by=site_year]

fieldData

## Obv vs predicted two panels with point size ---------------------------------

field.size<-ggplot(data = predictData, 
       aes(x=rel.abund.cleaners, y=prob.Vis.Leav,size=visitorChoices/20))+
  geom_point()+theme_classic()

sims.size<-ggplot(data = predictData, 
                   aes(x=rel.abund.cleaners, y=prob.Vis.Leav,size=visitorChoices_pred))+
  geom_point()+theme_classic()

field.size | sims.size

##  Obv vs predicted by location from cleaner data

field.size<-ggplot(data = fieldatabyLoc, 
                   aes(x=re.abund.clean, y=prob.Vis.leave,size=probVisi.data))+
  geom_point()+theme_classic()

sims.size<-ggplot(data = fieldatabyLoc, 
                  aes(x=re.abund.clean, y=prob.Vis.leave,size=probvisitor.pred))+
  geom_point()+theme_classic()


field.size | sims.size


## Obv vs predicted two panels with colour--- ---------------------------------

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0,0.8))

obs<-ggplot(data = predictData, 
       aes(x=rel.abund.cleaners, y=prob.Vis.Leav,color=market_binomial_data))+
  geom_point(size=3)+theme_classic()+ sc

pred<-ggplot(data = predictData, 
             aes(x=rel.abund.cleaners, y=prob.Vis.Leav,color=market_binomial_pred))+
  geom_point(size=3)+theme_classic()+ sc

obs | pred

## Obvx vs Pred by location from cleaner data

obs<-ggplot(data = fieldatabyLoc, 
            aes(x=re.abund.clean, y=prob.Vis.leave,color=probVisi.data))+
  geom_point(size=3)+theme_classic()+ sc

pred<-ggplot(data = fieldatabyLoc, 
             aes(x=re.abund.clean, y=prob.Vis.leave,color=probvisitor.pred))+
  geom_point(size=3)+theme_classic()+ sc

obs | pred



## Obv vs predicted two scatter ------------------------------------------------

ggplot(data = predictData, aes(x=market_binomial_pred,y=market_binomial_data))+
  geom_point()+theme_classic()+geom_abline(slope = 1)


ggplot(data = fieldatabyLoc, aes(x=probvisitor.pred,y=probVisi.data))+
    geom_point()+theme_classic()+geom_abline(slope = 1)


## Obv vs predicted contour ----------------------------------------------------


simsDir <- here("Simulations",paste0(scenario,"_"))

# simsDir <- paste0("e:/NeuchSims/AC/",paste0(scenario,"_"))

list.files(simsDir,recursive = TRUE,full.names = TRUE)

(listPar<-c("gamma","neta"))

(listVal<-c(0.2624,0))

FIAlastQuarData<-do.call(rbind,lapply(
  getFilelist(simsDir,fullNam = TRUE)$p1,file2lastProp,0.70,outPar="Vlp",
  full.path=TRUE))


range(fieldatabyLoc$prob.Vis.leave)
range(FIAlastQuarData$Vlp)

FIAlastQuarData[,pA:=1-pR-pV]

FIA.stats<-FIAlastQuarData[,.(meanProb=mean(Prob.RV.V),
                              upIQR=fivenum(Prob.RV.V)[4],
                              lowIQR=fivenum(Prob.RV.V)[2])
                           ,by=.(Neta,Gamma,pR,pV,Vlp)]

FIA.stats$pA<-round(1-FIA.stats$pR-FIA.stats$pV,1)

pointInt<-200

FIAinterpData<-AbundLeavData2interp(FIAlastQuarData,
                                    Var2int = "Prob.RV.V",npoints = pointInt)

colorbreaksMeans<-seq(0.3,0.8,length=100)


png(here("Simulations",paste0(scenario,"_"),"contour_pred_obs.png"),width = 800,height = 800)

par(plt=c(posPlot(numplotx = 4,idplotx = 1)[1],
          posPlot(numplotx = 4,idplotx = 3)[2:4]))
with(FIAinterpData,{
  plot(pAbs,VLeavProb,
       col = paletteMeans(100)[findInterval(Prob.RV.V,
                                            colorbreaksMeans)],
       main="",cex=1.3,cex.lab = 2,pch=20,xlab="",
       ylab="Visitor leaving probability")
})
# dev.off()
# points(x=predictData$rel.abund.cleaners,y = predictData$prob.Vis.Leav,
#        col=paletteMeans(100)[findInterval(predictData$market_binomial_data,
#                                          colorbreaksMeans)]
         # ,cex=3,pch=20)
points(x=fieldatabyLoc$re.abund.clean,y = fieldatabyLoc$prob.Vis.leave,
       col=paletteMeans(100)[findInterval(fieldatabyLoc$probVisi.data,
                                          colorbreaksMeans)]
       ,cex=3,pch=20)
points(x=fieldatabyLoc$re.abund.clean,y = fieldatabyLoc$prob.Vis.leave,
       col="black"
       ,cex=3.2,pch=1)

color.bar.aeqp(paletteMeans(100),min =0.3,#round(min(Prob.RV.V),2),
               max = 0.8,#round(max(Prob.RV.V),2),
               nticks = 3,
               title = "Probability of \n V over R",cex.tit = 1,numplotx = 6,
               numploty = 5,idplotx = 6,idploty = 4)

dev.off()


names(FIAinterpData)<-c("rel.abund.cleaners","prob.Vis.Leav","market_binomial_data")

names(fieldatabyLoc)[c(4,5,2)]<-c("rel.abund.cleaners","prob.Vis.Leav","market_binomial_data")

png(here("Simulations",paste0(scenario,"_"),
         paste0(strsplit(predfile,"seed")[[1]][1],"contour_ggplot.png")),width = 1300,height = 700)


fieldatabyLoc[,resids:=probVisi.data-probvisitor.pred]

rsqr<-1-(fieldatabyLoc[,sum(resids^2)])/
  fieldatabyLoc[,sum((probVisi.data-mean(probVisi.data))^2)]


cont.obs.pred<- ggplot(data = FIAinterpData,aes(x=rel.abund.cleaners,y=prob.Vis.Leav,
                                                fill=market_binomial_data))+
 geom_raster(interpolate = TRUE) +  
  scale_fill_gradientn(limits=c(0.3,1),colours= myPalette(100))+theme_classic()+
  geom_point(data = fieldatabyLoc,aes(fill=market_binomial_data),size=5,
             shape=21,color="black")+sc+xlab("Relative cleaner abundance")+
  ylab("Probability of visitor leaving")+
  labs(fill="Probability \n of choosing \n a visitor")+
  theme(axis.text = element_text(size=14),
axis.title.x = element_text(size=16),axis.title.y = element_text(size=16))

scatter.obs.pred<-ggplot(data = fieldatabyLoc,aes(y=market_binomial_data,x=probvisitor.pred))+
  geom_point(size=2)+ylim(0.45,0.8)+xlim(0.45,0.8)+
  geom_abline(slope=1)+ylab("Observed")+xlab("Predicted")+
  ggtitle("Probability of choosing a visitor")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
      axis.title.x = element_text(size=16),axis.title.y = element_text(size=16),
      axis.text = element_text(size=14))+
  geom_text(x = 0.5, y = 0.775, label = expression(y==x), parse = TRUE,size=6)+
  geom_text(x = 0.51, y = 0.75, label = deparse(bquote(R^2==.(round(rsqr,2)))), parse = TRUE,size=6)


cont.obs.pred|scatter.obs.pred

dev.off()




