#### Figures showing the fit between model and data ############################

library(data.table)
library(ggplot2)
library("RColorBrewer")
library("patchwork") 
library(ggpubr)
library(here)
library(cowplot)
source(here("loadData.R"))
source(here("data2interp.R"))
require('akima')
source(here("aesth_par.R"))
source(here("..","R_files",'ternaryAEQP.R'))
source(here("../R_files/posPlots.R"))


scenario<-"MCMCclean_gam_Nrew_sca"
scenario2<-"MCMCclean_gam_sca"

predfileMode.both<-grep(".txt",grep("round",list.files(here("Simulations",paste0(scenario,"_"))),value = T),
                    value = T)
predfileMode.gam<-grep(".txt",grep("round",list.files(here("Simulations",paste0(scenario2,"_"))),value = T),
                   value = T)

predfileSamples.both<-grep(".txt",grep("round",list.files(here("Simulations",
                                                               paste0(scenario,"_"),"samplesPost_"),
                                                          full.names = TRUE)
                                       ,value = T),
                         value = T)

predfileSamples.gam<-grep(".txt",grep("round",list.files(here("Simulations",
                                                               paste0(scenario2,"_"),"samplesPost_"),
                                                          full.names = TRUE)
                                       ,value = T),
                           value = T)


predictDataMode.both<-fread(here("Simulations",paste0(scenario,"_"),
                      predfileMode.both))
predictDataMode.gam<-fread(here("Simulations",paste0(scenario2,"_"),
                             predfileMode.gam))

predictDataSamps.both<-do.call(rbind,lapply(predfileSamples.both,fread))
predictDataSamps.both[,id_samp:=rep(1:100,each=120)]

predictDataSamps.gam<-do.call(rbind,lapply(predfileSamples.gam,fread))
predictDataSamps.gam[,id_samp:=rep(1:100,each=120)]




fieldatabyLoc.both<-predictDataMode.both[,.(probVisi.data=mean(visitorChoices)/20,
                              probvisitor.pred=max(visitorChoices_pred),
                              re.abund.clean=max(rel.abund.cleaners),
                            prob.Vis.leave=max(prob.Vis.Leav)),by=site_year]

fieldatabyLoc.gam<-predictDataMode.gam[,.(probVisi.data=mean(visitorChoices)/20,
                                   probvisitor.pred=max(visitorChoices_pred),
                                   re.abund.clean=max(rel.abund.cleaners),
                                   prob.Vis.leave=max(prob.Vis.Leav)),by=site_year]

fieldatabyLocSamps.both<-predictDataSamps.both[,.(probVisi.data=mean(visitorChoices)/20,
                                        probvisitor.pred=max(visitorChoices_pred),
                                        re.abund.clean=max(rel.abund.cleaners),
                                        prob.Vis.leave=max(prob.Vis.Leav)),
                                        by=.(site_year,id_samp)]

fieldatabyLocSamps.gam<-predictDataSamps.gam[,.(probVisi.data=mean(visitorChoices)/20,
                                      probvisitor.pred=max(visitorChoices_pred),
                                      re.abund.clean=max(rel.abund.cleaners),
                                      prob.Vis.leave=max(prob.Vis.Leav)),
                                      by=.(site_year,id_samp)]



fieldatabyLoc.both
fieldatabyLoc.gam

## Obvx vs Pred colour by location from cleaner data

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0,0.8))


obs.both<-ggplot(data = fieldatabyLoc.both, 
            aes(x=re.abund.clean, y=prob.Vis.leave,color=probVisi.data))+
  geom_point(size=3)+theme_classic()+ sc

pred.both<-ggplot(data = fieldatabyLoc.both, 
             aes(x=re.abund.clean, y=prob.Vis.leave,color=probvisitor.pred))+
  geom_point(size=3)+theme_classic()+ sc

obs.gam<-ggplot(data = fieldatabyLoc.gam, 
                 aes(x=re.abund.clean, y=prob.Vis.leave,color=probVisi.data))+
  geom_point(size=3)+theme_classic()+ sc

pred.gam<-ggplot(data = fieldatabyLoc.gam, 
                  aes(x=re.abund.clean, y=prob.Vis.leave,color=probvisitor.pred))+
  geom_point(size=3)+theme_classic()+ sc


plot_grid(obs.both,pred.both)
          #,obs.gam,pred.gam



 ## Obv vs predicted two scatter ------------------------------------------------

obs.pred.both<-ggplot(data = fieldatabyLoc.both, aes(x=probvisitor.pred,y=probVisi.data))+
    geom_point(data = fieldatabyLocSamps.both,aes(x=probvisitor.pred,y=probVisi.data),
               color='grey')+
  geom_point()+theme_classic()+geom_abline(slope = 1)+
  lims(x=c(0.45,0.8),y=c(0.45,0.8))+
  labs(title = "Full model")

obs.pred.gam<-ggplot(data = fieldatabyLoc.gam, aes(x=probvisitor.pred,y=probVisi.data))+
  geom_point()+theme_classic()+geom_abline(slope = 1)+lims(x=c(0.45,0.8),y=c(0.45,0.8))+
  labs(title = expression(gamma))

obs.pred.both | obs.pred.gam

## Obv vs predicted contour ----------------------------------------------------

simsDir.both<-here("Simulations",paste0(scenario,"_"))

# simsDir <- paste0("e:/NeuchSims/AC/",paste0(scenario,"_"))

# list.files(here("Simulations",paste0(scenario,"_")),
#            recursive = TRUE,full.names = TRUE)

(listPar<-c("gamma","neta"))

(listVal<-c(0.2624,0))

FIAlastQuarData<-do.call(rbind,lapply(
  getFilelist(simsDir.both,fullNam = TRUE)$p1,file2lastProp,0.70,outPar="Vlp",
  full.path=TRUE))


FIAlastQuarData[,pA:=1-pR-pV]

FIA.stats<-FIAlastQuarData[,.(meanProb=mean(Prob.RV.V),
                              upIQR=fivenum(Prob.RV.V)[4],
                              lowIQR=fivenum(Prob.RV.V)[2])
                           ,by=.(Neta,Gamma,pR,pV,Vlp)]

FIA.stats$pA<-round(1-FIA.stats$pR-FIA.stats$pV,1)

pointInt<-200

FIAinterpData.both<-AbundLeavData2interp(FIAlastQuarData,
                                    Var2int = "Prob.RV.V",npoints = pointInt)

colorbreaksMeans<-seq(0.3,0.8,length=100)


# simsDir <- paste0("e:/NeuchSims/AC/",paste0(scenario,"_"))

simsDir.gam<-here("Simulations",paste0(scenario2,"_"))

(listPar<-c("gamma","neta"))

(listVal<-c(0.2624,0))

FIAlastQuarData<-do.call(rbind,lapply(
  getFilelist(simsDir.gam,fullNam = TRUE)$p1,file2lastProp,0.70,outPar="Vlp",
  full.path=TRUE))


FIAlastQuarData[,pA:=1-pR-pV]

FIA.stats<-FIAlastQuarData[,.(meanProb=mean(Prob.RV.V),
                              upIQR=fivenum(Prob.RV.V)[4],
                              lowIQR=fivenum(Prob.RV.V)[2])
                           ,by=.(Neta,Gamma,pR,pV,Vlp)]

FIA.stats$pA<-round(1-FIA.stats$pR-FIA.stats$pV,1)

pointInt<-200

FIAinterpData.gam<-AbundLeavData2interp(FIAlastQuarData,
                                    Var2int = "Prob.RV.V",npoints = pointInt)

colorbreaksMeans<-seq(0.3,0.8,length=100)



png(here("Simulations",paste0(scenario,"_"),"contour_pred_obs.png"),width = 800,height = 800)

par(plt=c(posPlot(numplotx = 7,idplotx = 1)[1],
          posPlot(numplotx = 7,idplotx = 5)[2:4]))
with(FIAinterpData.both,{
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
points(x=fieldatabyLoc.both$re.abund.clean,y = fieldatabyLoc.both$prob.Vis.leave,
       col=paletteMeans(100)[findInterval(fieldatabyLoc.both$probVisi.data,
                                          colorbreaksMeans)]
       ,cex=3,pch=20)
points(x=fieldatabyLoc.both$re.abund.clean,y = fieldatabyLoc.both$prob.Vis.leave,
       col="black"
       ,cex=3.2,pch=1)

par(plt=c(posPlot(numplotx = 7,idplotx = 4)[1],
          posPlot(numplotx = 7,idplotx = 6)[2:4]),new=TRUE)
with(FIAinterpData.gam,{
  plot(pAbs,VLeavProb,
       col = paletteMeans(100)[findInterval(Prob.RV.V,
                                            colorbreaksMeans)],
       main="",cex=1.3,cex.lab = 2,pch=20,xlab="",
       ylab="",yaxt="n")
})

points(x=fieldatabyLoc.gam$re.abund.clean,y = fieldatabyLoc.gam$prob.Vis.leave,
       col=paletteMeans(100)[findInterval(fieldatabyLoc.gam$probVisi.data,
                                          colorbreaksMeans)]
       ,cex=3,pch=20)
points(x=fieldatabyLoc.gam$re.abund.clean,y = fieldatabyLoc.gam$prob.Vis.leave,
       col="black"
       ,cex=3.2,pch=1)

color.bar.aeqp(paletteMeans(100),min =0.3,#round(min(Prob.RV.V),2),
               max = 0.8,#round(max(Prob.RV.V),2),
               nticks = 3,
               title = "Probability of \n V over R",cex.tit = 1,numplotx = 10,
               numploty = 10,idplotx = 10,idploty = 6)

dev.off()

## COntour with GG

names(FIAinterpData.both)<-c("rel.abund.cleaners","prob.Vis.Leav","market_binomial_data")
names(fieldatabyLoc.both)[c(4,5,2)]<-c("rel.abund.cleaners","prob.Vis.Leav","market_binomial_data")


names(FIAinterpData.gam)<-c("rel.abund.cleaners","prob.Vis.Leav","market_binomial_data")
names(fieldatabyLoc.gam)[c(4,5,2)]<-c("rel.abund.cleaners","prob.Vis.Leav","market_binomial_data")


predictDataMode.both[,
                                         log.like:=dbinom(visitorChoices,
                                size = 20,prob = visitorChoices_pred,log = TRUE)]

predictDataMode.gam[,
                     log.like:=dbinom(visitorChoices,
                                      size = 20,prob = visitorChoices_pred,log = TRUE)]


rsqr.both.McFadden<-1-predictDataMode.both[,sum(log.like)]/
  sum(dbinom(x=predictDataMode.both[,visitorChoices],size = 20,prob = 0.5,log = TRUE))

rsqr.gam.McFadden<-1-predictDataMode.gam[,sum(log.like)]/
  sum(dbinom(x=predictDataMode.gam[,visitorChoices],size = 20,prob = 0.5,log = TRUE))



cont.obs.pred.both<- ggplot(data = FIAinterpData.both,aes(x=rel.abund.cleaners,y=prob.Vis.Leav,
                                                fill=market_binomial_data))+
 geom_raster(interpolate = TRUE) +  
  scale_fill_gradientn(limits=c(0.3,1),colours= myPalette(100))+theme_classic()+
  geom_point(data = fieldatabyLoc.both,aes(fill=market_binomial_data),size=5,
             shape=21,color="black")+sc+xlab("")+
  ylab("Probability of visitor leaving")+
  labs(fill="Probability \n of choosing \n a visitor")+
  theme(axis.text = element_text(size=12),
axis.title.x = element_text(size=12),axis.title.y = element_text(size=12))

scatter.obs.pred.both<-ggplot(data = fieldatabyLoc.both,aes(y=market_binomial_data,x=probvisitor.pred))+
  geom_point(data = fieldatabyLocSamps.both,aes(x=probvisitor.pred,y=probVisi.data),
             color='grey')+
  geom_point(size=4)+ylim(0.4,0.9)+xlim(0.4,0.9)+
  geom_abline(slope=1)+ylab("Observed")+xlab("Predicted")+
  ggtitle("")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
      axis.title.x = element_text(size=12),axis.title.y = element_text(size=12),
      axis.text = element_text(size=14))+
  geom_text(x = 0.8, y = 0.85, label = expression(y==x), parse = TRUE,size=4)+
  geom_text(x = 0.8, y = 0.5, label = deparse(bquote(R^2==.(round(rsqr.both.McFadden,3)))), 
            parse = TRUE,size=4)

cont.obs.pred.gam<- ggplot(data = FIAinterpData.gam,aes(x=rel.abund.cleaners,y=prob.Vis.Leav,
                                                          fill=market_binomial_data))+
  geom_raster(interpolate = TRUE) +  
  scale_fill_gradientn(limits=c(0.3,1),colours= myPalette(100))+theme_classic()+
  geom_point(data = fieldatabyLoc.gam,aes(fill=market_binomial_data),size=5,
             shape=21,color="black")+sc+xlab("Relative cleaner abundance")+
  ylab("Probability of visitor leaving")+
  labs(fill="Probability \n of choosing \n a visitor")+
  theme(axis.text = element_text(size=12),
        axis.title.x = element_text(size=12),axis.title.y = element_text(size=12))

scatter.obs.pred.gam<-ggplot(data = fieldatabyLoc.gam,aes(y=market_binomial_data,x=probvisitor.pred))+
  geom_point(data = fieldatabyLocSamps.gam,aes(x=probvisitor.pred,y=probVisi.data),
             color='grey')+
  geom_point(size=4)+ylim(0.45,0.9)+xlim(0.45,0.9)+
  geom_abline(slope=1)+ylab("Observed")+xlab("Predicted")+
  ggtitle("")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size=12),axis.title.y = element_text(size=12),
        axis.text = element_text(size=14))+
  geom_text(x = 0.8, y = 0.85, label = expression(y==x), parse = TRUE,size=4)+
  geom_text(x = 0.8, y = 0.5, label = deparse(bquote(R^2==.(round(rsqr.gam.McFadden,3)))), 
            parse = TRUE,size=4)

# png(here("Simulations",paste0(scenario,"_"),
    #      paste0(strsplit(predfileMode.both,"seed")[[1]][1],"contour_gamma_ggplot.png")),
    # width = 1300,height = 700)

png(here("contour_both_gamma.png"),width = 1300,height = 1000)


ggarrange(cont.obs.pred.both,scatter.obs.pred.both,
cont.obs.pred.gam,scatter.obs.pred.gam,
          labels=c('a','b','c','d'),common.legend=TRUE,legend = "top")

dev.off()




