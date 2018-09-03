# ------------------ Effect of client abundance ------------------------ #

# Directories --------------------------------------------------------------

projDir<-"d:/quinonesa/learning_models_c++/actCrit/"
simsDir<-"s:/quinonesa/Simulations/actCrit/InitVal1_/"


# libraries ----------------------------------------------------------------
source('d:/quinonesa/Dropbox/R_files/posPlots.R')
source(paste(projDir,"aesth_par.R",sep=""))
source(paste(projDir,"loadData.R",sep = ""))
library('plotrix')
# library('lme4')


# Load data ------------------------------------------------------------

setwd(simsDir)
# Define data to be loaded 

listParRuns<-c("AbundanceLpr")
listValRuns<-c(1)

getParam(simsDir,listParRuns,listValRuns)


FIAlast<-do.call(rbind,lapply(getFilelist(simsDir,listParRuns,listValRuns)$FIA, 
         function(x){
           if(as.numeric(gsub("[[:alpha:]]",
                              strsplit(x,"_")[[1]][7],
                              replacement=""))==
              as.numeric(gsub("[[:alpha:]]",
                              strsplit(x,"_")[[1]][8],
                              replacement=""))){
             file2lastProp(x,0.9)
             }}))




FIAlast[,pA:=1-pR-pV]

FIA.stats<-FIAlast[,.(meanProb=mean(Prob.RV.V),
                           upIQR=fivenum(Prob.RV.V)[4],
                           lowIQR=fivenum(Prob.RV.V)[2])
                        ,by=.(Neta,Gamma,pR,pV,Outbr,pA)]


# Plots -----------------------------------------------------------------------

FIA.stats[,posit:=ifelse(Gamma==0&Neta==0,0,
                         ifelse(Gamma==0.8&Neta==0,0.01,
                                ifelse(Gamma==0&Neta==1,0.02,0.03)))]

png("d:/quinonesa/Dropbox/Neuchatel/Figs/Actor_critic/Fig4_panel1.png",
width = 700 , height = 1200)

par(plt=posPlot(numplotx = 1,numploty = 1,1,1),las=1)
with(FIA.stats,{
  plotCI(x = (1-pA)+posit,
         y = meanProb,ui = upIQR
         ,li = lowIQR,
         col=colboxes[ifelse(Gamma==0.8,
                             ifelse(Neta==1,1,2),
                             ifelse(Neta==1,3,4))],
         pch=16,xlab="",ylab="",
         sfrac=0.008,yaxt='s',
         cex.axis=1.3,cex=2,cex.lab=3)
  mtext('Proportion of visitors chosen over residents',2,line = 4, cex=3,las=0)
  mtext('Overall client abundance',1,line = 5,las=1,cex=4)
  # text(x=par('usr')[1]+0.05*(par('usr')[2]-par('usr')[1]),
  #      y=par('usr')[3]+0.95*(par('usr')[4]-par('usr')[3]),
  #      labels='A',cex=2)
})

legend(x=0.6,y=0.65,
       legend=c("neg. reward + future", "future",
                "neg. reward","no neg. reward + no future"),
       col=colboxes,pch=15,cex=1.5,ncol=1)

dev.off()
