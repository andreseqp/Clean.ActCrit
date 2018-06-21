# --------------------------------- Time intervals -------------------------------------------------#

projDir<-"d:/quinonesa/learning_models_c++/actCrit/"
simsDir<-"s:/quinonesa/Simulations/actCrit/"

# libraries ---------------------------------------------------------------------------------------
source('d:/quinonesa/Dropbox/R_files/posPlots.R')
source(paste(projDir,"aesth_par.R",sep=""))
source(paste(projDir,"loadData.R",sep = ""))
library('plotrix')

# Load Data ---------------------------------------------------------------------------------------


# Define data to be loaded 

(listPar<-rep("BaseLine",1))
(listVal<-"")
(param<-getParam(simsDir,listparam = listPar,values = listVal))

#diffJsons(param[1],param[3])

# Load interval data for FIA from the raw data
FIAtimeInt<-do.call(
  rbind,lapply(
    getFilelist(simsDir,listPar,listVal)$FIA,
    file2timeInter,interV=1001))

# Load FIA data from processed file

# getFilelist(simsDir,listPar,listVal)$FIA

# FIAtimeInt<-do.call(
#   rbind,lapply(getFilelist(projDir,listPar,listVal)$FIA,fread))



# Load interval data for PIA from the raw data
PIAtimeInt<-do.call(
  rbind,lapply(
    getFilelist(simsDir,listPar,listVal)$PIA,
    file2timeInter,interV=1001))

# Load PIA data from processed file

# PIAtimeInt<-do.call(
#   rbind,lapply(getFilelist(genDir,listPar,listVal)$PIA,fread))

# Load DP data from the raw data
# DPdataProb<-do.call(rbind,
#                     lapply(getFilelist(simsDir,listPar,listVal)$DP,
#                            file2lastDP))

# Load DP data from processed file

# DPdataprob<-do.call(
#   rbind,lapply(getFilelist(genDir,listPar,listVal)$DP,fread))

# Plot the dynamics of VR choice -----------------------------------------------------------

extpar<-listPar[1]

FIAIntstats<-FIAtimeInt[,.(meanProb=mean(Prob.RV.V),
                           upIQR=fivenum(Prob.RV.V)[4],
                           lowIQR=fivenum(Prob.RV.V)[2])
                        ,by=.(Interv,Neta,Outbr,Tau,Gamma)]
setnames(FIAIntstats,'get',extpar)
PIAIntstats<-PIAtimeInt[,.(meanProb=mean(Prob.RV.V),
                           upIQR=fivenum(Prob.RV.V)[4],
                           lowIQR=fivenum(Prob.RV.V)[2])
                        ,by=.(Interv,Neta,Outbr,Tau,Gamma)]
setnames(PIAIntstats,'get',extpar)

FIAIntstats[,posit:=ifelse(Gamma==0&Neta==0,0,
                           ifelse(Gamma==0.8&Neta==0,0.1,
                             ifelse(Gamma==0&Neta==1,0.2,0.3)))]
PIAIntstats[,posit:=ifelse(Gamma==0&Neta==0,0,
                           ifelse(Gamma==0.8&Neta==0,0.1,
                                  ifelse(Gamma==0&Neta==1,0.2,0.3)))]

png("d:/quinonesa/Dropbox/Neuchatel/Figs/Actor_critic/Fig1.png",width = 1200,
    height = 800)

par(plt=posPlot(numplotx = 2,idplotx = 1),yaxt='s',las=1)
with(FIAIntstats,{
  plotCI(x=Interv+posit,y=meanProb,
         ui = upIQR,li=lowIQR,
         pch=16,xlab='',ylab='',
         col=colboxes[ifelse(Gamma==0.8,
                             ifelse(Neta==1,1,2),
                             ifelse(Neta==1,3,4))],
         sfrac=0.002,cex.axis=1.3,ylim=c(0.4,1))
  mtext(side = 2,text = "frequency of V over R",las=0,cex=2,line=4)
  lines(x=c(0,max(Interv)),y=c(0.5,0.5),col='grey')
  text(x=par('usr')[2]*0.1,y=par('usr')[3]+0.9*(par('usr')[4]-par('usr')[3]),
       labels='FAA',cex=1.5)
  # lines(x=c(0,max(Interv)),y=c(0.8,0.8),col='grey')
})

# with(DPdataProb,  
#      {matlines(x = t(matrix(rep(max(FIAtimeInt$Interv)*c(0.75,1),
#                                 each=length(RV.V)),length(RV.V))),
#                y=t(matrix(rep(probRV.V,2),length(RV.V))),
#                lwd=2,lty = "dashed",
#                col=colboxes[match(Gamma,unique(Gamma))])})

par(plt=posPlot(numplotx = 2,idplotx = 2),
    new=TRUE,yaxt='s',xpd=TRUE)
with(PIAIntstats,{
  plotCI(x=Interv+posit,y=meanProb,
         ui = upIQR,li=lowIQR,
         pch=16,xlab='',ylab='',
         col=colboxes[ifelse(Gamma==0.8,
                             ifelse(Neta==1,1,2),
                             ifelse(Neta==1,3,4))],
         sfrac=0.002,cex.axis=1.3,yaxt='n',ylim=c(0.4,1))
  lines(x=c(0,max(Interv)),y=c(0.5,0.5),col='grey')
  text(x=par('usr')[2]*0.1,y=par('usr')[3]+0.9*(par('usr')[4]-par('usr')[3]),
       labels='PAA',cex=1.5)
})
text(x = -2,y=0.32,labels = "Time",cex=2)
legend('topright',
       legend=c("neg. reward + future", "future",
                "neg. reward","no neg. reward + no future"),
       col=colboxes,pch=15,cex=1.5,ncol=1)

# png(filename = paste(projDir,eval(extpar),".png",sep=""))

dev.off()

# Replicating Olle's simulations -----------------------------------------------

colOlle<-c("red","green")
colOlle2<-c("blue","black")

cexpar<-1.5

png(filename = "d:/quinonesa/Dropbox/Neuchatel/Olle/Sarsa_Olle_par.png",
    width = 800,height = 800)

par(plt=posPlot(numplotx = 1,idplotx = 1),yaxt='s',las=1)
with(FIAIntstats[Neta==0.5&factRew==2],{
  plotCI(x=Interv,y=meanProb,
         ui = upIQR,li=lowIQR,
         pch=16,xlab='Time',ylab='Prob. V over R',cex.lab=2,
         col=colOlle[match(Gamma,unique(Gamma))],
         sfrac=0.002,cex.axis=1.3,ylim=c(0,1),cex=cexpar)
  lines(x=c(0,max(Interv)),y=c(0.5,0.5),col='grey')
})
par(new=TRUE)
with(FIAIntstats[Neta==0&factRew==1],{
  plotCI(x=Interv,y=meanProb,
         ui = upIQR,li=lowIQR,
         pch=16,xlab='',ylab='',
         col=colOlle2[match(Gamma,unique(Gamma))],
         sfrac=0.002,cex.axis=1.3,ylim=c(0,1),cex=cexpar)
  lines(x=c(0,max(Interv)),y=c(0.5,0.5),col='grey')
})

with(DPdataProb[Neta==0.5&factRew==2],  
     {matlines(x = t(matrix(rep(max(FIAtimeInt$Interv)*c(0.75,1),
                                each=length(RV.V)),length(RV.V))),
               y=t(matrix(rep(probRV.V,2),length(RV.V))),
               lwd=2,lty = "dashed",cex=1.2,
               col=colOlle[match(Gamma,unique(Gamma))])})

with(DPdataProb[Neta==0&factRew==1],  
     {matlines(x = t(matrix(rep(max(FIAtimeInt$Interv)*c(0.75,1),
                                each=length(RV.V)),length(RV.V))),
               y=t(matrix(rep(probRV.V,2),length(RV.V))),
               lwd=2,lty = "dashed",cex=1.2,
               col=colOlle2[match(Gamma,unique(Gamma))])})

legend('bottomright',
       legend=c("Punishment and future", "punishment",
                "future","no punishment no future"),
       col=c(colOlle,colOlle2),pch=15,cex=1.5,ncol=1)

dev.off()




  
  