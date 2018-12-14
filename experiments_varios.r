# ---- Comparison of differnt experimentals scenarios------------------------------------------#
#                       

# directory where source files are saved
projDir<-"d:/quinonesa/learning_models_c++/actCrit/"
# directory where simulation output is stored
simsDir<-"s:/quinonesa/Simulations/actCrit/Experiments_/"

# libraries ---------------------------------------------------------------------------------------

# external file to locate plots in the plotting region
source('d:/quinonesa/Dropbox/R_files/posPlots.R')

# aesthetic parameters 
source(paste(projDir,"aesth_par.R",sep=""))
# funtions to load data
source(paste(projDir,"loadData.R",sep = ""))
library('plotrix')

# Load Data ---------------------------------------------------------------------------------------

setwd(simsDir)

# Define data to be loaded 

(listPar<-rep("scen",4))
(listVal<-c(0,1,2,3))
(param<-getParam(simsDir,listparam = listPar,values = listVal))


# Load interval data for FAA from the raw data
FAAtime<-do.call(
  rbind,lapply(
    grep("alphaThNch1",getFilelist(simsDir,listPar,listVal)$FAA,value=TRUE),
    file2timeInter,interV=1001))

# Load interval data for PAA from the raw data

PAAtime<-do.call(
  rbind,lapply(
    grep("alphaThNch1_",getFilelist(simsDir,listPar,listVal)$PAA,value=TRUE),
    file2timeInter,interV=1001))


# Calculate statistics on the interval data ------------------------------------

extpar<-listPar[1]

FAAIntstats<-FAAtime[,.(meanProb=mean(Prob.RV.V),
                                             upIQR=fivenum(Prob.RV.V)[4],
                                             lowIQR=fivenum(Prob.RV.V)[2])
                                ,by=.(Interv,Neta,Outbr,Gamma,get(extpar))]
setnames(FAAIntstats,'get',extpar)

PAAIntstats<-PAAtime[,.(meanProb=mean(Prob.RV.V),
                                             upIQR=fivenum(Prob.RV.V)[4],
                                             lowIQR=fivenum(Prob.RV.V)[2])
                                ,by=.(Interv,Neta,Outbr,Gamma,get(extpar))]

setnames(PAAIntstats,'get',extpar)

# Compute position variable that allows to see the "treatments"

FAAIntstats[,posit:=ifelse(Gamma==0&Neta==0,0,
                               ifelse(Gamma==0.8&Neta==0,0.1,
                                      ifelse(Gamma==0&Neta==1,0.2,0.3)))]
PAAIntstats[,posit:=ifelse(Gamma==0&Neta==0,0,
                               ifelse(Gamma==0.8&Neta==0,0.1,
                                      ifelse(Gamma==0&Neta==1,0.2,0.3)))]


# Plot the dynamics of VR choice -----------------------------------------------------------

# png("d:/quinonesa/Dropbox/Neuchatel/Figs/Actor_critic/Fig_base_exp.png",
# width = 1200,height = 800)

netaPar<-0
gammaPar<-0.8

par(plt=posPlot(numplotx = 2,numploty = 1,idplotx = 1,idploty = 1)+
      c(-0.05,-0.05,0,0),
    yaxt='s',las=1)
with(FAAIntstats[Gamma==gammaPar&Neta==netaPar],{
  plotCI(x=Interv,y=meanProb,
         ui = upIQR,li=lowIQR,
         pch=16,xlab='',ylab='',xaxt="s",
         col=colboxes[scen+1],
         sfrac=0.002,cex.axis=1.3,ylim=c(0.4,1))
  lines(x=c(0,max(Interv)),y=c(0.5,0.5),col='grey')
  text(x=par('usr')[1]+0.05*(par('usr')[2]-par('usr')[1]),
       y=par('usr')[3]+0.9*(par('usr')[4]-par('usr')[3]),
       labels='A',cex=1.5)
  # lines(x=c(0,max(Interv)),y=c(0.8,0.8),col='grey')
})
legend('bottomright',
       legend=c('Nature','Experiment','Market Experiment','Extended market'),
       col=colboxes,pch=15,cex=1.5,ncol=1)

par(plt=posPlot(numplotx = 2,numploty = 1,idplotx = 2,idploty = 1)+
      c(-0.05,-0.05,0,0),
    new=TRUE,yaxt='s',xpd=TRUE)
par(plt=posPlot())
with(PAAIntstats[Gamma==gammaPar&Neta==netaPar],{
  plotCI(x=Interv+0.1*scen,y=meanProb,
         ui = upIQR,li=lowIQR,
         pch=16,xlab='',ylab='',xaxt="n",
         col=colboxes[scen+1],
         sfrac=0.002,cex.axis=1.3,yaxt='n')
  lines(x=c(0,max(Interv)),y=c(0.5,0.5),col='grey')
  text(x=par('usr')[1]+0.05*(par('usr')[2]-par('usr')[1]),
       y=par('usr')[3]+0.9*(par('usr')[4]-par('usr')[3]),
       labels='B',cex=1.5)
  axis(2)
})



