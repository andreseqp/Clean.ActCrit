######################## Abundance analysis ######################################

# Load libraries and external functions -----------------------------------

projDir<-"d:/quinonesa/learning_models_c++/actCrit/"
simsDir<-"s:/quinonesa/Simulations/actCrit/"

# libraries ---------------------------------------------------------------------------------------
source('d:/quinonesa/Dropbox/R_files/posPlots.R')
source(paste(projDir,"aesth_par.R",sep=""))
source(paste(projDir,"loadData.R",sep = ""))
source('D:/quinonesa/Dropbox/R_files/posPlots.R')
source('D:/quinonesa/Dropbox/R_files/vioplot.R')
source('D:/quinonesa/Dropbox/R_files/ternaryAEQP.R')
library('plotrix')
library('akima')
library("vcd")

# Delete data reset environment ---------------------------------------------------------

rm(list = ls())

rm(list=ls()[grepl('data.table', sapply(ls(), function(x) class(get(x))))])


# Useful plotting tricks ------------------------------------------------------------------

axisRangy<-c('s','n','n')
axisRangx<-c('n','n','s')
adjyaxis<-c(rep(-0.04,2),rep(-0.02,2))
lettRang<-matrix(c('A','B','C','D','E','F','G','H','I'),ncol=3,byrow = TRUE)
titY<-matrix(c('','Probability of V over R','',rep('',6)),ncol=3)
titX<-matrix(c(rep('',7),'Learning trial',''),ncol=3,byrow = TRUE)


# Load Data FIA --------------------------------------------------------------------------------------

setwd(simsDir)

(listPar<-c("Abundance"))

(listVal<-c(""))

FIAfirstReach<-do.call(rbind,lapply(
  getFilelist(simsDir,listPar,listVal)$FIA,
                                    loadDataFirstReach,0.75))

FIAlastQuarData<-do.call(rbind,lapply(
  getFilelist(simsDir,listPar,listVal)$FIA,file2lastProp,0.75))


FIA.stats<-FIAlastQuarData[,.(meanProb=mean(Prob.RV.V),
                                  upIQR=fivenum(Prob.RV.V)[4],
                                  lowIQR=fivenum(Prob.RV.V)[2])
                               ,by=.(Neta,Gamma,pR,pV,Outbr)]

FIA.stats$notProb<-1-FIA.stats$pR-FIA.stats$pV


paletteMeans <- colorRampPalette(c('#d73027','#fc8d59','#fee090',
                                   '#e0f3f8','#91bfdb','#4575b4')[6:1],alpha=TRUE)

npoints<-100

FIAinterpData<-with(FIAlastQuarData[Neta==0&Gamma==0.8],
                 {interp(x=pR,y=pV,z=Prob.RV.V,duplicate = "mean",
                                           nx=npoints,ny=npoints)})
str(FIAinterpData)

FIAinterpDataTrans<-data.table(matrix(0,nrow = npoints*npoints,ncol = 4))

names(FIAinterpDataTrans)<-c("resProb","visProb","meanProb","notProb")
for (i in 1:npoints) {
  for (j in 1:npoints) {
    FIAinterpDataTrans[(i-1)*npoints+j,resProb:=FIAinterpData$x[i]]
    FIAinterpDataTrans[(i-1)*npoints+j,visProb:=FIAinterpData$y[j]]
    FIAinterpDataTrans[(i-1)*npoints+j,meanProb:=FIAinterpData$z[i,j]]
  }
  
}

FIAinterpDataTrans[,4]<-1-FIAinterpDataTrans[,1]-FIAinterpDataTrans[,2]

FIAinterpDataTrans<-FIAinterpDataTrans[resProb+visProb<0.9]

FIAinterpData.Neg<-with(FIAlastQuarData[Neta==1&Gamma==0],
                     {interp(x=pR,y=pV,z=Prob.RV.V,duplicate = "mean",
                                                    nx=npoints,ny=npoints)})

FIAinterpDataTrans.Neg<-data.table(matrix(0,nrow = npoints*npoints,ncol = 4))

names(FIAinterpDataTrans.Neg)<-c("resProb","visProb","meanProb","notProb")
for (i in 1:npoints) {
  for (j in 1:npoints) {
    FIAinterpDataTrans.Neg[(i-1)*npoints+j,resProb:=FIAinterpData.Neg$x[i]]
    FIAinterpDataTrans.Neg[(i-1)*npoints+j,visProb:=FIAinterpData.Neg$y[j]]
    FIAinterpDataTrans.Neg[(i-1)*npoints+j,meanProb:=FIAinterpData.Neg$z[i,j]]
  }
  
}

FIAinterpDataTrans.Neg[,4]<-1-FIAinterpDataTrans.Neg[,1]-FIAinterpDataTrans.Neg[,2]

FIAinterpDataTrans.Neg<-FIAinterpDataTrans.Neg[resProb+visProb<0.9]

# Plot real data prob ----------------------------------------------------------

with(FIA.stats[Neta==0&Gamma==0.8],{
  ternaryplot(cbind(pR,pV,notProb),
            col = paletteMeans(100)[findInterval(meanProb,seq(min(meanProb),
                                                              max(meanProb),
                                                              length=100))],
            main="",cex=0.8);
  color.bar.aeqp(paletteMeans(100),min =round(min(meanProb),2),
            max = round(max(meanProb),2),nticks = 5,numplotx = 5,numploty = 3,
            idplotx = 4,idploty = 3)
})

with(FIA.stats[Neta==1&Gamma==0],{
  ternaryplot(cbind(pR,pV,notProb),
              col = paletteMeans(100)[findInterval(meanProb,seq(min(meanProb),
                                                                max(meanProb),
                                                                length=100))],
              main="",cex=0.8);
  color.bar.aeqp(paletteMeans(100),min =round(min(meanProb),2),
                 max = round(max(meanProb),2),nticks = 5,numplotx = 5,numploty = 3,
                 idplotx = 4,idploty = 3)
})

# png(paste(dirfig,"triplex_tau10_neta0_Outbr0.png",sep=""),width=1000,height=1000)

with(FIAinterpDataTrans,{
  ternaryplotAEQP(cbind(resProb,visProb,notProb),
              col = paletteMeans(100)[findInterval(meanProb,
                                                   seq(min(meanProb),
                                                       max(meanProb),
                                                       length=100))],
              main="",cex=1,dimnames = c("Resident","Visitor","Absence"),
              border = "white", labels = "outside",labels_rot = c(0,0,0),
              cex.lab = 3.5,cex.grid = 2);
  color.bar.aeqp(paletteMeans(100),min =round(min(meanProb),2),
                 max = round(max(meanProb),2),nticks = 5,
                 title = "Probability of \n V over R",cex.tit = 2,numplotx = 5,
                 numploty = 5,idplotx = 5,idploty = 4)
})
# ternaryplotAEQP(interpDataTrans)

# variation panel -----------------------------------------------------------------------

paletteVar <- colorRampPalette(c('#d8b365','#f5f5f5','#5ab4ac'),alpha=TRUE)

npoints<-100

interpDataVar<-with(stateLastQuarData[Neta==0&Gamma==0.8],
                    {interp(x=pR,y=pV,
                            z=Prob.RV.V,
                            duplicate = "user",
                            dupfun = {function(x) fivenum(x)[4]-fivenum(x)[2]},
                                           nx=npoints,ny=npoints)})
bound <-0.8

state.stats<-stateLastQuarData[,.(meanProb=mean(Prob.RV.V),
                                  upIQR=fivenum(Prob.RV.V)[4],
                                  lowIQR=fivenum(Prob.RV.V)[2])
                               ,by=.(Neta,Gamma,resProb,visProb,Outbr)]



str(interpDataVar)

interpDataVarTrans<-data.table(matrix(0,nrow = npoints*npoints,ncol = 4))

names(interpDataVarTrans)<-c("resProb","visProb","IQR","notProb")
for (i in 1:npoints) {
  for (j in 1:npoints) {
    interpDataVarTrans[(i-1)*npoints+j,resProb:=interpDataVar$x[i]]
    interpDataVarTrans[(i-1)*npoints+j,visProb:=interpDataVar$y[j]]
    interpDataVarTrans[(i-1)*npoints+j,IQR:=interpDataVar$z[i,j]]
  }
  
}

interpDataVarTrans[,4]<-1-interpDataVarTrans[,1]-interpDataVarTrans[,2]

interpDataVarTrans<-interpDataVarTrans[resProb+visProb<0.9]

interpDataVar.Neg<-with(stateLastQuarData[Neta==1&Gamma==0],
                        {interp(x=pR,y=pV,z=Prob.RV.V,
                                duplicate = "user",
                                dupfun = 
                                  {function(x) fivenum(x)[4]-fivenum(x)[2]},
                                                       nx=npoints,ny=npoints)})

interpDataVarTrans.Neg<-data.table(matrix(0,nrow = npoints*npoints,ncol = 4))

names(interpDataVarTrans.Neg)<-c("resProb","visProb","IQR","notProb")
for (i in 1:npoints) {
  for (j in 1:npoints) {
    interpDataVarTrans.Neg[(i-1)*npoints+j,resProb:=interpDataVar.Neg$x[i]]
    interpDataVarTrans.Neg[(i-1)*npoints+j,visProb:=interpDataVar.Neg$y[j]]
    interpDataVarTrans.Neg[(i-1)*npoints+j,IQR:=interpDataVar.Neg$z[i,j]]
  }
  
}

interpDataVarTrans.Neg[,4]<-1-interpDataVarTrans.Neg[,1]-interpDataVarTrans.Neg[,2]

interpDataVarTrans.Neg<-interpDataVarTrans.Neg[resProb+visProb<0.9]

with(state.stats[Neta==1&Gamma==0],{
  ternaryplot(cbind(pR,pV,notProb),
              col = paletteVar(100)[findInterval(upIQR-lowIQR,
                                                 seq(min(upIQR-lowIQR),
                                                     max(upIQR-lowIQR),
                                                     length=100))],
              main="",cex=0.8);
  color.bar.aeqp(paletteVar(100),min =round(min(upIQR-lowIQR),3),
            max = round(max(upIQR-lowIQR),3),
            nticks = 3,numplotx = 15,numploty = 8,idplotx =8,idploty = 8)
})


# Speed panel -----------------------------------------------------------------------

paletteVar <- colorRampPalette(c('#d8b365','#f5f5f5','#5ab4ac'),alpha=TRUE)

npoints<-100

FIAinterpDataFirstR<-with(FIAfirstReach[Gamma==0.8&Neta==0],
                    {interp(x=pR,y=pV,z=firstReach,
                            duplicate = "mean",
                            nx=npoints,ny=npoints)})

FIAfrStats<-FIAfirstReach[,.(firstReach=mean(firstReach),
                             Prob.RV.V=mean(Prob.RV.V)),
                          by=.(Neta,Gamma,pR,pV,Outbr)]

FIAfrStats$notProb<-1-FIAfrStats$pR-FIAfrStats$pV

# Interpolation speed ----------------------------------------------------------

str(FIAinterpDataFirstR)

FIAinterpDataFRTrans<-data.table(matrix(0,nrow = npoints*npoints,ncol = 4))

names(FIAinterpDataFRTrans)<-c("resProb","visProb","FirstReach","notProb")
for (i in 1:npoints) {
  for (j in 1:npoints) {
    FIAinterpDataFRTrans[(i-1)*npoints+j,resProb:=FIAinterpDataFirstR$x[i]]
    FIAinterpDataFRTrans[(i-1)*npoints+j,visProb:=FIAinterpDataFirstR$y[j]]
    FIAinterpDataFRTrans[(i-1)*npoints+j,FirstReach:=FIAinterpDataFirstR$z[i,j]]
  }
  
}

FIAinterpDataFRTrans[,4]<-1-FIAinterpDataFRTrans[,1]-FIAinterpDataFRTrans[,2]

FIAinterpDataFRTrans<-FIAinterpDataFRTrans[resProb+visProb<0.9]

FIAinterpDataFR.Neg<-with(FIAfirstReach[Neta==1&Gamma==0],
                        {interp(x=pR,y=pV,z=firstReach,
                                duplicate = "mean",
                                nx=npoints,ny=npoints)})

FIAinterpDataFRTrans.Neg<-data.table(matrix(0,nrow = npoints*npoints,ncol = 4))

names(FIAinterpDataFRTrans.Neg)<-c("resProb","visProb","FirstReach","notProb")
for (i in 1:npoints) {
  for (j in 1:npoints) {
    FIAinterpDataFRTrans.Neg[(i-1)*npoints+j,resProb:=FIAinterpDataFR.Neg$x[i]]
    FIAinterpDataFRTrans.Neg[(i-1)*npoints+j,visProb:=FIAinterpDataFR.Neg$y[j]]
    FIAinterpDataFRTrans.Neg[(i-1)*npoints+j,FirstReach:=FIAinterpDataFR.Neg$z[i,j]]
  }
  
}

FIAinterpDataFRTrans.Neg[,4]<-1-FIAinterpDataFRTrans.Neg[,1]-
  FIAinterpDataFRTrans.Neg[,2]

FIAinterpDataFRTrans.Neg<-FIAinterpDataFRTrans.Neg[resProb+visProb<0.9]

with(FIAfrStats[Gamma==0.8&Neta==0],{
  ternaryplot(cbind(pR,pV,notProb),
              col = paletteVar(100)[findInterval(firstReach,
                                                 seq(min(firstReach),
                                                     max(firstReach),
                                                     length=100))],
              main="",cex=0.8);
  color.bar.aeqp(paletteVar(100),min =round(min(firstReach/501),3),
                 max = round(max(firstReach/501),3),
                 nticks = 3,numplotx = 15,numploty = 8,idplotx =8,idploty = 8)
})

with(FIAfrStats[Gamma==0&Neta==1],{
  ternaryplot(cbind(pR,pV,notProb),
              col = paletteVar(100)[findInterval(firstReach,
                                                 seq(min(firstReach),
                                                     max(firstReach),
                                                     length=100))],
              main="",cex=0.8);
  color.bar.aeqp(paletteVar(100),min =round(min(firstReach/501),3),
                 max = round(max(firstReach/501),3),
                 nticks = 3,numplotx = 15,numploty = 8,idplotx =8,idploty = 8)
})

# Plot real data speed ---------------------------------------------------------

with(na.omit(FIAinterpDataFRTrans),{
  ternaryplot(cbind(resProb,visProb,notProb),
              col = paletteVar(100)[findInterval(FirstReach,
                                                 seq(min(FirstReach),
                                                     max(FirstReach),
                                                     length=100))],
              main="",cex=0.8);
  color.bar.aeqp(paletteVar(100),min =round(min(FirstReach/501),3),
                 max = round(max(FirstReach/501),3),
                 nticks = 3,numplotx = 15,numploty = 8,idplotx =8,idploty = 8)
})

with(na.omit(FIAinterpDataFRTrans.Neg),{
  ternaryplot(cbind(resProb,visProb,notProb),
              col = paletteVar(100)[findInterval(FirstReach,
                                                 seq(min(FirstReach),
                                                     max(FirstReach),
                                                     length=100))],
              main="",cex=0.8);
  color.bar.aeqp(paletteVar(100),min =round(min(FirstReach/501),3),
                 max = round(max(FirstReach/501),3),
                 nticks = 3,numplotx = 15,numploty = 8,idplotx =8,idploty = 8)
})

# Figure ------------------------------------------------------------------------


png(paste("d:/quinonesa/Dropbox/Neuchatel/Figs/Actor_critic/",
          "triplex.png",sep=""),
    width=1000,height=1000)

cex.lab.par<-1.2

colorbreaksMeans<-seq(min(c(FIAinterpDataTrans$meanProb,
                                FIAinterpDataTrans.Neg$meanProb)),
                 max(c(FIAinterpDataTrans$meanProb,
                           FIAinterpDataTrans.Neg$meanProb)),length=100)

colorbreaksSpeed<-seq(min(c(FIAinterpDataFRTrans$FirstReach,
                                FIAinterpDataFRTrans.Neg$FirstReach),
                          na.rm = TRUE),
                      max(c(FIAinterpDataFRTrans$FirstReach,
                                FIAinterpDataFRTrans.Neg$FirstReach),
                          na.rm=TRUE),length=100)

plot.new()
with(FIAinterpDataTrans,{
  ternaryplotAEQP(cbind(resProb,visProb,notProb),
                  col = paletteMeans(100)[findInterval(meanProb,colorbreaksMeans)],
                  main="",cex=1,dimnames = c("Resident","Visitor","Absence"),border = "white",
                  labels = "outside",labels_rot = c(0,0,0),cex.lab = cex.lab.par,cex.grid = 1,
                  numplotx = 2,numploty = 2,idplotx = 1,idploty = 1)
})

# text(x = par('usr')[4]-0.9*(par('usr')[4]-par('usr')[3]),
#      y = par('usr')[2]-0.9*(par('usr')[2]-par('usr')[1]),labels = 'A')

with(na.omit(FIAinterpDataFRTrans),{
  ternaryplotAEQP(cbind(resProb,visProb,notProb),
                  col = paletteVar(100)[findInterval(FirstReach,
                                                     colorbreaksSpeed)],
                  main="",cex=1,dimnames = c("Resident","Visitor","Absence"),border = "white",
                  labels = "outside",labels_rot = c(0,0,0),cex.lab = cex.lab.par,cex.grid = 1,
                  numplotx = 2,numploty = 2,idplotx = 1,idploty = 2,new=FALSE);
  # color.bar.aeqp(paletteVar(100),min =round(min(FirstReach),2),
  #           max = round(max(FirstReach),2),nticks = 3,
  #           title = "Probability of \n V over R",cex.tit = 2,numplotx = 10,
  #           numploty = 10,idplotx = 8,idploty = 8)
})


# text(x = par('usr')[4]-0.9*(par('usr')[4]-par('usr')[3]),
#      y = par('usr')[2]-0.9*(par('usr')[2]-par('usr')[1]),labels = 'B')

with(FIAinterpDataTrans.Neg,{
  ternaryplotAEQP(cbind(resProb,visProb,notProb),
                  col = paletteMeans(100)[findInterval(meanProb,colorbreaksMeans)],
                  main="",cex=1,dimnames = c("Resident","Visitor","Absence"),border = "white",
                  labels = "outside",labels_rot = c(0,0,0),cex.lab = cex.lab.par,cex.grid = 1,
                  numplotx = 2,numploty = 2,idplotx = 2,idploty = 1,newpage = FALSE);
  # color.bar(rgb.palette(100),min =round(min(meanProb),2),max = round(max(meanProb),2),nticks = 5,
  #           title = "Probability of \n V over R",cex.tit = 2)
})
# text(x = par('usr')[4]-0.9*(par('usr')[4]-par('usr')[3]),
#      y = par('usr')[2]-0.9*(par('usr')[2]-par('usr')[1]),labels = 'A')

with(FIAinterpDataFRTrans.Neg,{
  ternaryplotAEQP(cbind(resProb,visProb,notProb),
                  col = paletteVar(100)[findInterval(FirstReach,colorbreaksSpeed)],
                  main="",cex=1,dimnames = c("Resident","Visitor","Absence"),
                  border = "white",labels = "outside",labels_rot = c(0,0,0),
                  cex.lab = cex.lab.par,cex.grid = 1,
                  numplotx = 2,numploty = 2,idplotx = 2,idploty = 2,
                  newpage = FALSE);
  # color.bar(rgb.palette(100),min =round(min(IQR),2),max = round(max(IQR),2),nticks = 3,
  #           title = "Probability of \n V over R",cex.tit = 2)
})
# text(x = par('usr')[4]-0.9*(par('usr')[4]-par('usr')[3]),
#      y = par('usr')[2]-0.9*(par('usr')[2]-par('usr')[1]),labels = 'B')

with(rbind(FIAinterpDataTrans,FIAinterpDataTrans.Neg),{
  par(new=TRUE)
  color.bar.aeqp(paletteMeans(100),min =round(min(meanProb),2),max = round(max(meanProb),2),nticks = 3,
            title = "Probability \n of V over R",cex.tit = 0.8,
            numplotx = 15,numploty = 8,idplotx =7,idploty = 7)})


with(rbind(FIAinterpDataFRTrans,FIAinterpDataFRTrans.Neg),{
  color.bar.aeqp(paletteVar(100),min =round(min(FirstReach/501,na.rm = TRUE),2),
                 max = round(max(FirstReach/501,na.rm = TRUE),2),nticks = 3,
            title = "Time to 0.75",cex.tit = 0.8,
            numplotx = 15,numploty = 8,idplotx =7,idploty = 2)})


dev.off()

# plot 5 PIA--------------------------------------------------------------------------------------

setwd("D:\\quinonesa\\Simulation") ## Office
## S:\\quinonesa\\Simulations\\Basic_sarsa"
rootList<-list.files()
rootdir<-rootList[2]
setwd(rootdir)
PIALastQuarData<-do.call(rbind,lapply(getFilelist(simsDir,listPar,listVal)$PIA
                                      ,file2lastProp,0.75))

PIA.stats<-PIALastQuarData[,.(meanProb=mean(Prob.RV.V),
                                  upIQR=fivenum(Prob.RV.V)[4],
                                  lowIQR=fivenum(Prob.RV.V)[2])
                               ,by=.(Neta,Gamma,pR,pV,Outbr)]

PIA.stats$notProb<-1-PIA.stats$pR-PIA.stats$pV


paletteMeans <- colorRampPalette(c('#d73027','#fc8d59','#fee090',
                                   '#e0f3f8','#91bfdb','#4575b4')[6:1],alpha=TRUE)

npoints<-100

PIAinterpData<-with(PIALastQuarData[Gamma==0.8&Neta==0],
                    {interp(x=pR,y=pV, z=Prob.RV.V, duplicate = "mean",
                                                    nx=npoints,ny=npoints)})
str(PIAinterpData)

PIAinterpDataTrans<-data.table(matrix(0,nrow = npoints*npoints,ncol = 4))

names(PIAinterpDataTrans)<-c("resProb","visProb","meanProb","notProb")
for (i in 1:npoints) {
  for (j in 1:npoints) {
    PIAinterpDataTrans[(i-1)*npoints+j,resProb:=PIAinterpData$x[i]]
    PIAinterpDataTrans[(i-1)*npoints+j,visProb:=PIAinterpData$y[j]]
    PIAinterpDataTrans[(i-1)*npoints+j,meanProb:=PIAinterpData$z[i,j]]
  }
  
}

PIAinterpDataTrans[,4]<-1-PIAinterpDataTrans[,1]-PIAinterpDataTrans[,2]

PIAinterpDataTrans<-PIAinterpDataTrans[resProb+visProb<0.9]

PIAinterpData.Neg<-with(PIALastQuarData[Neta==1&Gamma==0],
                        {interp(x=pR,y=pV,z=Prob.RV.V,
                                duplicate = "mean",nx=npoints,ny=npoints)})

PIAinterpDataTrans.Neg<-data.table(matrix(0,nrow = npoints*npoints,ncol = 4))

names(PIAinterpDataTrans.Neg)<-c("resProb","visProb","meanProb","notProb")
for (i in 1:npoints) {
  for (j in 1:npoints) {
    PIAinterpDataTrans.Neg[(i-1)*npoints+j,resProb:=PIAinterpData.Neg$x[i]]
    PIAinterpDataTrans.Neg[(i-1)*npoints+j,visProb:=PIAinterpData.Neg$y[j]]
    PIAinterpDataTrans.Neg[(i-1)*npoints+j,meanProb:=PIAinterpData.Neg$z[i,j]]
  }
  
}

PIAinterpDataTrans.Neg[,4]<-1-PIAinterpDataTrans.Neg[,1]-PIAinterpDataTrans.Neg[,2]

PIAinterpDataTrans.Neg<-PIAinterpDataTrans.Neg[resProb+visProb<0.9]

par()
with(PIA.stats[Neta==1&Gamma==0],{
  ternaryplot(cbind(pR,pV,notProb),
              col = paletteMeans(100)[findInterval(meanProb,seq(min(meanProb),
                                                                max(meanProb),
                                                                length=100))],
              main="",cex=0.8);
  color.bar.aeqp(paletteMeans(100),min =round(min(meanProb),2),
                 max = round(max(meanProb),2),
            nticks = 4,numplotx = 15,numploty = 5,idplotx = 12,idploty = 3)
})


# png(paste(dirfig,"triplex_tau10_neta0_Outbr0.png",sep=""),width=1000,height=1000)

with(PIAinterpDataTrans,{
  ternaryplotAEQP(cbind(resProb,visProb,notProb),
                  col = paletteMeans(100)[findInterval(meanProb,seq(min(meanProb),max(meanProb),length=100))],
                  main="",cex=1,dimnames = c("Resident","Visitor","Absence"),border = "white",
                  labels = "outside",labels_rot = c(0,0,0),cex.lab = 3.5,cex.grid = 2);
  color.bar.aeqp(paletteMeans(100),min =round(min(meanProb),2),max = round(max(meanProb),2),nticks = 5,
            title = "Probability of \n V over R",cex.tit = 2,
            numplotx = 15,numploty = 5,idplotx = 12,idploty = 3)
})

with(PIAinterpDataTrans.Neg,{
  ternaryplotAEQP(cbind(resProb,visProb,notProb),
                  col = paletteMeans(100)[findInterval(meanProb,seq(min(meanProb),max(meanProb),length=100))],
                  main="",cex=1,dimnames = c("Resident","Visitor","Absence"),border = "white",
                  labels = "outside",labels_rot = c(0,0,0),cex.lab = 3.5,cex.grid = 2);
  color.bar.aeqp(paletteMeans(100),min =round(min(meanProb),2),max = round(max(meanProb),2),nticks = 5,
                 title = "Probability of \n V over R",cex.tit = 2,
                 numplotx = 15,numploty = 5,idplotx = 12,idploty = 3)
})

# variation panel PIA -----------------------------------------------------------------------

paletteVar <- colorRampPalette(c('#d8b365','#f5f5f5','#5ab4ac'),alpha=TRUE)

npoints<-100

interpDataVar<-with(actLastQuarData[Neta==0],{interp(x=resProb,y=visProb,z=Prob.RV.V,duplicate = "user",
                                                       dupfun = {function(x) fivenum(x)[4]-fivenum(x)[2]},
                                                       nx=npoints,ny=npoints)})
str(interpDataVar)

interpDataVarTrans<-data.table(matrix(0,nrow = npoints*npoints,ncol = 4))

names(interpDataVarTrans)<-c("resProb","visProb","IQR","notProb")
for (i in 1:npoints) {
  for (j in 1:npoints) {
    interpDataVarTrans[(i-1)*npoints+j,resProb:=interpDataVar$x[i]]
    interpDataVarTrans[(i-1)*npoints+j,visProb:=interpDataVar$y[j]]
    interpDataVarTrans[(i-1)*npoints+j,IQR:=interpDataVar$z[i,j]]
  }
  
}

interpDataVarTrans[,4]<-1-interpDataVarTrans[,1]-interpDataVarTrans[,2]

interpDataVarTrans<-interpDataVarTrans[resProb+visProb<0.9]

interpDataVar.Neg<-with(actLastQuarData[Neta==0.5],{interp(x=resProb,y=visProb,z=Prob.RV.V,duplicate = "user",
                                                             dupfun = {function(x) fivenum(x)[4]-fivenum(x)[2]},
                                                             nx=npoints,ny=npoints)})

interpDataVarTrans.Neg<-data.table(matrix(0,nrow = npoints*npoints,ncol = 4))

names(interpDataVarTrans.Neg)<-c("resProb","visProb","IQR","notProb")
for (i in 1:npoints) {
  for (j in 1:npoints) {
    interpDataVarTrans.Neg[(i-1)*npoints+j,resProb:=interpDataVar.Neg$x[i]]
    interpDataVarTrans.Neg[(i-1)*npoints+j,visProb:=interpDataVar.Neg$y[j]]
    interpDataVarTrans.Neg[(i-1)*npoints+j,IQR:=interpDataVar.Neg$z[i,j]]
  }
  
}

interpDataVarTrans.Neg[,4]<-1-interpDataVarTrans.Neg[,1]-interpDataVarTrans.Neg[,2]

interpDataVarTrans.Neg<-interpDataVarTrans.Neg[resProb+visProb<0.9]

with(act.stats,{
  ternaryplot(cbind(resProb,visProb,notProb),
              col = paletteVar(100)[findInterval(upIQR-lowIQR,seq(min(upIQR-lowIQR),max(upIQR-lowIQR),length=100))],
              main="",cex=0.8);
  color.bar(paletteVar(100),min =round(min(upIQR-lowIQR),3),max = round(max(upIQR-lowIQR),3),
            nticks = 3,numplotx = 15,numploty = 8,idplotx =7,idploty = 7)
})


# png(paste(dirfig,"triplex_tau10_neta0_Outbr0.png",sep=""),width=1000,height=1000)

cex.lab.par<-1.2

png(paste(dirfig,"SuplFig1_4triplex_PIA.png",sep=''),width = 1000,height = 1000)

colorbreaksMeans<-seq(min(cbind(interpDataTrans$meanProb,interpDataTrans.Neg$meanProb)),
                      max(cbind(interpDataTrans$meanProb,interpDataTrans.Neg$meanProb)),length=100)

colorbreaksIQR<-seq(min(cbind(interpDataVarTrans$IQR,interpDataVarTrans.Neg$IQR)),
                    max(cbind(interpDataVarTrans$IQR,interpDataVarTrans.Neg$IQR)),length=100)

plot.new()
with(interpDataTrans,{
  ternaryplotAEQP(cbind(resProb,visProb,notProb),
                  col = paletteMeans(100)[findInterval(meanProb,colorbreaksMeans)],
                  main="",cex=1,dimnames = c("Resident","Visitor","Absence"),border = "white",
                  labels = "outside",labels_rot = c(0,0,0),cex.lab = cex.lab.par,cex.grid = 1,
                  numplotx = 2,numploty = 2,idplotx = 1,idploty = 1)
})

# text(x = par('usr')[4]-0.9*(par('usr')[4]-par('usr')[3]),
#      y = par('usr')[2]-0.9*(par('usr')[2]-par('usr')[1]),labels = 'A')



with(interpDataVarTrans,{
  ternaryplotAEQP(cbind(resProb,visProb,notProb),
                  col = paletteVar(100)[findInterval(IQR,colorbreaksIQR)],
                  main="",cex=1,dimnames = c("Resident","Visitor","Absence"),border = "white",
                  labels = "outside",labels_rot = c(0,0,0),cex.lab = cex.lab.par,cex.grid = 1,
                  numplotx = 2,numploty = 2,idplotx = 1,idploty = 2,newpage = FALSE);
  # color.bar(rgb.palette(100),min =round(min(IQR),2),max = round(max(IQR),2),nticks = 3,
  #           title = "Probability of \n V over R",cex.tit = 2)
})


# text(x = par('usr')[4]-0.9*(par('usr')[4]-par('usr')[3]),
#      y = par('usr')[2]-0.9*(par('usr')[2]-par('usr')[1]),labels = 'B')

with(interpDataTrans.Neg,{
  ternaryplotAEQP(cbind(resProb,visProb,notProb),
                  col = paletteMeans(100)[findInterval(meanProb,colorbreaksMeans)],
                  main="",cex=1,dimnames = c("Resident","Visitor","Absence"),border = "white",
                  labels = "outside",labels_rot = c(0,0,0),cex.lab = cex.lab.par,cex.grid = 1,
                  numplotx = 2,numploty = 2,idplotx = 2,idploty = 1,newpage = FALSE);
  # color.bar(rgb.palette(100),min =round(min(meanProb),2),max = round(max(meanProb),2),nticks = 5,
  #           title = "Probability of \n V over R",cex.tit = 2)
})
# text(x = par('usr')[4]-0.9*(par('usr')[4]-par('usr')[3]),
#      y = par('usr')[2]-0.9*(par('usr')[2]-par('usr')[1]),labels = 'A')

with(interpDataVarTrans.Neg,{
  ternaryplotAEQP(cbind(resProb,visProb,notProb),
                  col = paletteVar(100)[findInterval(IQR,colorbreaksIQR)],
                  main="",cex=1,dimnames = c("Resident","Visitor","Absence"),border = "white",
                  labels = "outside",labels_rot = c(0,0,0),cex.lab = cex.lab.par,cex.grid = 1,
                  numplotx = 2,numploty = 2,idplotx = 2,idploty = 2,newpage = FALSE);
  # color.bar(rgb.palette(100),min =round(min(IQR),2),max = round(max(IQR),2),nticks = 3,
  #           title = "Probability of \n V over R",cex.tit = 2)
})
# text(x = par('usr')[4]-0.9*(par('usr')[4]-par('usr')[3]),
#      y = par('usr')[2]-0.9*(par('usr')[2]-par('usr')[1]),labels = 'B')

with(rbind(interpDataTrans,interpDataTrans.Neg),{
  par(new=TRUE)
  color.bar(paletteMeans(100),min =round(min(meanProb),2),max = round(max(meanProb),2),nticks = 3,
            title = "Probability \n of V over R",cex.tit = 0.8,
            numplotx = 15,numploty = 8,idplotx =7,idploty = 7)})


with(rbind(interpDataVarTrans,interpDataVarTrans.Neg),{
  color.bar(paletteVar(100),min =round(min(IQR),2),max = round(max(IQR),2),nticks = 3,
            title = "IQR size",cex.tit = 0.8,
            numplotx = 15,numploty = 8,idplotx =7,idploty = 2)})


dev.off()
