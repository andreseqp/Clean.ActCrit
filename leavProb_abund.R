######### Abundance x Leaving prob analysis ###################################

# Load libraries and external functions -----------------------------------

projDir<-"d:/quinonesa/learning_models_c++/actCrit/"
simsDir<-"s:/quinonesa/Simulations/actCrit/InitVal1_/"

source('d:/quinonesa/Dropbox/R_files/posPlots.R')
source(paste(projDir,"aesth_par.R",sep=""))
source(paste(projDir,"loadData.R",sep = ""))
source('D:/quinonesa/Dropbox/R_files/ternaryAEQP.R')
library('akima')
library('lattice')

# Load Data for interpolations --------------------------------------------------------------------------------------

setwd(simsDir)


(listPar<-c(rep("AbundanceLpr",6)))

(listVal<-c(0,0.2,0.4,0.6,0.8,1))


FIAlast<-rbindlist(lapply(getFilelist(simsDir,listPar,listVal)$FIA, 
                          function(x){
                            if(as.numeric(gsub("[[:alpha:]]",
                                               strsplit(x,"_")[[1]][7],
                                               replacement=""))==
                               as.numeric(gsub("[[:alpha:]]",
                                               strsplit(x,"_")[[1]][8],
                                               replacement=""))){
                              file2lastProp(x,0.9,outPar = listPar[1])
                            }}))




FIAlast[,pA:=1-pR-pV]

FIA.statsdot<-FIAlast[,.(meanProb=mean(Prob.RV.V),
                         upIQR=fivenum(Prob.RV.V)[4],
                         lowIQR=fivenum(Prob.RV.V)[2])
                      ,by=.(Neta,Gamma,pR,pV,AbundanceLpr,pA)]

# Future -----------------------------------------------------------------------


png("d:/quinonesa/Dropbox/Neuchatel/Results/actCrit/Future_abun_leav.png",
    width = 1000, height = 1000)
# interpolate real data
interpData<-with(FIAlast[Neta==0&Gamma==0.8],
                 {interp(x=pA,y=AbundanceLpr,z=Prob.RV.V,duplicate = "mean",
                         nx=100,ny=100)})

interpDataTrans<-data.table(matrix(0,nrow = 100*100,ncol = 3))
names(interpDataTrans)<-c("Abundance","LeavP","probRVV")
# Transpose interpolated data
for (i in 1:100) {
  for (j in 1:100) {
    interpDataTrans[(i-1)*100+j,Abundance:=interpData$x[i]]
    interpDataTrans[(i-1)*100+j,LeavP:=interpData$y[j]]
    interpDataTrans[(i-1)*100+j,probRVV:=interpData$z[i,j]]
  }
}



# levelplot(probRVV~Abundance*LeavP,data=interpDataTrans,
#           col.regions=paletteMeans(100),
#           xlab="1- Client Abundance ",
#           ylab="Visitor leaving probability")
# 
plot.new()
par(plt=posPlot())
filled.contour(x=interpData$x,y=interpData$y,z=interpData$z,
               col=paletteMeans(50),nlevels = 50,
          xlab=list('1- Client Abundance', cex=1.5),
          ylab=list('Visitor leaving probability', cex=1.5),
          plot.axes=c(axis(1, cex.axis=1.5),axis(2, cex.axis=1.5)))


dev.off()

# Punishment -----------------------------------------------------------------------


png("d:/quinonesa/Dropbox/Neuchatel/Results/actCrit/Punish_abun_leav.png",
    width = 1000, height = 1000)
# interpolate real data
interpData<-with(FIAlast[Neta==1&Gamma==0],
                 {interp(x=pA,y=AbundanceLpr,z=Prob.RV.V,duplicate = "mean",
                         nx=100,ny=100)})

interpDataTrans<-data.table(matrix(0,nrow = 100*100,ncol = 3))
names(interpDataTrans)<-c("Abundance","LeavP","probRVV")
# Transpose interpolated data
for (i in 1:100) {
  for (j in 1:100) {
    interpDataTrans[(i-1)*100+j,Abundance:=interpData$x[i]]
    interpDataTrans[(i-1)*100+j,LeavP:=interpData$y[j]]
    interpDataTrans[(i-1)*100+j,probRVV:=interpData$z[i,j]]
  }
}


plot.new()
par(plt=posPlot())
filled.contour(x=interpData$x,y=interpData$y,z=interpData$z,
               col=paletteMeans(50),nlevels = 50,
               xlab=list('1- Client Abundance', cex=1.5),
               ylab=list('Visitor leaving probability', cex=1.5),
               plot.axes=c(axis(1, cex.axis=1.5),axis(2, cex.axis=1.5)))


dev.off()


# Punishment and future -----------------------------------------------------------------------


png("d:/quinonesa/Dropbox/Neuchatel/Results/actCrit/FuturePunish_abun_leav.png",
    width = 1000, height = 1000)
# interpolate real data
interpData<-with(FIAlast[Neta==1&Gamma==0.8],
                 {interp(x=pA,y=AbundanceLpr,z=Prob.RV.V,duplicate = "mean",
                         nx=100,ny=100)})

interpDataTrans<-data.table(matrix(0,nrow = 100*100,ncol = 3))
names(interpDataTrans)<-c("Abundance","LeavP","probRVV")
# Transpose interpolated data
for (i in 1:100) {
  for (j in 1:100) {
    interpDataTrans[(i-1)*100+j,Abundance:=interpData$x[i]]
    interpDataTrans[(i-1)*100+j,LeavP:=interpData$y[j]]
    interpDataTrans[(i-1)*100+j,probRVV:=interpData$z[i,j]]
  }
}


plot.new()
par(plt=posPlot())
filled.contour(x=interpData$x,y=interpData$y,z=interpData$z,
               col=paletteMeans(51),nlevels = 50,
               xlab=list('1- Client Abundance', cex=1.5),
               ylab=list('Visitor leaving probability', cex=1.5),
               plot.axes=c(axis(1, cex.axis=1.5),axis(2, cex.axis=1.5)))


dev.off()

# Nothing -----------------------------------------------------------------------


png("d:/quinonesa/Dropbox/Neuchatel/Results/actCrit/Nothing_abun_leav.png",
    width = 1000, height = 1000)
# interpolate real data
interpData<-with(FIAlast[Neta==0&Gamma==0],
                 {interp(x=pA,y=AbundanceLpr,z=Prob.RV.V,duplicate = "mean",
                         nx=100,ny=100)})

interpDataTrans<-data.table(matrix(0,nrow = 100*100,ncol = 3))
names(interpDataTrans)<-c("Abundance","LeavP","probRVV")
# Transpose interpolated data
for (i in 1:100) {
  for (j in 1:100) {
    interpDataTrans[(i-1)*100+j,Abundance:=interpData$x[i]]
    interpDataTrans[(i-1)*100+j,LeavP:=interpData$y[j]]
    interpDataTrans[(i-1)*100+j,probRVV:=interpData$z[i,j]]
  }
}


plot.new()
par(plt=posPlot())
filled.contour(x=interpData$x,y=interpData$y,z=interpData$z,
               col=paletteMeans(50),nlevels = 50,
               xlab=list('1- Client Abundance', cex=1.5),
               ylab=list('Visitor leaving probability', cex=1.5),
               plot.axes=c(axis(1, cex.axis=1.5),axis(2, cex.axis=1.5)))


dev.off()

