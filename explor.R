# ------------------ Exploration ------------------------ #

# Directories --------------------------------------------------------------

projDir<-"d:/quinonesa/learning_models_c++/actCrit/"
simsDir<-"s:/quinonesa/Simulations/actCrit/"

# libraries ----------------------------------------------------------------
source('d:/quinonesa/Dropbox/R_files/posPlots.R')
source(paste(projDir,"aesth_par.R",sep=""))
source(paste(projDir,"loadData.R",sep = ""))
library('plotrix')
library('lme4')


# Load data ------------------------------------------------------------


# Define data to be loaded 

(listPar<-rep("alphaTh",1))
(listVal<-"")


FIAraw<-loadRawData(simsDir,"FIA",listparam = listPar,values = listVal)
param<-getParam(simsDir,listparam = listPar,values = listVal)

FIAraw


plot(logist(Theta)~Age,data=FIAraw[(Training==0&Gamma==0.8)&Neta==0])
lines(x=c(0,5000),y=c(0.5,0.5),col="grey")

FIAagg<-FIAraw[, as.list(unlist(lapply(.SD, function(x) 
  list(mean = mean(x),IQ.h = fivenum(x)[4],IQ.l=fivenum(x)[2])))),
               by=.(Age,Alpha,Gamma,Tau,Neta,Outbr,AlphaTh), 
               .SDcols=c('Theta','RV.V','RV.R')]





FIAtimeInt<-do.call(
  rbind,lapply(
    getFilelist(simsDir,listPar,listVal)$FIA,
    file2timeInter,interV=501))

PIAtimeInt<-do.call(
  rbind,lapply(
    getFilelist(genDir,listPar,listVal)$PIA,
    file2timeInter,interV=501))

DPdataProb<-do.call(rbind,
                    lapply(getFilelist(genDir,listPar,listVal)$DP,
                           file2lastDP))



# Plot of the dynamics of the feature weights --------------------------------------------------

countR<-1
countC<-0
i<-0
par(xaxt='s',yaxt='s')
plot.new()
ylimtemp<-c(min(FIAagg[(Tau==10 & Gamma==0.8)&(Neta==0 & Outbr==0),
                       .SD,.SDcols=grep("_",names(FIAagg),value = TRUE)]),
            max(FIAagg[(Tau==10 & Gamma==0.8)&(Neta==0 & Outbr==0),
                       .SD,.SDcols=grep("_",names(FIAagg),value = TRUE)]))
with(FIAagg[(Tau==10 & Gamma==0.8)&(Neta==0 & Outbr==0)],{
  for(feat in grep("_0.mean",names(FIAagg),value = TRUE)){
    i<-i+1
    countC<-countC+1
    par(plt=posPlot(numplotx = 5,numploty = 5,idplotx = countC,idploty = countR),
        new=TRUE,las=1,cex.main=0.5)
    plot(c(min(Age),max(Age)),rep(0,2),type = "l",
         xlab = '',ylab='',ylim=ylimtemp,col="grey")
    polygon(c(Age,Age[length(Age):1]),
              c(get(feat)+get(grep("_0.sd",names(FIAagg),
                                   value = TRUE)[i]),
                get(feat)[length(Age):1]-
                  get(grep("_0.sd",names(FIAagg),
                           value = TRUE)[i])[length(Age):1]),
            col = colours[1],border = FALSE)
    polygon(c(Age,Age[length(Age):1]),
            c(get(grep("_1.mean",names(FIAagg),value = TRUE)[i])+
                    get(grep("_1.sd",names(FIAagg),
                             value = TRUE)[i]),
              get(grep("_1.mean",names(FIAagg),
                       value = TRUE)[i])[length(Age):1]-
                get(grep("_1.sd",names(FIAagg),
                         value = TRUE)[i])[length(Age):1]),
            col = colours[2],border = FALSE);
    lines(Age,get(feat),type = "l")
    lines(Age,get(grep("_1.mean",names(FIAagg),
                       value = TRUE)[i]),type = "l")
    # title(main = feat,line = -4)
    legend('bottomleft',
           legend = c(feat,grep("_1.mean",names(FIAagg),value=TRUE)[i])
                                       ,col = colours,pch = 15,cex = 0.5)
    par(yaxt='n');
    if((i)%%5==0)
    {
      countR<-countR+1
      countC<-0
      par(yaxt='s',xaxt='n')
    }
  }
})
  
# Plot the dynamics of the clients values --------------------------------------------------------------

par(plt=posPlot(),xaxt='s',yaxt='s')
with(FIAraw[((Tau==10 & Gamma==0.8)&(Neta==0 & Outbr==0.2))&option=='RV'],{
  plot(value_choice~Age,type='p',ylim=c(min(value_choice),max(value_choice)),
       xlab='Trials',ylab='Estimated value',pch=20,cex=1,col=Type_choice+1)
  points(value_discard~Age,pch=20,cex=1,col=Type_discard+1)
  text(x=par('usr')[1]+(par('usr')[2]-par('usr')[1])*0.05,
       y=par('usr')[3]+(par('usr')[4]-par('usr')[3])*0.05,
       labels = bquote(tau==.(unique(Tau))))
  text(x=par('usr')[1]+(par('usr')[2]-par('usr')[1])*0.05,
       y=par('usr')[3]+(par('usr')[4]-par('usr')[3])*0.1,
       labels=bquote(gamma==.(unique(Gamma))))
  text(x=par('usr')[1]+(par('usr')[2]-par('usr')[1])*0.05,
       y=par('usr')[3]+(par('usr')[4]-par('usr')[3])*0.15,
       labels = bquote(eta==.(unique(Neta))))
  legend('topleft',col =c(1,2,3),legend = c('resident','visitor','absence'),pch =20,
         ncol = 3,cex = 0.8)
})


# Plot dynamics of probability to choose V over R ------------------------------

par(plt=posPlot(numplotx = 1,idplotx = 1),yaxt='s',las=1)
with(FIAagg[Neta==0&Gamma==0.8],{
  plotCI(x=Age,y=logist(Theta.mean),
         ui = logist(Theta.IQ.h),li=logist(Theta.IQ.l),
         pch=16,xlab='Time',ylab='Prob. V over R',cex.lab=2,
         col=colboxes[match(AlphaTh,unique(AlphaTh))],
         sfrac=0.0002,cex.axis=1.3,ylim=c(0,1),cex=1.2)
  lines(x=c(0,max(Age)),y=c(0.5,0.5),col='grey')
})

legend('topright',
       legend=unique(FIAagg[,AlphaTh])[order(unique(FIAagg[,AlphaTh]))],
       col=colboxes,pch=15,
       title="AlphaTH",cex=1.5,ncol=3)


extpar<-listPar[1]

FIAIntstats<-FIAtimeInt[,.(meanProb=mean(Prob.RV.V),
                           upIQR=fivenum(Prob.RV.V)[4],
                           lowIQR=fivenum(Prob.RV.V)[2])
                        ,by=.(Interv,Neta,Outbr,Tau,Gamma,get(extpar))]
setnames(FIAIntstats,'get',extpar)

par(plt=posPlot(numplotx = 1,idplotx = 1),yaxt='s',las=1)
with(FIAIntstats[Neta==0&Gamma==0.8],{
  plotCI(x=Interv,y=meanProb,
         ui = upIQR,li=lowIQR,
         pch=16,xlab='Time',ylab='Prob. V over R',cex.lab=2,
         col=colboxes[match(AlphaTh,unique(AlphaTh))],
         sfrac=0.002,cex.axis=1.3,ylim=c(0,1),cex=1.2)
  lines(x=c(0,max(Interv)),y=c(0.5,0.5),col='grey')
})

legend('topright',
       legend=unique(FIAagg[,AlphaTh])[order(unique(FIAagg[,AlphaTh]))],
       col=colboxes,pch=15,
       title="AlphaTH",cex=1.5,ncol=3)





