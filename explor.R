# ------------------ Exploration ------------------------ #

# Directories --------------------------------------------------------------

projDir<-"d:/quinonesa/learning_models_c++/actCrit/"
simsDir<-"s:/quinonesa/Simulations/actCrit/"


# libraries ----------------------------------------------------------------
source('d:/quinonesa/Dropbox/R_files/posPlots.R')
source(paste(projDir,"aesth_par.R",sep=""))
source(paste(projDir,"loadData.R",sep = ""))
library('plotrix')
# library('lme4')


# Load data ------------------------------------------------------------


# Define data to be loaded 

(listPar<-c("test","gamma","neta"))
(listVal<-c("",0.8,0))


FIAraw<-loadRawData(simsDir,"FIA",listparam = listPar,values = listVal)
param<-getParam(simsDir,listparam = listPar,values = listVal)

FIAraw<-fread(getFilelist(simsDir,listPar,listVal)$FIA)

file.info(getFilelist(simsDir,listPar,listVal)$FIA)

FIAagg<-FIAraw[, as.list(unlist(lapply(.SD, function(x) 
  list(mean = mean(x),IQ.h = fivenum(x)[4],IQ.l=fivenum(x)[2])))),
               by=.(Age,Alpha,Gamma,Tau,Neta,Outbr,AlphaTh), 
               .SDcols=c('Theta','Delta')]



FIAtimeInt<-do.call(
  rbind,lapply(
    getFilelist(simsDir,listPar,listVal)$FIA,
    file2timeInter,interV=501))

# PIAtimeInt<-do.call(
#   rbind,lapply(
#     getFilelist(simsDir,listPar,listVal)$PIA,
#     file2timeInter,interV=501))

# DPdataProb<-do.call(rbind,
#                     lapply(getFilelist(genDir,listPar,listVal)$DP,
#                            file2lastDP))



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


# Using Theta
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

# Plot Theta

par(plt=posPlot(numplotx = 1,idplotx = 1)+c(-0.05,-0.05,0,0),yaxt='s',las=1)
with(FIAagg[(Neta==0.0&Gamma==0.8)],{
  plot(x=Age,y=logist(Theta.mean),
         pch=16,xlab='Time',ylab='Theta',cex.lab=2,
         col=colboxes[match(AlphaTh,unique(AlphaTh))],
         cex.axis=1.3,cex=1,type="l")
  lines(x=c(0,max(Age)),y=c(0,0),col='grey')
  par(new=TRUE)
  plot(x=Age,y=Delta.mean,type="l",col="green",yaxt='n')
  axis(4)
})

FIAraw[(Neta==0&Gamma==0.8)&Training==0,.(Age,Theta)][Theta==max(Theta)]

legend('topright',
       legend=unique(FIAagg[,AlphaTh])[order(unique(FIAagg[,AlphaTh]))],
       col=colboxes,pch=15,
       title="AlphaTH",cex=1.5,ncol=3)

# Plot lines theta

FIAlines<-dcast(FIAagg[(Neta==0&Gamma==0.8)],Age~AlphaTh,
                value.var = "Theta.mean")
names(FIAlines)[2:4]<-paste0("AlphaTh",names(FIAlines)[2:4],sep="")

par(plt=posPlot(numplotx = 1,idplotx = 1),yaxt='s',las=1)
with(FIAlines,{
  matplot(y=cbind(AlphaTh0.005,AlphaTh0.01,AlphaTh0.05),
           pch=16,xlab='Time',ylab='Value',cex.lab=2,
           col=colboxes,
           cex.axis=1.3,cex=1,yaxt='n',type='l')
  axis(2)
  lines(x=c(0,max(Age)),y=c(0,0),col='grey')
})

# One training round

par(plt=posPlot(numplotx = 1,idplotx = 1)+c(-0.05,-0.05,0,0),yaxt='s',las=1)
with(FIAraw[((Neta==0.0&Gamma==0.8)&Training==0)&option=='RV'],{
  plot(x=Age[Choice==1],y=Delta[Choice==1],#*(1-logist(Theta[Choice==1]))
       pch=16,xlab='Time',cex.lab=2,
       col="red",cex=0.1,
       cex.axis=1.3,type="p")
  points(x=Age[Choice==0],y=Delta[Choice==0],#*logist(Theta[Choice==0]),
         col='green',cex=0.1)
  # par(new=TRUE)
  # plot(x=Age,y=Delta.mean,type="l",col="green",yaxt='n')
  # axis(4)
})

sum(FIAraw[Training==0&option=='RV',AlphaTh*ifelse(Choice,(1-logist(Theta))*Delta,
                                       -logist(Theta)*Delta)])

FIAraw[(Training==0&Age==max(Age)),Theta]

with(FIAraw[Training==0],{
  matplot(y=cbind(RV.V,RV.R),x=cbind(Age,Age),type = 'l',lty = c(1,2))})

hist(FIAraw[option=='RV'&Choice==1,Delta],col=rgb(0,0,1,1/4),breaks = 30)
hist(FIAraw[option=='RV'&Choice==0,Delta],col=rgb(1,0,1,1/4),add=TRUE,
     breaks = 30)

# Plot values

par(plt=posPlot(numplotx = 1,idplotx = 1)-c(0.05,0.05,0,0),yaxt='s',las=1)
with(FIAagg[(Neta==0&Gamma==0.8)&AlphaTh==0.01],{
  plot(y=RV.V.mean,x=Age,
       pch=16,xlab='Time',ylab='Value',cex.lab=2,
       col=colboxes[match(AlphaTh,unique(AlphaTh))],
       cex.axis=1.3,ylim=c(0,5),cex=1,yaxt='n',type='l')
  
  axis(2)
  lines(x=Age,y=RV.R.mean,col=colboxes[match(AlphaTh,unique(AlphaTh))],
         pch=8,lty="dashed")
  lines(x=c(0,max(Age)),y=c(0,0),col='grey')
  par(new=TRUE)
  plot(Theta.mean~Age,col="green",ylab = "",xlab="",yaxt='n')
  axis(4)
})


legend('topright',
       legend=unique(FIAagg[,AlphaTh])[order(unique(FIAagg[,AlphaTh]))],
       col=colboxes,pch=15,
       title="AlphaTH",cex=1.5,ncol=3)


extpar<-listPar[1]

FIAIntstats<-FIAtimeInt[,.(meanProb=mean(Prob.RV.V),
                           upIQR=fivenum(Prob.RV.V)[4],
                           lowIQR=fivenum(Prob.RV.V)[2])
                        ,by=.(Interv,Neta,Outbr,Tau,Gamma,AlphaTh)]
setnames(FIAIntstats,'get',extpar)

par(plt=posPlot(numplotx = 1,idplotx = 1),yaxt='s',las=1)
with(FIAIntstats[Neta==0&Gamma==0.8],{
  plotCI(x=Interv,y=meanProb,
         ui = upIQR,li=lowIQR,
         pch=16,xlab='Time',ylab='Prob. V over R',cex.lab=2,
         col=colboxes[match(AlphaTh,unique(AlphaTh))],
         sfrac=0.002,cex.axis=1.3,ylim=c(0,1),cex=1.2,
         xlim=c(0,10))
  lines(x=c(0,10),y=c(0.5,0.5),col='grey')
})

legend('bottomright',
       legend=unique(FIAagg[,AlphaTh])[order(unique(FIAagg[,AlphaTh]))],
       col=colboxes,pch=15,
       title="AlphaTH",cex=1.5,ncol=3)

with(FIAraw[])
plot()

# Olle's results ---------------------------------------------------------------

cexpar<-1.5
yaxRangy<-c('s','n','n')

ylabRang<-c("Prob. V over R","","")
xlabRang<-c("","Time","")

png(filename = "d:/quinonesa/Dropbox/Neuchatel/Olle/ActCrit_Olle_par.png",
    width = 1600,height = 800)
count<-0
for(alphTh in unique(FIAIntstats$AlphaTh)){
  
  count<-count+1
  par(plt=posPlot(numplotx = 3,idplotx = count),yaxt=yaxRangy[count],las=1,
      new=c(FALSE,TRUE)[count])
  with(FIAIntstats[(Neta==0.5&factRew==2)&AlphaTh==alphTh],{
    plotCI(x=Interv,y=meanProb,
           ui = upIQR,li=lowIQR,
           pch=16,xlab=xlabRang[count],ylab=ylabRang[count],cex.lab=2,
           col=colOlle[match(Gamma,unique(Gamma))],xlim=c(0,15),
           sfrac=0.002,cex.axis=1.3,ylim=c(0,1),cex=cexpar)
    lines(x=c(0,max(Interv)),y=c(0.5,0.5),col='grey')
  })
  par(new=TRUE)
  with(FIAIntstats[(Neta==0&factRew==1)&AlphaTh==alphTh],{
    plotCI(x=Interv,y=meanProb,
           ui = upIQR,li=lowIQR,
           pch=16,xlab='',ylab='',
           col=colOlle2[match(Gamma,unique(Gamma))],xlim=c(0,15),
           sfrac=0.002,cex.axis=1.3,ylim=c(0,1),cex=cexpar)
    lines(x=c(0,max(Interv)),y=c(0.5,0.5),col='grey')
  })

  text(x=par('usr')[1]+0.3*(par('usr')[2]-par('usr')[1]),
       y=par('usr')[3]+0.1*(par('usr')[4]-par('usr')[3]),
       labels = bquote(alpha[theta]==.(alphTh)),cex = 2)
}
legend('bottomright',
       legend=c("Punishment and future", "punishment",
                "future","no punishment no future"),
       col=c(colOlle,colOlle2),pch=15,cex=1.3,ncol=1)

dev.off()

## Debugging -------------------------------------------------------------------

setwd(simsDir)

list.files("ActCrit")

getFilelist(simsDir)

