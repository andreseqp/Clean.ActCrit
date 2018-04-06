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

(listPar<-rep("test",1))
(listVal<-"")

param<-getParam(simsDir,listparam = listPar,values = listVal)


FIAraw<-loadRawData(simsDir,"FIA",listparam = listPar,values = listVal)
param<-getParam(genDir,listparam = listPar,values = listVal)


plot(logist(Theta)~Age,data=FIAraw[(Training==0&Gamma==0.0)&Neta==0])
lines(x=c(0,5000),y=c(0.5,0.5),col="grey")

FIAagg<-FIAraw[, as.list(unlist(lapply(.SD, function(x) list(mean = mean(x),
                                                             sd = sd(x))))),
               by=.(Age,Alpha,Gamma,Tau,Neta,Outbr), 
               .SDcols=grep("[[:digit:]]",names(FIAraw))]





FIAtimeInt<-do.call(
  rbind,lapply(
    getFilelist(genDir,listPar,listVal)$FIA,
    file2timeInter,interV=1001))

PIAtimeInt<-do.call(
  rbind,lapply(
    getFilelist(genDir,listPar,listVal)$PIA,
    file2timeInter,interV=1001))

DPdataProb<-do.call(rbind,
                    lapply(getFilelist(genDir,listPar,listVal)$DP,
                           file2lastDP))


featNames<-list(first.client=list(
  choise_0=list(
    mean=grep("_1_0.mean",names(FIAagg),value = TRUE),
    sd=grep("_1_0.sd",names(FIAagg),value = TRUE)),
  choise_1=list(
    mean=grep("_1_1.mean",names(FIAagg),value = TRUE),
    sd=grep("_1_1.sd",names(FIAagg),value = TRUE))),
  second.client=list(
    choise_0=list(
      mean=grep("_2_0.mean",names(FIAagg),value = TRUE),
      sd=grep("_2_0.sd",names(FIAagg),value = TRUE)),
    choise_1=list(
      mean=grep("_2_1.mean",names(FIAagg),value = TRUE),
      sd=grep("_2_1.sd",names(FIAagg),value = TRUE))))


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


# Try to understand the failures --------------------------------------------------------

tmp<-FIAraw[option=='RV'& Age>0.5*max(Age)]

tmp[,':='(length.diff=Length_choice-Length_discard,
          height.diff=Height_choice-Height_discard,
          redMain.diff=redMain_choice-redMain_discard)]

names(tmp)

npx<-3

par(plt=posPlot(numplotx = npx,idplotx = 1)-c(0.05,0.05,0,0))
plot(Type_choice~length.diff,data=tmp)
lines(x = rep(0,2),y=c(0,1),col='grey')

par(plt=posPlot(numplotx = npx,idplotx = 2)-c(0.05,0.05,0,0),new=TRUE)
plot(Type_choice~height.diff,data=tmp,yaxt='n',ylab='')
lines(x = rep(0,2),y=c(0,1),col='grey')

par(plt=posPlot(numplotx = npx,idplotx = 3)-c(0.05,0.05,0,0),new=TRUE)
plot(Type_choice~redMain.diff,data=tmp,yaxt='n',ylab='')
lines(x = rep(0,2),y=c(0,1),col='grey')

Heig.mod<-glm(Type_choice~length.diff+height.diff+redMain.diff,
              family = binomial,data = tmp)
summary(Heig.mod)

Heig.mod.int<-glm(Type_choice~length.diff*height.diff*redMain.diff,
              family = binomial,data = tmp)
summary(Heig.mod.int)

Heig.mod.length<-glm(Type_choice~Length_1_0+Length_1_1+Length_2_0+Length_2_1,
              family = binomial,data = tmp)
summary(Heig.mod.length)

Heig.mod.height<-glm(Type_choice~Height_1_0+Height_1_1+Height_2_0+Height_2_1,
                     family = binomial,data = tmp)
summary(Heig.mod.height)






