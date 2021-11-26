##### Analysis of the Aproximate Bayesian Computation to estimating learning
##### parameters in the AC models using Zegni's data

## Libraries

require(here)
library(plot3D) 
require(data.table)
library(coda)
library(mcmcplots)
library(ggplot2)
library(cowplot)
library(ggdist)
library(bayesplot) 
library(lattice)
source(here("../R_files/posPlots.R"))
source(here("loadData.R"))
library("jsonlite")

# simdir<-"D:/Neuch_simulations/ABCfit/Simulations/"

scen1<-"MCMCclean_gam_Nrew_sca_"
scen2<-"MCMCclean_gam_sca_"
scen3<-"MCMCclean_nRew_sca_"

## Load files --------------------------------------------------------------

# all chains in a single object

MCMCdata1<-loadMCMCrun(scen1)
MCMCdata2<-loadMCMCrun(scen2)
MCMCdata3<-loadMCMCrun(scen3)

# One per onject
ABCraw<-fread(here("Simulations",scen,ABCruns[3]))

# Defaults
burn.in<-1000
thinning<-100


# All in a list
mcmcList.1<-mcmc.list(do.call(list,lapply(unique(MCMCdata1$seed), function(repli){
  rundata<-MCMCdata1[seed==repli]
  mcmcRun<-mcmc(rundata[,.(gamma,negReward,scaleConst)],thin = 100)#
  return(mcmcRun)
})))


mcmcList.2<-mcmc.list(do.call(list,lapply(unique(MCMCdata2$seed), function(repli){
  rundata<-MCMCdata2[seed==repli]
  mcmcRun<-mcmc(rundata[,.(gamma,negReward,scaleConst)],thin = 100)#
  return(mcmcRun)
})))


mcmcList.3<-mcmc.list(do.call(list,lapply(unique(MCMCdata3$seed), function(repli){
  rundata<-MCMCdata3[seed==repli]
  mcmcRun<-mcmc(rundata[,.(gamma,negReward,scaleConst)],thin = 100)#
  return(mcmcRun)
})))


## Base R plots -----------------------------------------------------------------



## get original parameter values from the simulations

# parsOrigin<-fromJSON(here("Simulations",scen,
#               grep("parameters_pred",listFiles,value = TRUE)))
# 


# filter


ABC.gamma.time.short<-dcast(MCMCdata3,iteration~seed,value.var = c("gamma"))
ABC.nRew.time.short<-dcast(MCMCdata3,iteration~seed,value.var = "negReward")
ABC.sca.time.short<-dcast(MCMCdata3,iteration~seed,value.var = "scaleConst")
ABC.logLike.time.short<-dcast(MCMCdata3,iteration~seed,value.var = "fit")

par(plt=posPlot(numploty = 4,idploty = 4,numplotx = 2,idplotx = 1)-c(0.05,0.05,0,0),
    mfrow=c(1,1),las=1)
matplot(y=ABC.gamma.time.short[,c(2,3,4)],
        x=ABC.gamma.time.short[,iteration],
        type="l",lty=1,col=2:5,ylab = "",
        xlab="",xaxt="n",lwd=0.1)

mtext(text =  expression(gamma),cex=3,side = 2,line = 3)

par(plt=posPlot(numploty = 4,idploty = 4,numplotx = 2,idplotx = 2)-c(0.05,0.05,0,0),
    mfrow=c(1,1),
    las=1,new=TRUE)
densGamma<-density(MCMCdata3$gamma)
plot(densGamma,yaxt="n",ylab="",xlab="",cex.axis=0.7)
axis(4)
modeGam<-densGamma$x[densGamma$y==max(densGamma$y)]
lines(x = rep(modeGam,2),y = range(densGamma$y))
meanGam<-mean(MCMCdata3$gamma)
lines(x = rep(meanGam,2),y = range(densGamma$y),col="red")
medianGam<-median(MCMCdata3$gamma)
lines(x = rep(medianGam,2),y = range(densGamma$y),col="green")

lines(x = rep(parsOrigin$init[3],2),y = range(densGamma$y),col="blue")

par(plt=posPlot(numploty = 4,idploty = 3,numplotx = 2,idplotx = 1)-c(0.05,0.05,0,0),
    mfrow=c(1,1),las=1,new=TRUE)
matplot(y=ABC.nRew.time.short[,c(2,3,4)],
        x=ABC.nRew.time.short[,iteration],
        type="l",lty=1,col=2:5,ylab = "",
        xlab="",xaxt="n",lwd=0.1)
mtext(text = expression(eta),line = 3,cex = 3,side = 2)

par(plt=posPlot(numploty = 4,idploty = 3,numplotx = 2,idplotx = 2)-c(0.05,0.05,0,0),
    mfrow=c(1,1), las=1,new=TRUE)
densnegReward<-density(MCMCdata3$negReward)
plot(densnegReward,yaxt="n",ylab="",xlab="",cex.axis=0.7,
     main="",xlim=c(-5,5))
axis(4)

modenegReward<-densnegReward$x[densnegReward$y==max(densnegReward$y)]
lines(x = rep(modenegReward,2),y = range(densnegReward$y))
meannegReward<-mean(MCMCdata3$negReward)
lines(x = rep(meannegReward,2),y = range(densnegReward$y),col="red")
mediannegReward<-median(MCMCdata3$negReward)
lines(x = rep(mediannegReward,2),y = range(densnegReward$y),col="green")

lines(x = rep(parsOrigin$init[4],2),y = range(densGamma$y),col="blue")

par(plt=posPlot(numploty = 4,idploty = 2,numplotx = 2,idplotx = 1)-c(0.05,0.05,0,0),
    mfrow=c(1,1),las=1,new=TRUE)
matplot(y=ABC.sca.time.short[,c(2,3,4)],
        x=ABC.sca.time.short[,iteration],
        type="l",lty=1,col=2:5,ylab = "",
        xlab="",xaxt="n",lwd=0.1)
mtext(text = "Scale const." ,line = 3,cex = 1,side = 2,las=0)
par(plt=posPlot(numploty = 4,idploty = 2,numplotx = 2,idplotx = 2)-c(0.05,0.05,0,0),
    mfrow=c(1,1),
    las=1,new=TRUE)
densScal<-density(MCMCdata3$scaleConst)
plot(densScal,yaxt="n",ylab="",xlab="",cex.axis=0.7,
     main="")
axis(4)

modeScal<-densScal$x[densScal$y==max(densScal$y)]
lines(x = rep(modeScal,2),y = range(densScal$y))
meanScal<-mean(MCMCdata3$scaleConst)
lines(x = rep(meanScal,2),y = range(densScal$y),col="red")
medianScal<-median(MCMCdata3$scaleConst)
lines(x = rep(medianScal,2),y = range(densScal$y),col="green")

lines(x = rep(parsOrigin$init[5],2),y = range(densGamma$y),col="blue")

legend("right",legend = c("mode","mean","median"),col = c("black","red","green"),
       lty=1,lwd=2,cex=0.8)


par(plt=posPlot(numploty = 4,idploty = 1,)-c(0.05,0.05,0.05,0.05),mfrow=c(1,1),las=1,new=TRUE)
matplot(y=ABC.logLike.time.short[,c(2,3,4)],
        x=ABC.logLike.time.short[,iteration],
        type="l",lty=1,col=2:5,ylab = "",
        xlab="",xaxt="n",lwd=0.1)
axis(1)
mtext(text = "loglikelihood" ,line = 3,cex = 1,side = 2,las=0)
mtext(text = "iteration" ,line = 2,cex = 1,side = 1,las=0)

modeGam;modenegReward;modeScal


plot(data=MCMCdata3,gamma~negReward,type="p",cex=0.2)
plot(data=MCMCdata3,negReward~scaleConst,type="p",cex=0.2)
plot(data=MCMCdata3,gamma~scaleConst,type="p",cex=0.2)

## Density plots with gg -------------------------------------------------------



cuts.1<-lapply(MCMCdata1[,.(gamma,negReward,scaleConst)],
             function(x){
               c(mode_hdci(x,.width = c(0.95,.66))$ymin,
                 mode_hdci(x,.width = c(.66,.95))$ymax)  
})


cuts.2<-lapply(MCMCdata2[,.(gamma,negReward,scaleConst)],
               function(x){
                 c(mode_hdi(x,.width = c(0.95,.66))$ymin,
                   mode_hdi(x,.width = c(.66,.95))$ymax)  
               })

cuts.3<-lapply(MCMCdata3[,.(gamma,negReward,scaleConst)],
               function(x){
                 c(mode_hdi(x,.width = c(0.95,.66))$ymin,
                   mode_hdi(x,.width = c(.66,.95))$ymax)  
               })


loglikehoods.both<-data.table(lglikelihood=c(MCMCdata1$fit,MCMCdata2$fit,
                                             MCMCdata3$fit),
                              model=c(rep("both",length(MCMCdata1$fit)),
                                      rep("gam",length(MCMCdata2$fit)),
                                          rep("Nrew",length(MCMCdata3$fit))))


fieldData<-fread(here("Data","data_ABC_cleaner_absolute.txt"))

nullLikehood<-sum(dbinom(x=fieldData[,score_visitor],
                         size = 20,prob = 0.5,log = TRUE))

# MCMCdata1[,Rsqrd:=1-fit/nullLikehood]

loglikehoods.both[,Rsqrd:=1-lglikelihood/nullLikehood]

loglikehoods.both[,model:=factor(model,levels=c("gam","Nrew","both"))]

ggplot(data=loglikehoods.both[is.finite(lglikelihood)],
       aes(fill=model,x=lglikelihood,y=model))+
  stat_halfeye(alpha=0.4)+theme_classic()+xlim(-750,-400) +
  # scale_fill_manual(values=c("#d73027", "#4575b4"))+
  geom_vline(xintercept = nullLikehood,color="red")


df.Rsqrd.mode<-data.table(model=c("both","gam"),
                          Rsqr=c(rsqr.both.McFadden,rsqr.gam.McFadden))

rsqr.both<-
  ggplot(data=loglikehoods.both[is.finite(lglikelihood)],
                  aes(y=model,x=Rsqrd,fill=model))+
  stat_halfeye(alpha=0.5,point_size=3)+
  theme_classic()+xlim(-0.5,0.3)+
  scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3"),
  name = "Model", labels = c("Future reward","Negative reward","Full"))+
  geom_vline(xintercept = 0,color="black",size=1)+
  theme(legend.position = c(.25, .80),legend.key.size = unit(0.5,'cm'),
        axis.text.y = element_blank())

png(here("post_both_gamma.png"),width = 700,height = 800)

png(here("Simulations",paste0(scenario,"_"),
         paste0(strsplit(predfileMode.both,"seed")[[1]][1],"poster.png")),width = 1300,height = 700)


plot_grid(nrow=3,align = "v",byrow = FALSE,
          ggplot(data=MCMCdata1,aes(x=gamma)) + 
            stat_halfeye(aes(fill=stat(cut(x,breaks = cuts.1$gamma))),
                         point_interval = mode_hdi, .width = c(.66, .95),point_size=3,
                         show.legend=FALSE) + 
            labs(title="Full model",subtitle = expression(gamma),x="",y="")+
            theme_classic()+scale_fill_manual(values=c("gray85","skyblue","gray85")),
          ggplot(data=MCMCdata1,aes(x=scaleConst)) + 
            stat_halfeye(aes(fill=stat(cut(x,breaks = cuts.1$scaleConst))),
                         point_interval = mode_hdi, .width = c(.66, .95),point_size=3,
                         show.legend=FALSE)+
            theme_classic()+scale_fill_manual(values=c("skyblue","gray85"))+
          labs(subtitle = "Scalling const.",x="",y="")+xlim(-2,1500)+theme_classic(),
          ggplot(data=MCMCdata1,aes(x=negReward)) + 
            stat_halfeye(aes(fill=stat(cut(x,breaks = cuts.1$negReward))),
                         point_interval = mode_hdi, .width = c(.66, .95),point_size=3,
                         show.legend=FALSE)+
            scale_fill_manual(values=c("gray85","skyblue","gray85"))+
            labs(subtitle = expression(eta),x="",y="")+
            xlim(-2.5,2.5)+theme_classic(),
          ggplot(data=MCMCdata2,aes(x=gamma)) +
            stat_halfeye(aes(fill=stat(cut(x,breaks = cuts.2$gamma))),
                         point_interval = mode_hdi, .width = c(.66, .95),point_size=3,
                         show.legend=FALSE) +
            scale_fill_manual(values=c("gray85","skyblue","gray85"))+
            labs(title="Only future reward",subtitle = expression(gamma),x="",y="")+theme_classic(),
          ggplot(data=MCMCdata2,aes(x=scaleConst)) +
            stat_halfeye(aes(fill=stat(cut(x,breaks = cuts.2$scaleConst))),
                         point_interval = mode_hdi, .width = c(.66, .95),point_size=3,
                         show.legend=FALSE) +
            scale_fill_manual(values=c("gray85","skyblue","gray85"))+
            labs(subtitle = "Scalling const.",x="",y="")+
            theme_classic(),
          rsqr.both+labs(subtitle = expression(pseudo-R^2),x="",y=""),
          # theme(legend.position = c(.25, .55)),
          # ggplot(data=MCMCdata1,aes(x=Rsqrd))+
          #   geom_density(alpha=0.6)+theme_classic()+xlim(-0.5,0.3)+
          #   scale_fill_manual(values=c("#d7191c", "#4575b4"))+
          #   scale_color_manual(values=c("#d73027", "#4575b4"))+
          #   geom_vline(xintercept = 0,color="black",size=1),
          labels=c('a','b','c','d','e','f')
)



dev.off()

str(areas.data)

mcmc_dens(mcmcList,pars = "gamma")
mcmc_dens(mcmcList,pars = "negReward")
mcmc_dens(mcmcList,pars = "scaleConst")

ggplot(data=ABCfiltered,aes(x=gamma))+
  geom_density()+geom_vline(xintercept = modeGam)+
  theme_classic()

# Base R for a single chain ----------------------------------------------------

par(plt=posPlot(numploty = 4,idploty = 4,numplotx = 2,idplotx = 1)-c(0.05,0.05,0,0),
    mfrow=c(1,1),las=1)
matplot(y=ABCfiltered[,gamma],
        x=ABCfiltered[,iteration],
        type="l",lty=1,col=2:5,ylab = "",
        xlab="",xaxt="n",lwd=0.1)
title(main = expression(gamma),line = -2,cex=3)

par(plt=posPlot(numploty = 4,idploty = 4,numplotx = 2,idplotx = 2)-c(0.05,0.05,0,0),
    mfrow=c(1,1),
    las=1,new=TRUE)
densGamma<-density(ABCfiltered$gamma)
plot(densGamma,yaxt="n",ylab="",xlab="",cex.axis=0.7)
axis(4)
modeGam<-densGamma$x[densGamma$y==max(densGamma$y)]
lines(x = rep(modeGam,2),y = range(densGamma$y))
meanGam<-mean(ABCfiltered$gamma)
lines(x = rep(meanGam,2),y = range(densGamma$y),col="red")
medianGam<-median(ABCfiltered$gamma)
lines(x = rep(medianGam,2),y = range(densGamma$y),col="green")

par(plt=posPlot(numploty = 4,idploty = 3,numplotx = 2,idplotx = 1)-c(0.05,0.05,0,0),new=TRUE)
matplot(y=ABCfiltered[,negReward],
        x=ABCfiltered[,iteration],
        type="l",lty=1,col=2:5,ylab = "",
        xlab="",xaxt="n",lwd=0.1)

par(plt=posPlot(numploty = 4,idploty = 3,numplotx = 2,idplotx = 2)-c(0.05,0.05,0,0),
    mfrow=c(1,1), las=1,new=TRUE)
densnegReward<-density(ABCfiltered$negReward)
plot(densnegReward,yaxt="n",ylab="",xlab="",cex.axis=0.7,xlim=c(0,5),
     main="")
axis(4)

modenegReward<-densnegReward$x[densnegReward$y==max(densnegReward$y)]
lines(x = rep(modenegReward,2),y = range(densnegReward$y))
meannegReward<-mean(ABCfiltered$negReward)
lines(x = rep(meannegReward,2),y = range(densnegReward$y),col="red")
mediannegReward<-median(ABCfiltered$negReward)
lines(x = rep(mediannegReward,2),y = range(densnegReward$y),col="green")

par(plt=posPlot(numploty = 4,idploty = 2,numplotx = 2,idplotx = 1)-c(0.05,0.05,0,0)
    ,new=TRUE)
matplot(y=ABCfiltered[,scaleConst],
        x=ABCfiltered[,iteration],
        type="l",lty=1,col=2:5,ylab = "",
        xlab="",xaxt="n",lwd=0.1)

mtext(text = "Scale const." ,line = 3,cex = 1,side = 2,las=0)
par(plt=posPlot(numploty = 4,idploty = 2,numplotx = 2,idplotx = 2)-c(0.05,0.05,0,0),
    mfrow=c(1,1),
    las=1,new=TRUE)
densScal<-density(ABCfiltered$scaleConst)
plot(densScal,yaxt="n",ylab="",xlab="",cex.axis=0.7,
     main="")
axis(4)

modeScal<-densScal$x[densScal$y==max(densScal$y)]
lines(x = rep(modeScal,2),y = range(densScal$y))
meanScal<-mean(ABCfiltered$scaleConst)
lines(x = rep(meanScal,2),y = range(densScal$y),col="red")
medianScal<-median(ABCfiltered$scaleConst)
lines(x = rep(medianScal,2),y = range(densScal$y),col="green")

legend("right",legend = c("mode","mean","median"),col = c("black","red","green"),
       lty=1,lwd=2,cex=0.8)

par(plt=posPlot(numploty = 4,idploty = 1)-c(0.05,0.05,0,0),new=TRUE)
matplot(y=ABCfiltered[,fit],
        x=ABCfiltered[,iteration],
        type="l",lty=1,col=2:5,ylab = "",
        xlab="",xaxt="n",lwd=0.1)

axis(1)

par(plt=posPlot())
plot(dnorm(seq(0,500,by=0.01),30,1)~seq(0,500,by=0.01),type="l")


     
ABCburned[match(max(gamma),gamma)]

2/-Inf

par(plt=posPlot(numploty = 1,idploty = 1))
matplot(y=ABCburned[,6],x=ABCburned[,1],
        yaxt="s",xaxt="s",type="l",
        ylab="log-likelihood",xlab="",cex = 0.05,pch = 20)
lines(x=c(0,1000000),y=rep(-83.177,2),col="red",lwd=2)


hist(ABCfiltered$gamma)

hist(ABCfiltered$negReward)

hist(ABCfiltered$fit)


plot(negReward~gamma, data = ABCraw,type="p",
     ylab=expression(eta),xlab=expression(gamma),cex = 0.1)

##  Create cuts:
gamma_c <- cut(ABCfiltered$gamma, 100)
ngRew_c <- cut(ABCfiltered$negReward, 100)

##  Calculate joint counts at cut levels:
z <- table(gamma_c, ngRew_c)/sum(table(gamma_c, ngRew_c))



##  Plot as a 3D histogram:
hist3D(z=z)

##  Plot as a 2D heatmap:

image2D(z=z, border="black",y=seq(min(ABCfiltered$negReward),
                                  max(ABCfiltered$negReward),
                                  length.out = ncol(z)),
        x=seq(min(ABCfiltered$gamma),max(ABCfiltered$gamma),
              length.out = ncol(z)),xlab=expression(gamma),ylab=expression(eta))



par(plt=posPlot(),mfrow=c(1,1))
plot(fit~gamma,data=ABCraw,pch=21,cex=0.1)
mod1<-lm(fit~gamma,data=ABCraw)
anova(mod1)
abline(mod1)
mod2<-lm(fit~gamma+I(gamma^2),data=ABCraw)
anova(mod2)
lines(y=predict.lm(mod2,data.frame(gamma=seq(0,1,length=1000))),
                   x=seq(0,1,length=1000),col="red")


plot(fit~negReward,data=ABCraw,pch=21,cex=0.1)
(mod1.1<-lm(fit~negReward,data=ABCraw))
anova(mod1.1)
abline(mod1.1)
(mod1.2<-lm(fit~negReward+I(negReward^2),data=ABCraw))
anova(mod1.2)
lines(y=predict.lm(mod1.2,data.frame(negReward=seq(0,5,length=1000))),
      x=seq(0,5,length=1000),col="red")


## MCMC analisis with coda --------------------------------------------------

# ABCraw<-fread(ABCruns[3])
ABCraw<-fread(here("Simulations",scen,ABCruns[2]))

ABC.mcmc<-mcmc(ABCraw[,.(gamma,negReward,scaleConst)])
effectiveSize(ABC.mcmc)

(sumMCMC<-summary(ABC.mcmc))
sumMCMC$statistics[,1]
plot(ABC.mcmc)
crosscorr.plot(ABC.mcmc)
crosscorr(ABC.mcmc)
density(ABC.mcmc)
geweke.plot(ABC.mcmc)
HPDinterval(ABC.mcmc)
raftery.diag(ABC.mcmc)
autocorr(ABC.mcmc)
rejectionRate(ABC.mcmc)

hist(ABC.mcmc[,1],breaks = 100)
hist(ABC.mcmc[,2],breaks = 100)

## Coda lists --------------------------------------------------------------------

(sumMCMClist<-summary(mcmcList.1))
sumMCMClist$statistics[,1]
effectiveSize(mcmcList.1)
par(plt=posPlot())
plot(mcmcList.1)
raftery.diag(mcmcList.1)
geweke.plot(mcmcList.1)
gelman.plot(mcmcList.1)
autocorr(mcmcList.1)
mcmc_acf(mcmcList.1, pars = c("gamma", "negReward","scaleConst"), 
         lags = 1000)
rejectionRate(mcmcList.1)

denplot(mcmcList.1,collapse = FALSE)
densplot(mcmcList.1[[1]][,1])
densplot(mcmcList.1[[1]][,2])
densplot(mcmcList.1[[1]][,3])

points(x=densGamma$x[sort(densGamma$y,decreasing = TRUE,index.return=T)$ix[1:20]],
       densGamma$y[sort(densGamma$y,decreasing = TRUE,index.return=T)$ix[1:20]],
       col="red")

median(c(mcmcList[[1]][,1],mcmcList[[2]][,1],mcmcList[[3]][,1]))
(c(mcmcList[[1]][,2],mcmcList[[2]][,2],mcmcList[[3]][,2]))
sumMCMClist

densEta<-density(c(mcmcList[[1]][,2],mcmcList[[1]][,2],mcmcList[[1]][,2]))
densEta$x[densEta$y==max(densEta$y)]

densGamma<-density(c(mcmcList[[1]][,1],mcmcList[[1]][,1],mcmcList[[1]][,1]))
densGamma$x[densGamma$y==max(densGamma$y)]

densScal<-density(c(mcmcList[[1]][,3],mcmcList[[1]][,3],mcmcList[[1]][,3]))
densScal$x[densScal$y==max(densScal$y)]



str(mcmcList[[1]])


funcK.gammaD<-function(mod,v){
  k1<-(2+(mod^2)/v +mod*sqrt((4+(mod^2)/v)/v))/2
  k2<-(2+(mod^2)/v -mod*sqrt((4+(mod^2)/v)/v))/2
  if(k1>0 || k2 <0) return(k1)
  else if(k1<0 || k2 >0) return(k2)
  else return("error")
}
funcTheta.gammaD<-function(mod,v){
  sqrt(v/funcK.gammaD(mod,v))
}

# parameterazing gamma distribution
mode<-0.5;varGam<-0.001
funcK.gammaD(mode,varGam)
funcTheta.gammaD(mode,varGam)

k<-funcK.gammaD(mode,varGam);theta<-funcTheta.gammaD(mode,varGam)
# k<-1;theta<-2
plot(y=dgamma(seq(0,5,length.out = 2000),k,scale = theta),
     x=seq(0,5,length.out = 2000),type="l")
lines(x=c(k*theta,k*theta),y=c(0,10))
lines(x=c((k-1)*theta,(k-1)*theta),y=c(0,10))
lines(x=rep(1.75,2),y=c(0,10))
lines(x=rep(1.86,2),y=c(0,10))
k*theta^2

dgamma(x = 1.86,shape = k,scale=theta)

factorial(k-1)

1.86^(k-1)*exp(-1.86/theta)/(gamma(k)*theta^k)

1/(theta^k *lgamma(k)) *1.86^(k-1) *exp(-(1.86/theta))

gamma(k)

k^theta

# parameterazing beta distribution
alph.beta<-1;beta.beta<-1

# alph.beta<-1;beta.beta<-1

plot(y=dbeta(seq(0,1,length.out = 200),shape1 = alph.beta,shape2 = beta.beta),
     x=seq(0,1,length.out = 200),type="l")
lines(x=rep((alph.beta-1)/(alph.beta+beta.beta-2),2),y=c(0,5))
alph.beta*beta.beta/((alph.beta+beta.beta)^2*(alph.beta+beta.beta+1))







funcAlpha.beta<-function(meanBet,varBet){
  ((meanBet*(1-meanBet)/varBet)-1)*meanBet
}
funcBeta.beta<-function(meanBet,varBet){
  ((meanBet*(1-meanBet)/varBet)-1)*(1-meanBet)
}
meanBet<-0.005; varBet<-0.005
alph.beta<-funcAlpha.beta(meanBet,varBet)
beta.beta<-funcBeta.beta(meanBet,varBet)
alph.beta;beta.beta
plot(y=dbeta(seq(0,1,length.out = 200),shape1 = alph.beta,shape2 = beta.beta),
     x=seq(0,1,length.out = 200),type="l")
lines(x=rep((alph.beta-1)/(alph.beta+beta.beta-2),2),y=c(0,20))
lines(x=rep(meanBet,2),y=c(0,20),col="red")
alph.beta*beta.beta/((alph.beta+beta.beta)^2*(alph.beta+beta.beta+1))

dbeta(0.063217047440181279,shape1 = alph.beta,shape2 = beta.beta)

(gamma(alph.beta) * gamma(beta.beta)) / gamma(alph.beta + beta.beta)
lgamma(alph.beta)+lgamma(beta.beta)-lgamma(alph.beta+beta.beta)

value = exp(
  lgamma(x)
  + lgamma(y)
  - lgamma(x + y));
