##### Analysis of the Aproximate Bayesian Computation to estimating learning
##### parameters in the AC models using Zegni's data

## Libraries

require(here)
library(plot3D) 
require(data.table)
library(coda)
library(mcmcplots)
library(bayesplot) 
library(lattice)
source(here("../R_files/posPlots.R"))

simdir<-"D:/Neuch_simulations/ABCfit/Simulations/"

scen<-"ABCclean_gam_Nrew_exp_sca30_"


# (listFiles<-list.files(paste0(simdir,scen),full.names = TRUE))
(listFiles<-list.files(here("Simulations",scen)))
ABCruns<-grep("chain",listFiles,value = TRUE)


# all in a single object
ABCraw<-do.call(rbind,lapply(ABCruns, function(file){
  rundata<-fread(here("Simulations",scen,file))
  seedNum<-strsplit(file,"_")[[1]][3]
  seedNum<-as.numeric(gsub("[[:alpha:]]",seedNum,replacement = ''))
  rundata[,seed:=rep(seedNum,dim(rundata)[1])]
}))

# One per onject
ABCraw<-fread(here("Simulations",scen,ABCruns[1]))

# All in a list
mcmcList<-mcmc.list(do.call(list,lapply(ABCruns, function(file){
  rundata<-fread(here("Simulations",scen,file))
  mcmcRun<-mcmc(rundata[,.(gamma,negReward)])
  return(mcmcRun)
})))




head(ABCraw)

burn.in<-1000
thinning<-10

# filter

ABCburned<-ABCraw[iteration>burn.in]

ABCfiltered<-ABCraw[iteration>burn.in & iteration %% thinning==0]

ABCburned[seed==1,]
ABC.gamma.time.short<-dcast(ABCburned,iteration~seed,value.var = c("gamma"))
ABC.nRew.time.short<-dcast(ABCburned,iteration~seed,value.var = "negReward")
ABC.logLike.time.short<-dcast(ABCburned,iteration~seed,value.var = "fit")

par(plt=posPlot(numploty = 2,idploty = 2),mfrow=c(1,1),las=1)
matplot(y=ABC.gamma.time.short[,c(2,3,4)],
        x=ABC.gamma.time.short[,iteration],
        type="l",lty=1,col=2:5,ylab = "",
        xlab="",xaxt="n",lwd=0.1)
title(main = expression(gamma),line = -2,cex=3,new=T)
par(plt=posPlot(numploty = 2,idploty = 1),mfrow=c(1,1),las=1,new=TRUE)
matplot(y=ABC.nRew.time.short[,c(2,3,4)],
        x=ABC.nRew.time.short[,iteration],
        type="l",lty=1,col=2:5,ylab = "",
        xlab="",xaxt="n",lwd=0.1)
title(main = expression(eta),line = -2,cex=3)
axis(1)

par(plt=posPlot(numploty = 1,idploty = 1),mfrow=c(1,1),las=1)
matplot(y=ABC.logLike.time.short[,c(2,3,4)],
        x=ABC.logLike.time.short[,iteration],
        type="l",lty=1,col=2:5,ylab = "",
        xlab="",xaxt="n",lwd=0.1)


par(plt=posPlot(numploty = 2,idploty = 2),mfrow=c(1,1),las=1)
matplot(y=ABCburned[,gamma],
        x=ABCburned[,iteration],
        type="l",lty=1,col=2:5,ylab = "",
        xlab="",xaxt="n",lwd=0.1)

title(main = expression(gamma),line = -2,cex=3)
par(plt=posPlot(numploty = 2,idploty = 1),new=TRUE)
matplot(y=ABCburned[,negReward],
        x=ABCburned[,iteration],
        type="l",lty=1,col=2:5,ylab = "",
        xlab="",xaxt="n",lwd=0.1)
title(sub = expression(eta))
axis(1)


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


## MCMC analisis with coda

# ABCraw<-fread(ABCruns[3])
ABCraw<-fread(here("Simulations",scen,ABCruns[3]))

ABC.mcmc<-mcmc(ABCraw[,.(gamma,negReward)])
effectiveSize(ABC.mcmc)

summary(ABC.mcmc)
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


(sumMCMClist<-summary(mcmcList))
sumMCMClist$statistics[,1]
effectiveSize(mcmcList)
par(plt=posPlot())
plot(mcmcList)
raftery.diag(mcmcList)
geweke.plot(mcmcList)
gelman.plot(mcmcList)
autocorr(mcmcList)
mcmc_acf(mcmcList, pars = c("gamma", "negReward"), 
         lags = 100)
rejectionRate(mcmcList)
densGamma<-density(mcmcList[[1]][,1])
densGamma$x[sort(densGamma$y,decreasing = TRUE,index.return=T)$ix[1:10]]
denplot(mcmcList,collapse = FALSE)
densplot(mcmcList[[1]][,1])
densplot(mcmcList[[2]][,2])


points(x=densGamma$x[sort(densGamma$y,decreasing = TRUE,index.return=T)$ix[1:20]],
       densGamma$y[sort(densGamma$y,decreasing = TRUE,index.return=T)$ix[1:20]],
       col="red")



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
