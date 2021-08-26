##### Analysis of the Aproximate Bayesian Computation to estimating learning
##### parameters in the AC models using Zegni's data

## Libraries

require(here)
library(plot3D) 
require(data.table)
library(coda)
source("../R_files/posPlots.R")

simdir<-"D:/Neuch_simulations/ABCfit/Simulations/"

scen<-"ABC_gam_Nrew_"


(listFiles<-list.files(paste0(simdir,scen),full.names = TRUE))
ABCruns<-grep("chain",listFiles,value = TRUE)


ABCraw<-do.call(rbind,lapply(ABCruns, function(file){
  rundata<-fread(here("Simulations",scen,file))
  seedNum<-strsplit(file,"_")[[1]][4]
  seedNum<-as.numeric(gsub("[[:alpha:]]",seedNum,replacement = ''))
  rundata[,seed:=rep(seedNum,dim(rundata)[1])]
}))

ABCraw<-fread(here("Simulations",scen,ABCruns[2]))


head(ABCraw)

burn.in<-1000
thinning<-10

# filter

ABCburned<-ABCraw[iteration>burn.in]

ABCfiltered<-ABCraw[iteration>burn.in & iteration %% thinning==0]

ABCburned[seed==1]
ABC.gamma.time.short<-dcast(ABCburned,iteration~seed,value.var = c("gamma"))
ABC.nRew.time.short<-dcast(ABCburned,iteration~seed,value.var = "negReward")

par(plt=posPlot(numploty = 2,idploty = 2),mfrow=c(1,1),las=1)
matplot(y=ABCburned[,gamma],
        x=ABCburned[,iteration],
        type="l",lty=1,col=2:5,ylab = "",
        xlab="",xaxt="n")
title(main = expression(gamma),line = -2,cex=3)
par(plt=posPlot(numploty = 2,idploty = 1),new=TRUE)
matplot(y=ABCburned[,negReward],
        x=ABCburned[,iteration],
        type="l",lty=1,col=2:5,ylab = "",
        xlab="",xaxt="n")
title(sub = expression(eta))
axis(1)


par(plt=posPlot(numploty = 1,idploty = 1))
matplot(y=ABCburned[,6],x=ABCburned[,1],
        type="p",yaxt="s",xaxt="s",
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

ABCraw<-fread(ABCruns[1])

ABC.mcmc<-mcmc(ABCraw[,.(gamma,negReward)])
effectiveSize(ABC.mcmc)

summary(ABC.mcmc)
plot(ABC.mcmc)


