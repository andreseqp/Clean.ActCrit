##### Analysis of the Aproximate Bayesian Computation to estimating learning
##### parameters in the AC models using Zegni's data

## Libraries

require(here)
library(plot3D) 
require(data.table)
source("../R_files/posPlots.R")

(listFiles<-list.files(here("Simulations","ABC_gam_Nrew_Prior_"),full.names = TRUE))

ABCraw<-fread(listFiles[1],)

head(ABCraw)

par(plt=posPlot(numploty = 2,idploty = 2),mfrow=c(1,1))
matplot(y=ABCraw[1:20000,c(4,5)],x=ABCraw[1:20000,1],type="l",lty=1,col=2:5,ylab = "Parameters",
        xlab="",xaxt="n")
legend("left",col = 2:5,legend = names(ABCraw)[c(4,5)],lty=1)
par(plt=posPlot(numploty = 2,idploty = 1),new=TRUE)
matplot(y=ABCraw[1:20000,6],x=ABCraw[1:20000,1],type="p",yaxt="s",xaxt="s",
        ylab="log-likelihood",xlab="",cex = 0.05,pch = 20)


matplot(y=ABCraw[1:50,6],x=ABCraw[1:50,1],type="p",yaxt="s",xaxt="s",
        ylab="log-likelihood",xlab="",cex = 0.1)

hist(ABCraw$gamma)

hist(ABCraw$negReward)

hist(ABCraw$fit)


plot(negReward~gamma, data = ABCraw,type="p",
     ylab=expression(eta),xlab=expression(gamma),cex = 0.1)



##  Create cuts:
gamma_c <- cut(ABCraw$gamma, 20)
ngRew_c <- cut(ABCraw$negReward, 20)

##  Calculate joint counts at cut levels:
z <- table(gamma_c, ngRew_c)/sum(table(gamma_c, ngRew_c))



##  Plot as a 3D histogram:
hist3D(z=z, border="black")

##  Plot as a 2D heatmap:

image2D(z=z, border="black",y=seq(min(ABCraw$negReward),max(ABCraw$negReward),
                                  length.out = ncol(z)),
        x=seq(min(ABCraw$gamma),max(ABCraw$gamma),
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


