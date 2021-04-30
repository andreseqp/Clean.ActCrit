##### Analysis of the Aproximate Bayesian Computation to estimating learning
##### parameters in the AC models using Zegni's data

## Libraries

require(here)
require(data.table)
source("../R_files/posPlots.R")

(listFiles<-list.files(here("Simulations","ABC_gam_Nrew_"),full.names = TRUE))

ABCraw<-fread(listFiles[1],)

head(ABCraw)

par(plt=posPlot(numploty = 2,idploty = 2),mfrow=c(1,1))
matplot(y=ABCraw[,c(4,5)],x=ABCraw[,1],type="l",lty=1,col=2:5,ylab = "Parameters",
        xlab="",xaxt="n")
legend("topleft",col = 2:5,legend = names(ABCraw)[c(4,5)],lty=1)
par(plt=posPlot(numploty = 2,idploty = 1),new=TRUE)
matplot(y=ABCraw[,6],x=ABCraw[,1],type="l",yaxt="s",xaxt="s",ylab="Fit",xlab="")


hist(ABCraw$gamma)

plot(ratio~iteration,data=ABCraw,type="l",ylab="ratio",xlab="iterations")

rbinom(100,1,prob = 0.5)
