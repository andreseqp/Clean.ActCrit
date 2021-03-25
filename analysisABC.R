##### Analysis of the Aproximate Bayesian Computation to estimating learning
##### parameters in the AC models using Zegni's data

## Libraries

require(here)
require(data.table)

listFiles<-list.files(here("Simulations","ABCtest_"),full.names = TRUE)

ABCraw<-fread(listFiles[1])
names(ABCraw)<-c("iteration","alphaAct","alphaCrit","gamma","NegRew","fit")

ABCraw
