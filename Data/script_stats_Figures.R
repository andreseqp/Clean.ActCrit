
#####loading packages-------------

require(car)
require(visreg)
require(ggplot2)
require(broom)
require(gridExtra)
require(cowplot)
require(EnvStats)
require(rsq)
require(psych)
library("wesanderson")
require(lme4)
library(here)
library(data.table)


# set_here(path = "C:/Users/andre/OneDrive - Universidad de los Andes/Neuchatel/Data_model/data/11956158/")
#####Uploading datasets-------------

## creating work space
# setwd("~/Google Drive/1.Files_manuscripts/Quinones, Triki and Bshary/Figshare")

here()

market <- fread(here("data","market_model.csv"),header=TRUE,sep=",")

head(market)

describe(market)



### Standardisation and centering continuous predictors by scaling variables -------

market$sabundance_cleaner<- c(scale(market$abundance_cleaners_100m2, scale=F))
market$scleaner_large_ratio<- c(scale(market$cleaner_large_ratio, scale=F))
market$scleaner_small_ratio<- c(scale(market$cleaner_small_ratio, scale=F))
market$scleaner_client_ratio<- c(scale(market$cleaner_client_ratio, scale=F))
market$spercentage_swim_off<- c(scale(market$percentage_swim_off, scale=F))


## model1: cleaner fish abundance-----

Anova(model1 <- glm(market_binomial~  spercentage_swim_off*
                           poly(sabundance_cleaner,2 )
                          ,family=binomial,data=market))

##Anova(model1 <- glmer(market_binomial~  spercentage_swim_off*
#poly(sabundance_cleaner,2 )+(1|site)+(1|year)
#,family=binomial,data=market))## As the estimates of the random factors turned out to be zero, indicating no effect of site and year, the random factors were then removed from the models 

summary(model1)

rsq(model1) #explianed variance

tidy(model1,conf.int=T,exponentiate=F)## confidence interval

## checking model assumptions
plot(resid(model1))
par(mfrow=c(2,2)); plot(model1); par(mfrow=c(1,1))
plot(model1, which=4)
residualPlots(model1)
influenceIndexPlot(model1)
1 - pchisq(deviance(model1), model1$df.resid)



### Plot of marginal effects: (Fig. 1a)
pal <- wes_palette("Zissou1", 100, type = "continuous")

plot1 <- visreg2d(model1, y= "spercentage_swim_off", x= "sabundance_cleaner",
         zlab="",scale="response",
         plot.type = "gg")+labs(x=expression(paste("\n      Cleaner \ndensity per 100",m^{2})))+ 
ylab("Percentage of visitors \n swimming away if not serviced")+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10,title = NULL))+
  theme(legend.position="none")+
  scale_fill_gradientn(colors=pal,
                       limits=c(0,1))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        axis.title.x=element_text(margin=margin(15)))


plot1

## model2: cleaner fish abundance-----

Anova(model2 <- glm(market_binomial~  spercentage_swim_off*
                      poly(scleaner_client_ratio,2 )
                    ,family=binomial,data=market))

summary(model2)

rsq(model2)#explianed variance

tidy(model2,conf.int=T,exponentiate=F)## confidence interval

## checking model assumptions
plot(resid(model2))
par(mfrow=c(2,2)); plot(model2); par(mfrow=c(1,1))
plot(model2, which=4)
residualPlots(model2)
influenceIndexPlot(model2)
1 - pchisq(deviance(model2), model2$df.resid)



### Plot of marginal effects: (Fig. 1b)

plot2 <- visreg2d(model2, y= "spercentage_swim_off", x= "scleaner_client_ratio",
         zlab="",scale="response",
         plot.type = "gg")+xlab("Cleaner to \n 100 clients ratio")+ ylab("")+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10,title = NULL))+
  scale_fill_gradientn(colors=pal,
                       limits=c(0,1))+theme(legend.position="none")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
visreg2d(model2, y= "spercentage_swim_off", x= "scleaner_client_ratio",
         zlab="",scale="response",plot.type="persp",ylim=c(0,0.2),)

plot2
## model3: cleaner fish abundance-----

Anova(model3 <- glm(market_binomial~  spercentage_swim_off*
                      poly(scleaner_large_ratio,2 )
                    ,family=binomial,data=market))


summary(model3)

rsq(model3)#explianed variance

tidy(model3,conf.int=T,exponentiate=F)## confidence interval

## checking model assumptions
plot(resid(model3))
par(mfrow=c(2,2)); plot(model3); par(mfrow=c(1,1))
plot(model3, which=4)
residualPlots(model3)
influenceIndexPlot(model3)
1 - pchisq(deviance(model3), model3$df.resid)



### Plot of marginal effects: (Fig. 1c)

plot3 <- visreg2d(model3, y= "spercentage_swim_off", x= "scleaner_large_ratio",
         zlab="",scale="response",
         plot.type = "gg")+xlab("Cleaner to 100 \n large clients ratio")+ ylab("")+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10,title = NULL))+
  scale_fill_gradientn(colors=pal,
                       limits=c(0,1))+theme(legend.position="none")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

plot3
## model4: cleaner fish abundance-----

Anova(model4 <- glm(market_binomial~  spercentage_swim_off*
                      poly(scleaner_small_ratio,2 )
                    ,family=binomial,data=market))


summary(model4)

rsq(model4)#explianed variance

tidy(model4,conf.int=T,exponentiate=F)## confidence interval

## checking model assumptions
plot(resid(model4))
par(mfrow=c(2,2)); plot(model4); par(mfrow=c(1,1))
plot(model4, which=4)
residualPlots(model4)
influenceIndexPlot(model4)
1 - pchisq(deviance(model4), model4$df.resid)



### Plot of marginal effects: (Fig. 1d)

plot4 <- visreg2d(model4, y= "spercentage_swim_off", x= "scleaner_small_ratio",
         zlab="",scale="response",
         plot.type = "gg")+xlab("Cleaner to 100 \n small clients ratio")+ylab("")+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10,title = NULL))+
  scale_fill_gradientn(colors=pal,
                       limits=c(0,1))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

plot4


prow <- plot_grid(plot1,plot2,plot3, plot4+theme(legend.position="none"),
                   ncol=4,label_size=12, align = c("h", "V"), 
                   labels=c("auto"), label_x=0.08)

legend <- get_legend(plot4+theme(legend.position="right"))

Figure1 <- plot_grid(prow, legend, ncol=2, rel_widths = c(4, 0.5))
Figure1

