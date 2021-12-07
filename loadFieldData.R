library("data.table")
library("here")
library("readxl")
library('dplyr')
library('ggplot2')

## Load field data based on the market experiment performance criteria

fieldData<-fread(here("Data","market_model.csv"))


str(fieldData)
## Summarize ecological data by site

fieldData[,unique(site_year)]
names(fieldData)

fieldData.site<-fieldData[,lapply(.SD,max),by=site_year,
                          .SDcols=c("abundance_large_100m2","abundance_small_100m2",
                                   "abundance_cleaners_100m2","percentage_swim_off")]


fieldData.cleaner<-read_xlsx(here("Data","market_raw_data.xlsx"),sheet = "round_data")

fieldData.cleaner<-data.table(fieldData.cleaner)

str(fieldData.cleaner)

fieldData.cleaner[,use_sims:=ifelse(is.na(use_sims),0,1)]
fieldData.cleaner[,use_sims:=as.logical(use_sims)]

fieldData.filt<-fieldData.cleaner[use_sims==T,]
fieldData.filt[,score_visitor:=as.numeric(score_visitor)]
fieldData.filt[,stage:=as.factor(stage)]

str(fieldData.filt)

fieldData.sum<-fieldData.filt[,sum(score_visitor),
                              by=.(site_year,cleaner_ID)]

str(fieldData.sum)

names(fieldData.sum)[3]<-"score_visitor"
names(fieldData.sum)[1]<-"site.year"

fieldData.sum[,unique(site.year)]
fieldData.site$site_year

fieldData.sum[site.year=="NHS2017",site.year:="NHS 2017"]

fieldData.sum[,abund.cleaners:=
                fieldData.site[match(site.year,site_year),abundance_cleaners_100m2]]

fieldData.sum[order(abund.cleaners),max(abund.cleaners),by=site.year]$site.year==
  
  fieldData.site[order(abundance_cleaners_100m2),.(site_year,abundance_cleaners_100m2)]$site_year

fieldData.sum[,abund.visitors:=
                fieldData.site[match(site.year,site_year),abundance_large_100m2]]

fieldData.sum[,abund.residents:=
                fieldData.site[match(site.year,site_year),abundance_small_100m2]]

fieldData.sum[,prob.Vis.Leav:=
                fieldData.site[match(site.year,site_year),percentage_swim_off*0.01]]

fieldData.sum[,site.year:=gsub(" ",replacement = "_",x = site.year)]

fieldData.sum[,cleaner_ID:=gsub(" ",replacement = "",x = cleaner_ID)]

str(fieldData.sum)

threashold<-1.5

fieldData.sum[,highDens:=abund.cleaners>threashold]
fieldData.sum[,visPref:=sapply(score_visitor,function(x){
  binom.test(x,n=20,p=0.5,alternative = "greater")$p.value<0.05
})]
fieldData.sum[,competence:=highDens+visPref!=1]



absplot<-fieldData.sum %>%
  ggplot(aes(x=abund.cleaners,y=score_visitor,
             color=as.factor(competence)))+
  geom_point()+geom_vline(xintercept = 1.5)+
  scale_color_manual(values=c("#999999", "#E69F00"))+
  labs(color="Competence")+geom_jitter()+
  geom_text(x=1.7,y=2,label="Threshold",inherit.aes = FALSE)+
  theme_classic()

ratioPlot<-fieldData.sum %>%
  ggplot(aes(x=abund.cleaners/(abund.visitors+abund.visitors),y=score_visitor,
             color=as.factor(competence)))+
  geom_point()+#geom_vline(xintercept = 1.5)+
  scale_color_manual(values=c("#999999", "#E69F00"))+
  labs(color="Competence")+geom_jitter(width = 0.0005)+
  theme_classic()

png(here("threshold.png"))
ggarrange(absplot,ratioPlot,labels = c("a","b"),common.legend = TRUE,ncol = 1)
dev.off()


fieldData.site %>%
  ggplot(aes(x=abundance_cleaners_100m2,fill=score_visitor,y=prob.Vis.Leav))+
  geom_point()+theme_classic()

setorder(fieldData.sum,site.year)

fieldData.sum[,`:=`(highDens=NULL,visPref=NULL,competence=as.integer(competence))]

fwrite(fieldData.sum,here("data",paste0("data_ABC_cleaner_abs_threa",threashold,".txt")),
       row.names = FALSE,sep = "\t")

rep(runif(12,min = range(fieldData.sum$abund.cleaners)[1],
          max=range(fieldData.sum$abund.cleaners)[2]),each=10)

ranges.fielsum<-fieldData.sum[,as.list(unlist(lapply(.SD, function(x){list(min=min(x),
                                                            max=max(x))}))),
                              .SDcols=c("abund.cleaners",
                                        "abund.visitors",
                                        "abund.residents",
                                        "prob.Vis.Leav")]

fakeData<-fieldData.sum[,.(site.year,cleaner_ID,score_visitor,
  abund.cleaners=rep(runif(12,min = ranges.fielsum$abund.cleaners.min,
                                                       max=ranges.fielsum$abund.cleaners.max),each=10),
                              abund.visitors=rep(runif(12,min = ranges.fielsum$abund.visitors.min,
                                                       max=ranges.fielsum$abund.visitors.max),each=10),
                              abund.residents=rep(runif(12,min = ranges.fielsum$abund.residents.min,
                                                       max=ranges.fielsum$abund.residents.max),each=10),
                              prob.Vis.Leav=rep(runif(12,min = ranges.fielsum$prob.Vis.Leav.min,
                                                        max=ranges.fielsum$prob.Vis.Leav.max),each=10))]



fwrite(fakeData,here("data",paste0("data_ABC_cleaner_fakePartial.txt")),
       row.names = FALSE,sep = "\t")

### Adjust data to fit IBD

clean.abund.scale<-modeScal

fieldData.sum[,rel.abund.cleaners:=clean.abund.scale*abund.cleaners/
                (clean.abund.scale*abund.cleaners+abund.visitors+abund.residents)]
fieldData.sum[,`:=`(rel.abund.visitors=(1-rel.abund.cleaners)*abund.visitors/(abund.visitors+abund.residents),
                rel.abund.residents=(1-rel.abund.cleaners)*abund.residents/(abund.visitors+abund.residents))]

print(fieldData.sum[,.(max(rel.abund.cleaners),max(abund.cleaners)),by=site.year],nrows = 120)

fieldData[,rel.abund.cleaners1:=clean.abund.scale*abundance_cleaners_100m2/
         (clean.abund.scale*abundance_cleaners_100m2+abundance_clients_100m2)]


fieldData[,`:=`(rel.abund.visitors=(1-rel.abund.cleaners)*abundance_large_100m2/abundance_clients_100m2,
             rel.abund.residents=(1-rel.abund.cleaners)*abundance_small_100m2/abundance_clients_100m2)]

fieldData[,prob.Vis.Leav:=percentage_swim_off/100]

fieldData[,abundance_clients_100m2-abundance_large_100m2-abundance_small_100m2]

fieldData[,.(min(rel.abund.cleaners),max(rel.abund.cleaners))]
fieldData[,.(min(rel.abund.visitors),max(rel.abund.visitors))]
fieldData[,.(min(rel.abund.residents),max(rel.abund.residents))]
fieldData[rel.abund.residents==min(rel.abund.residents),rel.abund.cleaners*rel.abund.visitors]

market.ABC<-fieldData[,.(site_year,rel.abund.cleaners,rel.abund.visitors,
                      rel.abund.residents,prob.Vis.Leav,
                      market_binomial)]

hist(market.ABC[,rel.abund.cleaners])

fwrite(market.ABC,here("data",paste0("data_ABC_",clean.abund.scale,".txt")),
       row.names = FALSE,sep = "\t")

marketABC.site<-market.ABC[,lapply(.SD,mean),by=site_year,.SDcols=c("rel.abund.cleaners",
                                                    "rel.abund.visitors",
                                                    "rel.abund.residents",
                                                    "prob.Vis.Leav",
                                                    "market_binomial")]
marketABC.site[,countMarket:=market.ABC[,sum(market_binomial),by=site_year]$V1]
marketABC.site[,totalMarket:=market.ABC[,length(market_binomial),by=site_year]$V1]
marketABC.site[,site_year:=gsub(" ","_",site_year)]

# marketABC.site.fake<-marketABC.site
# marketABC.site.fake[,prob.Vis.Leav:=1]
# marketABC.site.fake[,rel.abund.cleaners:=seq(0.01,0.95,length.out = 12)]
# marketABC.site.fake[,rel.abund.visitors:=(1-rel.abund.cleaners)/2]
# marketABC.site.fake[,rel.abund.residents:=(1-rel.abund.cleaners)/2]

fwrite(marketABC.site,here("data",paste0("data_ABC_site_",clean.abund.scale,".txt")),
       row.names = FALSE,sep = "\t")


## Load raw field data including the number of visitor choices out of each 
## 10 trial test session

fieldData.raw<-read_xlsx(here("Data","market_raw_data.xlsx"),sheet = "round_data")

fieldData.raw<-data.table(fieldData.raw)

str(fieldData.raw)

fieldData.raw[,use_sims:=ifelse(is.na(use_sims),0,1)]
fieldData.raw[,use_sims:=as.logical(use_sims)]

fieldData.filt<-fieldData.raw[use_sims==T,]
fieldData.filt[,score_visitor:=as.numeric(score_visitor)]
fieldData.filt[,stage:=as.factor(stage)]

str(fieldData.filt)

fieldData.sum<-fieldData.filt[,sum(score_visitor),
                              by=.(site_year,cleaner_ID)]

str(fieldData.sum)

names(fieldData.sum)[3]<-"score_visitor"
names(fieldData.sum)[1]<-"site.year"

fieldData.sum[,unique(site.year)]
marketABC.site$site_year

fieldData.sum[site.year=="NHS2017",site.year:="NHS 2017"]

fieldData.sum[,rel.abund.cleaners:=
                marketABC.site[match(site.year,site_year),rel.abund.cleaners]]

fieldData.sum[order(rel.abund.cleaners),max(rel.abund.cleaners),by=site.year]$site.year==

marketABC.site[order(rel.abund.cleaners),.(site_year,rel.abund.cleaners)]$site_year

fieldData.sum[,rel.abund.visitors:=
                marketABC.site[match(site.year,site_year),rel.abund.visitors]]

fieldData.sum[,rel.abund.residents:=
                marketABC.site[match(site.year,site_year),rel.abund.residents]]

fieldData.sum[,prob.Vis.Leav:=
                marketABC.site[match(site.year,site_year),prob.Vis.Leav]]

fieldData.sum[,site.year:=gsub(" ",replacement = "_",x = site.year)]

fieldData.sum[,cleaner_ID:=gsub(" ",replacement = "",x = cleaner_ID)]

str(fieldData.sum)

fwrite(fieldData.sum,here("data",paste0("data_ABC_cleaner_",clean.abund.scale,".txt")),
       row.names = FALSE,sep = "\t")


### Adjust data to fit IBD

str(fieldData)

fieldData[,rel.abund.cleaners:=10*abundance_cleaners_100m2/
            (abundance_cleaners_100m2+abundance_clients_100m2)]


fieldData[,`:=`(rel.abund.visitors=(1-rel.abund.cleaners)*abundance_large_100m2/abundance_clients_100m2,
                rel.abund.residents=(1-rel.abund.cleaners)*abundance_small_100m2/abundance_clients_100m2)]

fieldData[,prob.Vis.Leav:=percentage_swim_off/100]

fieldData[,.(min(rel.abund.cleaners),max(rel.abund.cleaners))]
fieldData[,.(min(rel.abund.visitors),max(rel.abund.visitors))]
fieldData[,.(min(rel.abund.residents),max(rel.abund.residents))]
fieldData[rel.abund.residents==min(rel.abund.residents),rel.abund.cleaners*rel.abund.visitors]

market.ABC<-fieldData[,.(site_year,rel.abund.cleaners,rel.abund.visitors,
                         rel.abund.residents,prob.Vis.Leav,
                         market_binomial)]


marketABC.site<-market.ABC[,lapply(.SD,mean),by=site_year,.SDcols=c("rel.abund.cleaners",
                                                                    "rel.abund.visitors",
                                                                    "rel.abund.residents",
                                                                    "prob.Vis.Leav",
                                                                    "market_binomial")]
marketABC.site[,countMarket:=market.ABC[,sum(market_binomial),by=site_year]$V1]
marketABC.site[,totalMarket:=market.ABC[,length(market_binomial),by=site_year]$V1]
marketABC.site[,site_year:=gsub(" ","_",site_year)]

marketABC.site.fake<-marketABC.site
marketABC.site.fake[,prob.Vis.Leav:=1]
marketABC.site.fake[,rel.abund.cleaners:=seq(0.01,0.95,length.out = 12)]
marketABC.site.fake[,rel.abund.visitors:=(1-rel.abund.cleaners)/2]
marketABC.site.fake[,rel.abund.residents:=(1-rel.abund.cleaners)/2]

fwrite(marketABC.site,here("data","data_ABC_site.txt"),
       row.names = FALSE,sep = "\t")




head(fieldData.raw,n = 30)

tmp.max.round<-fieldData.raw[,max(round),by=.(site_year,stage,cleaner_ID)]







fieldData[,rowsum(.SD),.SDcol=grep("rel.abund",names(fieldData),value = TRUE)]

plot(data=fieldData,abundance_large_100m2~rel.abund.cleaners)
abline(lm(data=fieldData,abundance_large_100m2~rel.abund.cleaners))

mod.1<-lm(data=fieldData,abundance_large_100m2~rel.abund.cleaners*rel.abund.visitors)
summary(mod.1)
library(car)
avPlots(mod.1)

# Random graphs

par(mfrow=c(2,2))

hist(fieldData[,abundance_clients_100m2])
hist(fieldData[,abundance_large_100m2])
hist(fieldData[,abundance_small_100m2])
hist(fieldData[,abundance_cleaners_100m2])

fieldData[,`:=`(rel_larg_clients=abundance_large_100m2/
                  (abundance_large_100m2+abundance_small_100m2+abundance_cleaners_100m2),
                rel_small_clients=abundance_small_100m2/
            (abundance_large_100m2+abundance_small_100m2+abundance_cleaners_100m2),
            rel_cleaners=abundance_cleaners_100m2/
              (abundance_large_100m2+abundance_small_100m2+abundance_cleaners_100m2),
            prob.Vis.Leav=percentage_swim_off*100)]

par(mfro=c(2,2))

hist(fieldData[,rel_larg_clients])
hist(fieldData[,rel_small_clients])
hist(fieldData[,rel_cleaners])
