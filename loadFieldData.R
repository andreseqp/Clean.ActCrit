library("data.table")
library("here")

fieldData<-fread(here("Data","market_model.csv"))

str(fieldData)

### Adjust data to fit IBD

fieldData[,rel.abund.cleaners:=abundance_cleaners_100m2/
         (max(abundance_cleaners_100m2)+min(abundance_cleaners_100m2))]
fieldData[,`:=`(rel.abund.visitors=(1-rel.abund.cleaners)*abundance_large_100m2/abundance_clients_100m2,
             rel.abund.residents=(1-rel.abund.cleaners)*abundance_small_100m2/abundance_clients_100m2)]

fieldData[,prob.Vis.Leav:=percentage_swim_off/100]

market.ABC<-fieldData[,.(rel.abund.cleaners,rel.abund.visitors,
                      rel.abund.residents,prob.Vis.Leav,
                      market_binomial)]

fwrite(market.ABC,here("data","data_ABC.txt"),row.names = FALSE,sep = "\t")


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
