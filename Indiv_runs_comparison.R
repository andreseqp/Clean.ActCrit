############### Plot individual runs ###########################################

# Load libraries and external functions -----------------------------------

projDir<-"d:/quinonesa/learning_models_c++/actCrit/"
simsDir<-"s:/quinonesa/Simulations/actCrit/InitVal1_/"


source('d:/quinonesa/Dropbox/R_files/posPlots.R')
source(paste(projDir,"aesth_par.R",sep=""))
source(paste(projDir,"loadData.R",sep = ""))
source('D:/quinonesa/Dropbox/R_files/posPlots.R')

# Individual runs exploration FIA------------------------------------------------

setwd(simsDir)

listParRuns<-c("AbundanceLpr","gamma","neta")
listValRuns<-c(1,0.8,0)


tmp3<-do.call(rbind,
              lapply(getFilelist(simsDir,listParRuns,listValRuns)$FIA, 
                     function(x){
                       if(as.numeric(gsub("[[:alpha:]]",
                                                    strsplit(x,"_")[[1]][7],
                                                    replacement=""))==
                          as.numeric(gsub("[[:alpha:]]",
                                          strsplit(x,"_")[[1]][8],
                                          replacement=""))){
                         fread(x)}
                     }))


# tmp3<-do.call(rbind,
#               lapply(grep("pV0.7",
#                           getFilelist(simsDir,listParRuns,listValRuns)$FIA,
#                           value=TRUE),fread))

tmp3agg<-tmp3[, as.list(unlist(lapply(.SD, function(x) 
  list(mean = mean(x),IQ.h = fivenum(x)[4],IQ.l=fivenum(x)[2])))),
  by=.(Age,Alpha,Gamma,Tau,Neta,Outbr,pR,pV), 
  .SDcols=c('ThetaV','ThetaR','RV','VV','RR','R0','V0','00_')]

tmp3agg[,prefer:=logist(ThetaV.mean,ThetaR.mean)]

setnames(tmp3agg,"00_.mean","AA.mean")


pVlines<-dcast(tmp3agg,formula = Age~pR,value.var = "prefer")

names(pVlines)[2:dim(pVlines)[2]]<-paste0("pR_",
                                          names(pVlines)[2:dim(pVlines)[2]])

# png(paste("d:/quinonesa/Dropbox/Neuchatel/Figs/Actor_critic/",
#           "Runs_visLeav_0_pA0.2.png",sep=""),
#     width=1000,height=1000)


rangplot<-c(0.1,0.2,0.3,0.4)

par(plt=posPlot(numplotx = 1,numploty = 2,idplotx = 1,idploty = 2),yaxt='s')
with(pVlines,{
  matplot(cbind(Age,Age,Age,Age),
          cbind(get(paste("pR_",rangplot[1],sep="")),
                get(paste("pR_",rangplot[2],sep="")),
                get(paste("pR_",rangplot[3],sep="")),
                get(paste("pR_",rangplot[4],sep=""))),type='l',xlab='',
          ylab = "p(V)",lty=1,xaxt='n')
})

legend("bottomright",legend=c(rangplot[1],
                              rangplot[2],
                              rangplot[3],
                              rangplot[4]),
       title = expression(p[v]),
       col=1:5,pch=20,pt.cex = 3,
       ncol=1)

text(x=rep(80,2),y=c(0.85,0.7),labels = c("Actor-Critic FIA",
                                          expression(gamma==0.8)))

# ylimline<-c(min(tmp3agg[(pR==rangplot[1]||pR==rangplot[2])||
#                           pR==rangplot[3],.(RV.mean,VV.mean,RR.mean,R0.mean,
#                                             V0.mean,AA.mean)]),
#             max(tmp3agg[(pR==rangplot[1]||pR==rangplot[2])||
#                           pR==rangplot[3],.(RV.mean,VV.mean,RR.mean,R0.mean,
#                                             V0.mean,AA.mean)]))

ylimline<-c(4.6,
            max(tmp3agg[(pR==rangplot[1]||pR==rangplot[2])||
                          pR==rangplot[3],.(RV.mean,VV.mean,RR.mean,R0.mean,
                                            V0.mean,AA.mean)]))

par(plt=posPlot(numplotx = 3,numploty = 2,idplotx = 1,idploty = 1),yaxt="s",
    xaxt="s",new=TRUE)
with(tmp3agg[pR==rangplot[1]],{
  matplot(cbind(Age,Age,Age,Age,Age,Age),
          cbind(RV.mean,VV.mean,RR.mean,V0.mean,R0.mean,AA.mean),type='l',
          col=colorValues,ylab = "Value",lty=1,xlab = '',ylim=ylimline)
  text(x=2000,y=4.8,labels = bquote(p[v]==.(pV)))
  text(x=2000,y=4.7,labels = bquote(p[r]==.(pR)))
})

legend("bottomright",legend=c("RV","VV",
                              "RR","VA",
                              "RA","AA"),
       col=colorValues,pch=20,pt.cex = 1,title="States values",
       ncol=2,cex=1)

par(plt=posPlot(numplotx = 3,numploty = 2,idplotx = 2,idploty = 1),
    new=TRUE,yaxt="n",xaxt="s")
with(tmp3agg[pR==rangplot[2]],{
  matplot(cbind(Age,Age,Age,Age,Age,Age),
          cbind(RV.mean,VV.mean,RR.mean,V0.mean,R0.mean,AA.mean),type='l',
          col=colorValues,ylab = "",lty=1,xlab='',ylim=ylimline)
  text(x=2000,y=4.8,labels = bquote(p[v]==.(pV)))
  text(x=2000,y=4.7,labels = bquote(p[r]==.(pR)))
})

par(plt=posPlot(numplotx = 3,numploty = 2,idplotx = 3,idploty = 1),
    new=TRUE,yaxt="n",xaxt='s')
with(tmp3agg[pR==rangplot[3]],{
  matplot(cbind(Age,Age,Age,Age,Age,Age),
          cbind(RV.mean,VV.mean,RR.mean,V0.mean,R0.mean,AA.mean),type='l',
          col=colorValues,ylab = "",lty=1,xlab='',ylim=ylimline)
  text(x=2000,y=4.8,labels = bquote(p[v]==.(pV)))
  text(x=2000,y=4.7,labels = bquote(p[r]==.(pR)))
})

dev.off()

par(plt=posPlot(numplotx = 3,idploty = 1,idplotx = 1,numploty = 2)
    ,xaxt="s",new=TRUE,yaxt='s')
with(tmp3agg[pR==0.3],{
  plot(logist(ThetaV.mean,ThetaR.mean)~Age,type="l",ylim = c(0,1),
       ylab="Theta")
})

par(plt=posPlot(numplotx = 3,idploty = 1,idplotx = 2,numploty = 2)
    ,xaxt="s",new=TRUE,yaxt='n')
with(tmp3agg[pR==0.2],{
  plot(logist(ThetaV.mean,ThetaR.mean)~Age,type="l",ylab="",ylim = c(0,1))
})

par(plt=posPlot(numplotx = 3,idploty = 1,idplotx = 3,numploty = 2)
    ,xaxt="s",new=TRUE,yaxt='n')
with(tmp3agg[pR==0.1],{
  plot(logist(ThetaV.mean,ThetaR.mean)~Age,type="l",ylab="",ylim = c(0,1))
})


# Individual runs exploration PIA------------------------------------------------

setwd(simsDir)

listParRuns<-c("BaseLine")
listValRuns<-c("")

strsplit(getFilelist(simsDir,listParRuns,listValRuns)$PIA[1],"_")[[1]]

getParam(simsDir,listParRuns,listParRuns)

tmp3PIA<-do.call(rbind,
                 lapply(getFilelist(simsDir,listParRuns,listValRuns)$PIA, 
                        function(x){
                          if(round(sum(as.numeric(gsub("[[:alpha:]]",
                                                       strsplit(x,"_")[[1]][7:8],
                                                       replacement=""))),1)==0.4){
                            fread(x)
                          }}))

tmp3PIA<-do.call(rbind,
              lapply(grep("pR0.8",
                          getFilelist(simsDir,listParRuns,listValRuns)$PIA,
                          value=TRUE),fread))

tmp3PIA<-do.call(rbind,
                 lapply(getFilelist(simsDir,listParRuns,listValRuns)$PIA,
                             fread))


tmp3aggPIA<-tmp3PIA[, as.list(unlist(lapply(.SD, function(x) 
  list(mean = mean(x),IQ.h = fivenum(x)[4],IQ.l=fivenum(x)[2])))),
  by=.(Age,Alpha,Gamma,Tau,Neta,Outbr,pR,pV), 
  .SDcols=c('ThetaV','ThetaR','Resident','Visitor','Absence')]
tmp3aggPIA[,prefer:=logist(ThetaV.mean,ThetaR.mean)]

par(plt=posPlot(numplotx = 2,idplotx = 1))
plot(prefer~Age,data=tmp3aggPIA[Neta==0],type="p",
     col=colboxes[match(Gamma,unique(Gamma))])
legend("bottomright",legend = unique(tmp3aggPIA$Gamma),col = colboxes,pch = 1)
par(plt=posPlot(numplotx = 2,idplotx = 2),new=TRUE)
plot(prefer~Age,data=tmp3aggPIA[Neta==1],type="p",
     col=colboxes[match(Gamma,unique(Gamma))])


unique(tmp3aggPIA[,pV])

pVlines<-dcast(tmp3aggPIA,formula = Age~pV,
               value.var = "prefer")

names(pVlines)[2:dim(pVlines)[2]]<-paste0("pv_",
                                          names(pVlines)[2:dim(pVlines)[2]])

png(paste("d:/quinonesa/Dropbox/Neuchatel/Figs/Actor_critic/",
          "Runs_visLeav1_pA0.8_initVal0.png",sep=""),
    width=1000,height=1000)

rangplot<-c(0.0,0.1,0.2)

par(plt=posPlot(numplotx = 1,numploty = 2,idplotx = 1,idploty = 2),yaxt='s')
with(pVlines,{
  matplot(cbind(Age,Age,Age),
          cbind(get(paste("pv_",rangplot[1],sep="")),
                get(paste("pv_",rangplot[2],sep="")),
                get(paste("pv_",rangplot[3],sep=""))),type='l',xlab='',
          ylab = "p(V)",lty=1,xaxt='n')
})

with(pVlines,{
  plot(Age,get(paste("pv_",rangplot[2],sep="")),type='l',xlab='',
          ylab = "p(V)",lty=1,xaxt='n')
})


legend("bottomright",legend=c(rangplot[1],
                              rangplot[2],
                              rangplot[3]),
       title = expression(p[v]),
       col=1:5,pch=20,pt.cex = 3,
       ncol=1)

ylimline<-c(min(tmp3aggPIA[(pV==rangplot[1]||pV==rangplot[2])||
                          pV==rangplot[3],.(Resident.mean,Visitor.mean,
                                            Absence.mean)]),
            max(tmp3aggPIA[(pV==rangplot[1]||pV==rangplot[2])||
                          pv==rangplot[3],.(Resident.mean,Visitor.mean,
                                            Absence.mean)]))

par(plt=posPlot(numplotx = 3,numploty = 2,idplotx = 1,idploty = 1),yaxt="s",
    xaxt="s",new=TRUE)
with(tmp3aggPIA[pV==rangplot[1]],{
  matplot(cbind(Age,Age,Age),
          cbind(Resident.mean,Visitor.mean,Absence.mean),type='l',
          col=colorValues,ylab = "Value",lty=1,xlab="",ylim=ylimline)
  text(x=2000,y=3.5,labels = bquote(p[v]==.(pV)))
  text(x=2000,y=2.5,labels = bquote(p[r]==.(pR)))
})

legend("bottomright",legend=c("resident","visitor",
                              "absence"),
       col=colorValues,pch=20,pt.cex = 3,title="States values",
       ncol=1)

par(plt=posPlot(numplotx = 3,numploty = 2,idplotx = 2,idploty = 1),
    new=TRUE,yaxt="n",xaxt="s")
with(tmp3aggPIA[pV==rangplot[2]],{
  matplot(cbind(Age,Age,Age),
          cbind(Resident.mean,Visitor.mean,Absence.mean),type='l',
          col=colorValues,ylab = "",lty=1,xlab="time",ylim=ylimline)
  text(x=2000,y=3.5,labels = bquote(p[v]==.(pV)))
  text(x=2000,y=2.5,labels = bquote(p[r]==.(pR)))
})

par(plt=posPlot(numplotx = 3,numploty = 2,idplotx = 3,idploty = 1),
    new=TRUE,yaxt="n",xaxt="s")
with(tmp3aggPIA[pV==rangplot[3]],{
  matplot(cbind(Age,Age,Age),
          cbind(Resident.mean,Visitor.mean,Absence.mean),type='l',
          col=colorValues,ylab = "",lty=1,xlab="",ylim=ylimline)
  text(x=2000,y=3.5,labels = bquote(p[v]==.(pV)))
  text(x=2000,y=2.5,labels = bquote(p[r]==.(pR)))
})

dev.off()

############ Miscelaneus ######################################################

par(plt=posPlot(numplotx = 3,idploty = 1,idplotx = 1,numploty = 2)
    ,xaxt="s",new=TRUE,yaxt='s')
with(tmp3aggPIA[pR==pRrange[1]],{
  plot(logist(ThetaV.mean,ThetaR.mean)~Age,type="l",ylim = c(0.3,0.7),
       ylab="Theta")
  lines(x = c(0,20000),y = c(0.5,0.5),col="grey")
})

par(plt=posPlot(numplotx = 3,idploty = 1,idplotx = 2,numploty = 2)
    ,xaxt="s",new=TRUE,yaxt='n')
with(tmp3agg[pR==pRrange[2]],{
  plot(logist(ThetaV.mean,ThetaR.mean)~Age,type="l",ylab="",ylim = c(0.3,0.7))
  lines(x = c(0,20000),y = c(0.5,0.5),col="grey")
})

par(plt=posPlot(numplotx = 3,idploty = 1,idplotx = 3,numploty = 2)
    ,xaxt="s",new=TRUE,yaxt='n')
with(tmp3agg[pR==pRrange[3]],{
  plot(logist(ThetaV.mean,ThetaR.mean)~Age,type="l",ylab="",ylim = c(0.3,0.7))
  lines(x = c(0,20000),y = c(0.5,0.5),col="grey")
})


png(paste("d:/quinonesa/Dropbox/Neuchatel/Figs/Actor_critic/",
          "Runs_visLeav1_pA0.8_initVal0.png",sep=""),
    width=1000,height=1000)

rangplot<-c(0.0,0.1,0.2)

par(plt=posPlot(numplotx = 1,numploty = 2,idplotx = 1,idploty = 2),yaxt='s')

with(pVlines,{
  plot(Age,get(paste("pv_",rangplot[2],sep="")),type='l',xlab='',
       ylab = "p(V)",lty=1,xaxt='n')
})



ylimline<-c(min(tmp3aggPIA[(pV==rangplot[1]||pV==rangplot[2])||
                             pV==rangplot[3],.(Resident.mean,Visitor.mean,
                                               Absence.mean)]),
            max(tmp3aggPIA[(pV==rangplot[1]||pV==rangplot[2])||
                             pv==rangplot[3],.(Resident.mean,Visitor.mean,
                                               Absence.mean)]))



par(plt=posPlot(numplotx = 1,numploty = 2,idplotx = 1,idploty = 1),
    new=TRUE,yaxt="s",xaxt="s")
with(tmp3aggPIA[pV==rangplot[2]],{
  matplot(cbind(Age,Age,Age),
          cbind(Resident.mean,Visitor.mean,Absence.mean),type='l',
          col=colorValues,ylab = "",lty=1,xlab="time",ylim=ylimline)
  text(x=1000,y=4.5,labels = bquote(p[v]==.(pV)))
  text(x=1000,y=4,labels = bquote(p[r]==.(pR)))
})
legend("bottomright",legend=c("resident","visitor",
                              "absence"),
       col=colorValues,pch=20,pt.cex = 3,title="States values",
       ncol=1)



dev.off()
