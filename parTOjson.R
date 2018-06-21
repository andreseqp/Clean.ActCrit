# ------------------------ generate json files  ------------------------------------------ #

library("jsonlite")

projDir<-"D:/quinonesa/learning_models_c++/actCrit/"

simsDir<-"S:/quinonesa/Simulations/actCrit/"

visualDir<-"D:\\quinonesa/VisualStudio/ActCrit/"

exedir<-paste(projDir,'/./ActCrit.exe',sep='')

fileName<-"parameters.json"


# test<-fromJSON(paste(codedir,"\\test.json",sep=""))

# param<-list(totRounds=20000,ResReward=10,VisReward=10,ResProb=0.2,VisProb=0.2,
#             ResProbLeav=0,VisProbLeav=1,negativeRew=-10,experiment=FALSE,
#             inbr=0,outbr=0,trainingRep=30,forRat=0.0,
#             alphaT=0.01,printGen=1,seed=1, gammaRange=c(0,0.8),
#             tauRange=c(5,10),netaRange=c(0,0.5),alphaThRange=c(0.01),
#             folder=simsDir)

param<-list(totRounds=20000,ResReward=1,VisReward=1,
            ResProb=0.2,
            VisProb=0.2,
            ResProbLeav=0,VisProbLeav=1,negativeRew=-1,experiment=FALSE,
            inbr=0,outbr=0,trainingRep=30,forRat=0.0,
            alphaT=0.01,printGen=1,seed=1, gammaRange=c(0,0.8),
            tauRange=c(1),netaRange=c(0,1),alphaThRange=c(0.01),
            folder=simsDir, initVal=0)

setwd(simsDir)

check_create.dir<-function(folder,param,values){
  setwd(folder)
  listfolders<-paste(param,values,"_",sep = "")  
  currFolders<-lapply(listfolders,dir.exists)
  if(sum(currFolders>0)){
    warning("At least one of the folders already exists \n Please check",
            immediate. = TRUE)
    print(cbind(listfolders,currFolders))
    ans<-readline("Want to continue?")
    if(substr(ans, 1, 1) == "y"){
      lapply(listfolders,dir.create)
      return(listfolders)
    }
    else{
      return(listfolders)
    }
  }else{
    lapply(listfolders,dir.create)
    return(listfolders)
  }
}

rang<-c(0,1)
rangAbund<-seq(0,0.9,length=10)

check_create.dir(simsDir,param = rep("InitVal",1),
                 values = c(4))

listfolders<-check_create.dir(paste(simsDir,"InitVal4_/",sep=""),
                                    param = rep("AbundanceLpr",2),
                              values = rang)


for (i in 1:2) {
  param$folder<-paste(paste(simsDir,"InitVal4_/",sep=""),
                      listfolders[i],'/',sep='')
  param$ResProb<-rangAbund
  param$VisProb<-rangAbund
  param$initVal<-4
  param$VisProbLeav<-rang[i]
  param$totRounds<-20000
  param$printGen<-100
  outParam<-toJSON(param,auto_unbox = TRUE,pretty = TRUE)
  if(file.exists(paste(param$folder,fileName,sep = ''))){
    currFile<-fromJSON(paste(param$folder,fileName,sep = ''))
    if(sum(unlist(currFile)!=unlist(param))>0){
      warning("You are erasing old files!! n\ Check first!!!",immediate. = TRUE)
      print("OLD value")
      print(unlist(currFile)[unlist(currFile)!=unlist(param)])
      print("NEW value")
      print(unlist(param)[unlist(currFile)!=unlist(param)])
      ans<-readline("Want to continue?")
      if(substr(ans, 1, 1) == "y"){
        write(outParam,paste(param$folder,fileName,sep = ""))
      }
    }
  }
  else{
    write(outParam,paste(param$folder,fileName,sep = ""))
  }
  # system(paste(exedir,
  #   gsub("\\","/",paste(simsdir,listfolders[i],fileName,sep="\\"),fixed=TRUE)
  #   ,sep = " "))
}
gsub(pattern = "\\",replacement = "/",simsdir,fixed=TRUE)

# system(paste(exedir,
#              gsub("\\","/",paste(simsdir,listfolders[1],fileName,sep="\\"),fixed=TRUE)
#              ,sep = " "))



testParam$folder<-paste(simsDir,"test_/",sep = "")
outParam<-toJSON(testParam,auto_unbox = TRUE,pretty = TRUE)
write(outParam,paste(visualDir,"test.json",sep="/"))



