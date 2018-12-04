# -------------- generate json files with parameters------------------------------------------ #

library("jsonlite")

projDir<-"D:/quinonesa/learning_models_c++/actCrit/"

simsDir<-"S:/quinonesa/Simulations/actCrit/"

visualDir<-"D:\\quinonesa/VisualStudio/ActCrit/"

exedir<-paste(projDir,'/./ActCrit.exe',sep='')

fileName<-"parameters.json"

# Baseline parameter values
param<-list(totRounds=20000,ResReward=1,VisReward=1,
            ResProb=c(0.2),
            VisProb=c(0.2),
            ResProbLeav=0,VisProbLeav=1,negativeRew=-0.5,experiment=FALSE,
            inbr=0,outbr=0,trainingRep=30,forRat=0.0,
            alphaT=0.01,printGen=1,seed=1, gammaRange=c(0,0.8),
            tauRange=c(1),netaRange=c(0,1),alphaThRange=c(0.01),
            folder=simsDir)

setwd(simsDir)

# function that creates new folders for simulations results --------------------
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

# Arrays with the values of expernal parameters
rangLeav<-seq(0.2,0.8,by = 0.2)
rangAbund<-seq(0,0.9,length=10)

# check_create.dir(simsDir,param = rep("AbundanceLpr",4),
#                  values = rangLeav)

listfolders<-check_create.dir(paste(simsDir,"InitVal1_/",sep=""),
                                    param = rep("AbundanceLpr",4),
                              values = rangLeav)


# Loop through parameter names and values creating JSONs -----------------------
for (i in 1:4) {
  param$folder<-paste(simsDir,"InitVal1_/",listfolders[i],"/",sep="")
  param$ResProb<-rangAbund
  param$VisProb<-rangAbund
  param$VisProbLeav<-rangLeav[i]
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
        write(outParam,paste(param$folder,fileName,sep = "/"))
      }
    }
  }
  else{
    write(outParam,paste(param$folder,fileName,sep = "/"))
  }
  # Uncomment for running simulations directly through R:
  system(paste(exedir,
    gsub("\\","/",paste(param$folder,fileName,sep="\\"),fixed=TRUE)
    ,sep = " "))
}






