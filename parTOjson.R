# -------------- generate json files with parameters------------------------------------------ #

library("jsonlite")
library("here")
source(here("loadData.R"))

simsDir<-"Simulations"

exedir<-'/./ActCrit.exe'

fileName<-"parameters.json"

scenario<-"ABCtest"

# Baseline parameter values
param<-list(totRounds=20000,ResReward=1,VisReward=1,
            ResProb=c(0.2),
            VisProb=c(0.2),
            ResProbLeav=0,VisProbLeav=1,negativeRew=-0.5,scenario=0,
            inbr=0,outbr=0,trainingRep=30,forRat=0.0,
            alphaT=0.01,printGen=1,seed=1, gammaRange=c(0,0.8),
            netaRange=c(0,1),alphaThRange=c(0.01,0.02,0.03),numlearn=1,
            propfullPrint = 0.7,
            alphaThNch=0.01,
            folderL=paste0(here(simsDir),scenario,"_/"))

param_ABC<-list(totRounds=20000,ResReward=1,VisReward=1,
            ResProbLeav=0,scenario=0, inbr=0,outbr=0,forRat=0.0,
            seed=1, propfullPrint = 0.7,sdPert=0.01,chain_length=10000,
            init=c(0,0,0,0),
            folderL=paste0(here(simsDir),"/",scenario,"_/"))

clustfolderAnd="/hpcfs/home/a.quinones/Cleaner/AbundLvp_/"

clustfolderNeu=paste0("/home/ubuntu/AC/",scenario,"_/")

setwd(paste("./",simsDir,sep=""))


# Arrays with the values of external parameters
rangLeav<-seq(0,0.3,length.out = 20)
rangAbund<-seq(0.1,0.9,length=9)
rangScen<-c(0,1,2,3)
rangAlphNC<-c(0,0.5,1)

# General folder for analysis
check_create.dir(here(simsDir),param = rep(scenario,1),
                 values = c(""))

listfolders<-check_create.dir(here("Simulations",paste0(scenario,"_")),
                              param = rep("Vlp",length(rangLeav)),
                              values = round(rangLeav,2))


# Loop through parameter names and values creating JSONs -----------------------

for (i in 1:length(rangLeav)) {
  for(j in 1:length(rangAbund)){
    param$folderL<-paste0(here("Simulations",paste0(scenario,"_"),listfolders[i]),"/")
    param$folder<-paste0(clustfolderNeu,listfolders[i],"/")
    param$ResProb<-c((1-rangAbund[j])/2)
    param$VisProb<-c((1-rangAbund[j])/2)
    param$VisProbLeav<-rangLeav[i]
    outParam<-toJSON(param,auto_unbox = TRUE,pretty = TRUE)
    fileName<-paste("parameters",j,".json",sep="")
    if(file.exists(paste(param$folderL,fileName,sep = ''))){
      currFile<-fromJSON(paste(param$folderL,fileName,sep = ''))
      if(sum(unlist(currFile)!=unlist(param))>0){
        # warning("You are erasing old files!! n\ Check first!!!",immediate. = TRUE)
        # print("OLD value")
        # print(unlist(currFile)[unlist(currFile)!=unlist(param)])
        # print("NEW value")
        # print(unlist(param)[unlist(currFile)!=unlist(param)])
        # ans<-readline("Want to continue?")
        # if(substr(ans, 1, 1) == "y"){
          write(outParam,paste(param$folderL,fileName,sep = "/"))
          # jobfile(param$folderL,listfolders[i],jobid = j)
        # }
        # else{
        #   jobfile(param$folderL,listfolders[i])
        # }
      }
    }
    else{
      write(outParam,paste(param$folderL,fileName,sep = ""))
      # jobfile(param$folderL,listfolders[i],jobid = j)
    }
    # Uncomment for running simulations directly through R:
    # system(paste(exedir,
    #   gsub("\\","/",paste(param$folder,fileName,sep="\\"),fixed=TRUE)
    #   ,sep = " "))
  }
}
# 
for (k in 1:90) print(y)

# Generate json parameter files for ABC fit-------------------------------------

check_create.dir(here(simsDir),param = rep(scenario,1),
                 values = c(""))

param_ABC$folder<-param_ABC$folderL
outParam<-toJSON(param_ABC,auto_unbox = TRUE,pretty = TRUE)
fileName<-paste("parameters_ABC",".json",sep="")
if(file.exists(paste(param_ABC$folderL,fileName,sep = ''))){
  currFile<-fromJSON(paste(param_ABC$folderL,fileName,sep = ''))
  if(sum(unlist(currFile)!=unlist(param_ABC))>0){
    # warning("You are erasing old files!! n\ Check first!!!",immediate. = TRUE)
    # print("OLD value")
    # print(unlist(currFile)[unlist(currFile)!=unlist(param)])
    # print("NEW value")
    # print(unlist(param)[unlist(currFile)!=unlist(param)])
    # ans<-readline("Want to continue?")
    # if(substr(ans, 1, 1) == "y"){
    write(outParam,paste(param_ABC$folderL,fileName,sep = "/"))
    # jobfile(param$folderL,listfolders[i],jobid = j)
    # }
    # else{
    #   jobfile(param$folderL,listfolders[i])
    # }
  }
}else{
  write(outParam,paste(param_ABC$folderL,fileName,sep = ""))
  # jobfile(param$folderL,listfolders[i],jobid = j)
}
# Uncomment for running simulations directly through R:
# system(paste(exedir,
#   gsub("\\","/",paste(param$folder,fileName,sep="\\"),fixed=TRUE)
#   ,sep = " "))



## Automatically produce job files ---------------------------------------------

jobfile<-function(folder,jobname,timelim="10:00:00",
                  part="short",jobid=""){
  bashafile<-list(line0="#!/bin/bash",
                  jobname="#SBATCH --job-name=",
                  partit="#SBATCH -p ",nodes="#SBATCH -N 1",
                  cpus="#SBATCH --cpus-per-task=1", mem="#SBATCH --mem=2000",
                  time="#SBATCH --time=",
                  mailu="#SBATCH --mail-user=a.quinones@uniandes.edu.co",
                  mailt="#SBATCH --mail-type=END",
                  outp= paste0("#SBATCH -o ","/hpcfs/home/a.quinones/Cleaner/AbundLvp_/",
                               jobname,"/TEST_job.o%j"),
                  gethost="host=`/bin/hostname`",
                  getdate="date=/bin/date",
                  exec=paste0("/hpcfs/home/a.quinones/Cleaner/./cleaner ",
                              "/hpcfs/home/a.quinones/Cleaner/AbundLvp_/",
                              jobname,"/parameters",jobid,".json"),
                  printhost="echo \"Run  at: \"$host",
                  printdate="echo  \"Run  on: \"$date")
  
  bashafile$jobname<-paste0(bashafile$jobname,jobname)
  bashafile$time<-paste0(bashafile$time,timelim)
  bashafile$partit<-paste0(bashafile$partit,part)
  if(file.exists(paste0(folder,"jobfile",jobid,".sh"))){
    unlink(paste0(folder,"jobfile",jobid,".sh"))
  }
  conJobfile<-lapply(bashafile, write, 
                     file=file(paste0(folder,"jobfile",jobid,".sh"),"wb"), append=T)
}


