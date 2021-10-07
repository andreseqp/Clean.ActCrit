# -------------- generate json files with parameters------------------------------------------ #

library("jsonlite")
library("here")
source(here("loadData.R"))

simsDir<-"Simulations"

exedir<-'/./ActCrit.exe'

fileName<-"parameters.json"

scenario<-"ABCclean_gam_Nrew_unif_sca30"

# scenario<-"predictions"

# For predictions on the observed values ---------------------------------------

scenario<-"ABCclean_gam_Nrew_exp_sca30"

param_pred<-list(totRounds=10000,ResReward=1,VisReward=1,
            ResProbLeav=0,scenario=0, inbr=0,outbr=0,forRat=0.0,
            seed=1, propfullPrint = 0.7,sdPert=c(0.05,0.05,0.1,0.05),
            chain_length=0,
            init=c(0.05,0.05,0,0),# alphaA,AlphaC, Gamma, NegRew
            pertScen = 1, 	# enum perturnScen {all,  bothFut, justGam, justNegRew};
            MCMC =0, data="clean",nRep=30,
            dataFile = here("Data","data_ABC_cleaner_30.txt"),
            folderL=paste0(here(simsDir),"/",scenario,"_/"))




param_pred$folder<-param_pred$folderL
param_pred$init[c(3,4)]<-sumMCMClist$statistics[,1]
fileName<-paste("parameters_pred",param_pred$seed,".json",sep="")
outParam.pred<-toJSON(param_pred,auto_unbox = TRUE,pretty = TRUE)
if(file.exists(paste(param_pred$folderL,fileName,sep = ''))){
  currFile<-fromJSON(paste(param_pred$folderL,fileName,sep = ''))
  if(sum(unlist(currFile)!=unlist(param_pred))>0){
    # warning("You are erasing old files!! n\ Check first!!!",immediate. = TRUE)
    # print("OLD value")
    # print(unlist(currFile)[unlist(currFile)!=unlist(param)])
    # print("NEW value")
    # print(unlist(param)[unlist(currFile)!=unlist(param)])
    # ans<-readline("Want to continue?")
    # if(substr(ans, 1, 1) == "y"){
    write(outParam.pred,paste(param_pred$folderL,fileName,sep = "/"))
    # jobfile(param$folderL,listfolders[i],jobid = j)
    # }
    # else{
    #   jobfile(param$folderL,listfolders[i])
    # }
  }
}else{
  write(outParam.pred,paste(param_pred$folderL,fileName,sep = ""))
  # jobfile(param$folderL,listfolders[i],jobid = j)
}


# Baseline parameter values
## for the countour plots ------------------------------------------------------
param<-list(totRounds=10000,ResReward=1,VisReward=1,
            ResProb=c(0.2),
            VisProb=c(0.2),
            ResProbLeav=0,VisProbLeav=1,negativeRew=-sumMCMClist$statistics[,1][2],
            scenario=0,
            inbr=0,outbr=0,trainingRep=10,forRat=0.0,
            alphaT=0.05,printGen=1,seed=1, gammaRange=I(c(sumMCMClist$statistics[,1][1])),
            netaRange=I(c(1)),alphaThRange=I(c(0.05)),numlearn=1,
            propfullPrint = 0.7,
            alphaThNch=0.05,
            folderL=paste0(here(simsDir),scenario,"_/"))

clustfolderAnd="/hpcfs/home/a.quinones/Cleaner/AbundLvp_/"

clustfolderNeu=paste0("/home/ubuntu/AC/",scenario,"_/")

folderSims<-paste0("e:/NeuchSims/AC/",paste0(scenario,"_"))

setwd(paste("./",simsDir,sep=""))

fieldData<-fread(here("Data","data_ABC_cleaner_30.txt"))

range(fieldData$rel.abund.cleaners)
range(fieldData$prob.Vis.Leav)

# Arrays with the values of external parameters
rangLeav<-seq(0.02,0.4,length.out = 10)
rangAbund<-seq(0.01,0.7,length=10)
rangScen<-c(0)
rangAlphNC<-c(0,0.5,1)

# General folder for analysis
check_create.dir(here(simsDir),param = rep(scenario,1),
                 values = c(""))

listfolders<-check_create.dir(here("Simulations",paste0(scenario,"_")),
                              param = rep("Vlp",length(rangLeav)),
                              values = round(rangLeav,2))

listfolders<-check_create.dir(folderSims,
                              param = rep("Vlp",length(rangLeav)),
                              values = round(rangLeav,2))


# Loop through parameter names and values creating JSONs -----------------------

for (i in 1:length(rangLeav)) {
  for(j in 1:length(rangAbund)){
    param$folderL<-paste0(here("Simulations",paste0(scenario,"_"),listfolders[i]),"/")
    param$folder<-paste0(folderSims,"/",listfolders[i],"/") #param$folderL
    #paste0(clustfolderNeu,listfolders[i],"/")
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

scenario<-"ABCclean_gam_Nrew_exp_sca30"

# for MCMC
param_ABC<-list(totRounds=10000,ResReward=1,VisReward=1,
                ResProbLeav=0,scenario=1, inbr=0,outbr=0,forRat=0.0,
                seed=1, propfullPrint = 0.7,sdPert=c(0.05,0.05,0.1,0.05),
                chain_length=100000,
                init=c(0.05,0.05,0,0),# alphaA,AlphaC, Gamma, NegRew
                pertScen = 1, 	# enum perturnScen {all,  bothFut, justGam, justNegRew};
                MCMC =1, data="clean",nRep=1,
                dataFile = here("Data","data_ABC_cleaner_30.txt"),
                ##here("Data","data_ABC_site_20.txt")
                folderL=paste0(here(simsDir),"/",scenario,"_/"))

check_create.dir(here(simsDir),param = rep(scenario,1),
                 values = c(""))


for(seed in 1:3){
  param_ABC$folder<-param_ABC$folderL
  param_ABC$init<-c(0.05,0.05,runif(1,max = 0.6),runif(1,max = 0.5))##
  param_ABC$seed <- seed
  fileName<-paste("parameters_ABC_",seed,".json",sep="")
  outParam<-toJSON(param_ABC,auto_unbox = TRUE,pretty = TRUE)
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


