## Analysis of the model with dynamic changes in associability

# Libraries
library(data.table)
library(here)
library(ggplot2)
source(here("loadData.R"))
source(here("aesth_par.R"))

# Files to be loaded
scenario<-"equalAttAC1_"

parFocal<-"AttMech"

listPar<-c("AttMech","alph")
listVal<-c(0,0.01)
(list1<-getFilelist(here("Simulations",scenario),
            listparam = listPar,
            values = listVal,
            fullNam = TRUE)$PAA)


# list1<-greploop(params,mylist = list1)

# Load raw data
Mckintosh<-do.call(rbind,lapply(list1,loadFilewithPar,parOFint="AttMech"))

# Transform value data to long format
longMck_Value<-melt(Mckintosh[Age%%100==0],
                    id.vars = c("Age","Training","Gamma","AttMech"),
                    variable.name = "Client",
                    measure.vars = c("Resident","Visitor"))

# Transform associability (\alpha) data to long format
longMck_alpha<-melt(Mckintosh[Age%%500==0],
                    id.vars = c("Age","Training","Gamma","AttMech"),
                    variable.name = "Client",
                    measure.vars = c("alphaRes","alphaVis"))


ggplot(data=longMck_Value,aes(x=Age,y=value,colour=as.factor(Client),
                             group=as.factor(Client)))+
        stat_summary(fun.data = function(x){
                x.r<-fivenum(x)
                return(list(ymin=x.r[2],ymax=x.r[4],y=x.r[3]))
        },geom = "pointrange")+
        geom_line(aes(group=interaction(Client,Training)),
                  alpha=0.2,size=0.2)+
        scale_color_manual(values =colorValues)+
        facet_wrap(~Gamma*AttMech,scales = "free_y")+
        theme_classic()

ggplot(data=longMck_alpha,aes(x=Age,y=value,colour=as.factor(Client),
                              group=as.factor(Client)))+
        stat_summary(fun.data = function(x){
                x.r<-fivenum(x)
                return(list(ymin=x.r[2],ymax=x.r[4],y=x.r[3]))
        },geom = "pointrange")+
        geom_line(aes(group=interaction(Client,Training)),
                  alpha=0.2,size=0.2,)+
                facet_wrap(~Gamma*Client*AttMech,nrow = 3,scales = "free_y")+
        scale_color_manual(values =colorValues)+
        theme_classic()


GamVar<-do.call(rbind,lapply(
                getFilelist(here("Simulations",scenario),
                            listparam = listPar,
                            values = listVal,
                            fullNam = TRUE)$PAA,
                file2timeInter,interV=501,parOFint="AttMech"))
        
ggplot(data = GamVar,aes(x=Interv,y=Prob.RV.V,group=Gamma,
                         color=as.factor(Gamma)))+
        stat_summary(fun.data = function(x){
                x.r<-fivenum(x)
                return(list(ymin=x.r[2],ymax=x.r[4],y=x.r[3]))
        },geom ="pointrange",position = position_dodge(0.9))+
        geom_hline(yintercept = 0.5,color="red")+
        geom_line(aes(group=interaction(Gamma,Training),
                  color=as.factor(Gamma)),alpha=0.2,size=0.2,
                  position = position_dodge(0.9))+
        scale_color_manual(values =colorValues, name = expression(gamma))+
        xlab("Time")+ylab("Probability of V over R")+
        facet_grid(~AttMech*Alpha)+
        theme_classic()


