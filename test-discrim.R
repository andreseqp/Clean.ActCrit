## Analysis of the model with dynamic changes in associability

# Libraries
library(data.table)
library(here)
library(ggplot2)
source(here("loadData.R"))
source(here("aesth_par.R"))

# Files to be loaded
list1<-list.files(here("Simulations","test_"),full.names = TRUE)

# Load raw data
Mckintosh<-do.call(rbind,lapply(list1,fread))

# Transform value data to long format
longMck_Value<-melt(Mckintosh[Age%%100==0],
                    id.vars = c("Age","Training","Gamma"),
                    variable.name = "Client",
                    measure.vars = c("Resident","Visitor"))

# Transform associability (\alpha) data to long format
longMck_alpha<-melt(Mckintosh[Age%%500==0],
                    id.vars = c("Age","Training","Gamma"),
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
        facet_wrap(~Gamma)+
        theme_classic()

ggplot(data=longMck_alpha,aes(x=Age,y=value,colour=as.factor(Client),
                              group=as.factor(Client)))+
        stat_summary(fun.data = function(x){
                x.r<-fivenum(x)
                return(list(ymin=x.r[2],ymax=x.r[4],y=x.r[3]))
        },geom = "pointrange")+
        geom_line(aes(group=interaction(Client,Training)),
                  alpha=0.2,size=0.2,)+
                facet_wrap(~Gamma*Client,nrow = 3)+
        scale_color_manual(values =colorValues)+
        theme_classic()



(listPar<-rep("test",1))
(listVal<-c(""))
(param<-getParam(here("Simulations"),listparam = listPar,values = listVal))

GamVar<-do.call(rbind,lapply(
                getFilelist(here("Simulations","test_"),
                            # listparam = "alpha",
                            # values = 0.05,
                            fullNam = TRUE)$PAA,
                file2timeInter,interV=501))
        
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
        theme_classic()


