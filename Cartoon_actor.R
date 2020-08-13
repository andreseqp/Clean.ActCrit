pref<-rep(NA,1000)
prob<-rep(NA,1000)

pref[1]<-0;prob<-logist(pref[1],0)
for(it in seq(2,1000,by=2)) {
  pref[it]<-pref[it-1]
  pref[it+1]<-pref[it]+runif(1,0,0.1);
  prob[it]<-logist(pref[it],0)
  prob[it+1]<-logist(pref[it+1],0)
}

png("c:/Users/andre/OneDrive - Universidad de los Andes/Neuchatel/images/ActorCart.png",
    width = 640,height = 480)
par(plt=(posPlot()-c(-0.15,-0.08,-0.06,-0.06))*0.9,las=2)
plot(prob,type="l",xlim = c(0,200),ylim=c(0,1),xaxt="n",yaxt="n",xlab="Trial",
     cex.lab=3, lwd=4,col="blue",ylab="",)
axis(2,at=c(0,1),cex.axis=3,line = 0)
mtext(text = "probability",2,line =1.3,cex=3,las=2)
box(lwd=5)
dev.off()