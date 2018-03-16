

options(stringsAsFactors = FALSE)
dyad.all<-read.csv2("./data/dyad.hans.csv")
dyad.all2<-dyad.all

dyad.all2[which(dyad.all2[,2]=="WestGER"|dyad.all2[,2]=="EastGER"),2]<-"Germany"
dyad.all2[which(dyad.all2[,4]=="WestGER"|dyad.all2[,4]=="EastGER"),4]<-"Germany"

#reduce to country dyad
allec2<-countrydyad(dyad.all2[which(dyad.all[,5]!=0),])
allec2<-allec2[which(allec2[,5]>0),]
colnames(allec2)<-c("Country1","Country2","50 km","100 km","150 km","200 km")
allec2<-allec2[order(allec2[,5]),]

#####plot
#existing dyads by dyad-country

# FIGURE 1
png(file="output/barplot-existing150.png",width=1080,height=1080,pointsize=22)
#pdf(file="output/barplot-existingX.pdf")
par(mai=c(0.7,2,1,0.5),mgp=c(3,0.5,0),mar=c(3,5.5,4.5,2)+0.1)
barall<-data.frame(paste(allec2[,1],allec2[,2],sep="-"),allec2[,5])
colnames(barall)<-c("Dyad",colnames(allec2)[5])
b<-barplot(t(barall[,2]),las=2,yaxt="n",horiz=TRUE,xaxt="n",space=0)
#grid(nx=0,ny=NULL)
axis(side=2,at=b[c(1:length(barall[,1]))],labels=barall[,1],las=1,cex.axis=0.7,tck=-0.01)
axis(side=1,at=c(seq(from=0,to=max(barall[,2]),by=2),max(barall[,2])),labels=c(seq(from=0,to=max(barall[,2]),by=2),max(barall[,2])),
     las=1,cex.axis=0.6,tck=-0.01)
axis(side=3,at=c(seq(from=0,to=max(barall[,2]),by=2),max(barall[,2])),labels=c(seq(from=0,to=max(barall[,2]),by=2),max(barall[,2])),
     las=1,cex.axis=0.6,tck=-0.01)
for (i in 1:max(allec2[,5])+1) {
  i<-i-1
  abline(v=i,col="#D3D3D399",lty=3)
  
}
row.names(barall)<-c(1:length(barall[,1]))
barall2<-barall
for (i in 1:length(barall2[,1])) barall2[i,2]<-ifelse(as.numeric(row.names(barall2)[i])%%2==1,0,barall2[i,2])
b<-barplot(t(barall[,2]),las=2,yaxt="n",horiz=TRUE,xaxt="n",
           space=0,col=gray.colors(3)[1],add=T)
b<-barplot(t(barall2[,2]),las=2,yaxt="n",horiz=TRUE,xaxt="n",
           space=0,col="#a8a8a8",add=T)
dev.off()


# FIGURE 2
##barplot by year all range
yearpl<-data.frame(c(1940:2010),0,0,0,0)

y50<-table(dyad.all[which(dyad.all[,6]==1&dyad.all[,5]!=0),5])
y100<-table(dyad.all[which(dyad.all[,7]==1&dyad.all[,5]!=0),5])
y150<-table(dyad.all[which(dyad.all[,8]==1&dyad.all[,5]!=0),5])
y200<-table(dyad.all[which(dyad.all[,9]==1&dyad.all[,5]!=0),5])
for (i in 1:length(yearpl[,1])) {
  if (length(y50[which(as.numeric(row.names(y50))==yearpl[i,1])]>0)) {
    yearpl[i,2]<-y50[which(as.numeric(row.names(y50))==yearpl[i,1])]
  }
  if (length(y100[which(as.numeric(row.names(y100))==yearpl[i,1])]>0)) {
    yearpl[i,3]<-y100[which(as.numeric(row.names(y100))==yearpl[i,1])]
  }
  if (length(y150[which(as.numeric(row.names(y150))==yearpl[i,1])]>0)) {
    yearpl[i,4]<-y150[which(as.numeric(row.names(y150))==yearpl[i,1])]
  }
  if (length(y200[which(as.numeric(row.names(y200))==yearpl[i,1])]>0)) {
    yearpl[i,5]<-y200[which(as.numeric(row.names(y200))==yearpl[i,1])]
  }
}

colnames(yearpl)<-c("year","50 km","100 km","150 km","200 km")
row.names(yearpl)<-c(1:length(yearpl[,1]))
png(file="output/barplot-year.png",width=1080,height=1080,pointsize=22)
par(mai=c(1.0,1,0.5,0.1),mgp=c(3,1,0),mfrow=c(1,1))
b<-barplot(t(yearpl[,4]),las=2,yaxt="n",horiz=FALSE,xaxt="n",space=0)
grid(nx=0,ny=NULL)
axis(side=1,at=b[c(seq(1,length(yearpl[,1]),1))],labels=FALSE,tck=0.01)
axis(side=1,at=b[c(seq(1,length(yearpl[,1]),1))],labels=FALSE,tck=-0.01)
axis(side=1,at=b[c(seq(1,length(yearpl[,1]),5),2004)],labels=c(seq(min(yearpl[,1]),max(yearpl[,1]),5),2004),las=2,cex.axis=0.7,tck=-0.02)
axis(side=2,at=c(seq(from=0,to=max(yearpl[,4]),by=2),max(yearpl[,4])),labels=c(seq(from=0,to=max(yearpl[,4]),by=2),max(yearpl[,4]))
     ,las=1,cex.axis=0.8,ylim=c(0,35),yaxs="i")
for (i in 1:max(yearpl[,4])) {
  i<-i-1
  abline(h=i,col="#D3D3D399",lty=3)
}
yearpl2<-data.frame(yearpl,0)
c<-0
for (i in 1:length(yearpl2[,1])) {
  if (yearpl2[i,4]!=0)  c<-c+1
  yearpl2[i,6]<-ifelse(yearpl2[i,4]!=0,c,0)
}
for (i in 1:length(yearpl2[,1])) {
  yearpl2[i,4]<-ifelse(yearpl2[i,6]%%2==1,0,yearpl2[i,4])
}
b<-barplot(t(yearpl[,4]),las=2,yaxt="n",horiz=FALSE,xaxt="n",
           ylab="Amount of dyads",space=0,add=T)
b<-barplot(t(yearpl2[,4]),las=2,yaxt="n",horiz=FALSE,xaxt="n",main="Amount of dyads per year",
           space=0,col="#a8a8a8",add=T)
abline(v=b[length(yearpl[which(yearpl[,1]<=1990),1])],col="red",lty=2,lwd=1)
abline(v=b[length(yearpl[which(yearpl[,1]<=1995),1])],col="red",lty=2,lwd=1)
abline(v=b[length(yearpl[which(yearpl[,1]<=2004),1])],col="red",lty=2,lwd=1)
dev.off()
