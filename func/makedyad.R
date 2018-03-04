makedyad<-function(c1) {
  c2<-data.frame(0,0)
  for (i in 1:length(c1)) {
    for (j in 1:length(c1)) {
      c3<-data.frame(c1[i],c1[j])
      colnames(c3)<-colnames(c2)
      c2<-rbind(c2,c3)
    }
  }
  c2<-c2[-1,]
  c4<-data.frame(0,0)
  for (i in 1:length(c2[,1])) {
    j<-1
    k<-0
    while(j<=length(c4[,1])) {
      if ((c2[i,1]==c4[j,1]&&c2[i,2]==c4[j,2])|(c2[i,1]==c4[j,2]&&c2[i,2]==c4[j,1])) {
        break
      }
      j<-j+1
      if (j>length(c4[,1])) {
        k<-1
        break
      }
    }
    if (k==1) {
      var<-data.frame(c2[i,1],c2[i,2])
      colnames(c4)<-colnames(var)
      c4<-rbind(c4,var)
    }
  }
  c4<-c4[-1,]
  return(c4)
}