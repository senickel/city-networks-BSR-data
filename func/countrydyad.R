#country dyad with amount of twinnings per interval
countrydyad<-function(data) {
  countries<-row.names(table(c(row.names(table(data[,2])),row.names(table(data[,4])))))
  var<-makedyad(countries)
  var<-data.frame(var,0,0,0,0)
  for (i in 1:length(data[,1])) {
    for (j in 1:4) {
      var[which((data[i,2]==var[,1]&data[i,4]==var[,2])|(data[i,4]==var[,1]&data[i,2]==var[,2])),j+2]<-
        var[which((data[i,2]==var[,1]&data[i,4]==var[,2])|(data[i,4]==var[,1]&data[i,2]==var[,2])),j+2]+as.numeric(data[i,5+j])
    }
  }
  var<-var[-which(var[,6]==0),]
  return(var)
}