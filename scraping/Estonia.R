land<-"Estonia"
url <- paste("http://en.wikipedia.org/wiki/List_of_twin_towns_and_sister_cities_in_",land,sep="")
url
web_page <- readLines(url)
wstart<-grep("<h3><span class=\"mw-headline\" id=\"Kehra",web_page)

wend<-grep("<span class=\"mw-headline\" id=\"References\">References</span>",web_page)-1
web<-web_page[wstart:wend]
Encoding(web)<-"UTF-8"
cities<-grep("<h3><span class=\"mw-headline\" id",web) #has the number of row where the main cities are in
getdata<-NULL
for (i in 1:length(cities)) getdata[i]<-web[cities[i]]
length(getdata)
head(getdata)
#getting the cities out of the code
citysplit<-strsplit(getdata,"<span")

citysplit[1:10]
citysplit2<-splitlist(citysplit,2,"\">")
citysplit2
length(citysplit2[[9]])
citysplit3<-NULL
for (i in 1:length(citysplit2)) {
  leng<-length(citysplit2[[i]])
  citysplit3[i]<-citysplit2[[i]][leng]
}
citysplit3
citysplit3<-sub("</a>","",citysplit3)
citysplit3<-sub("</span>","",citysplit3)

citysplit3
citydata<-rbind(citysplit3)#,region) ## bind city and region to dataset

### next step: get the partnercities
startline<-c(cities)
endline<-c(cities[2:length(cities)],length(web))
citydata2<-rbind(citydata,startline,endline)
head(web)
web[1:50]
partnercities<-grep("<span class=\"flagicon\"",web) #has the number of row where the main cities are in

partnersplit<-strsplit(web[partnercities],"\">")
partnersplit2<-NULL
partnersplit
pcity<-partlist(partnersplit,4)
pcountry<-partlist(partnersplit,5)
pcity2<-strsplit(pcity,"</a>")
pcity2<-partlist(pcity2,1)
pcity2
pyear<-yearout(pcountry,"</i>")
pyear

pcountry2<-strsplit(pcountry,"</a>")
pcountry2<-partlist(pcountry2,1)
pcountry2


citycountry<-data.frame(pcity2,pcountry2,pyear)
head(citycountry)
table(citycountry[,2])
citycountry[which(citycountry[,2]==0),]
#export for further editing
write.csv(citycountry,paste("data/PARTNERCITIES-",land,".csv",sep=""))
##edited - load again
citycountry2<-read.csv(paste("data/PARTNERCITIES-",land,".csv",sep=""),colClasses = "character")
citycountry2<-citycountry2[,2:4]
tabnames<-row.names(table(citycountry2[,2]))
tabnames

citycountry2[which(citycountry2[,2]=="the Netherlands"),2]<-"The Netherlands"



write.csv(citycountry2,paste("data/PARTNERCITIES-",land,"2.csv",sep=""))
citycountry3<-read.csv(paste("data/PARTNERCITIES-",land,"2.csv",sep=""),colClasses = "character")
citycountry3<-citycountry3[,2:4]


#### now merge the partnercities under the cities
cities2<-c(cities[2:length(cities)],wend)
gaw<-matrix(0,length(cities2),2)
gaw[,1]<-cities2

k<-1
for (i in 1:length(cities2)) {
  while(gaw[i,1]>partnercities[k]) {
    gaw[i,2]<-1+gaw[i,2]
    k<-k+1
  }
}
gaw[1:10]
head(gaw)
head(citydata)
length(citydata[1,])
length(gaw[,1])
citycom<-matrix(0,length(partnercities),4)
i<-1
for (j in 1:length(gaw[,1])) {
  k<-1
  while(gaw[j,2]>=k) {
    citycom[i,1]<-citydata[1,j]
    
    k<-k+1
    i<-i+1
  }
}
head(citycom)
head(citycountry3)
head(citycom)

length(citycountry3[,1])
length(citycom[,1])
citycom<-cbind(citycom[,1],citycountry3)
head(citycom)
citycom


write.csv(citycom,paste("data/PARTNERCITIES ",land,".csv",sep=""))
