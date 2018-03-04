source("func/source.R")
land<-"Denmark"
url <- paste("http://en.wikipedia.org/wiki/List_of_twin_towns_and_sister_cities_in_",land,sep="")
web_page <- readLines(url)
wstart<-grep("<h2><span class=\"mw-headline\" id=\"Albertslund\"",web_page)
wend<-grep("<span class=\"mw-headline\" id=\"References\">References</span>",web_page)-1
web<-web_page[wstart:wend]
Encoding(web)<-"UTF-8"
cities<-grep("<h2><span class=\"mw-headline\" id=\"",web) #has the number of row where the main cities are in
getdata<-NULL
for (i in 1:length(cities)) getdata[i]<-web[cities[i]]
length(getdata)
head(getdata)
#getting the cities out of the code
citysplit<-strsplit(getdata,">")

citysplit[1:10]
citysplit2<-splitlist(citysplit,4,"<")
citysplit3<-partlist(citysplit2,1)
citysplit3[which(citysplit3=="")]<-"Horsens" #DENMARK

citydata<-rbind(citysplit3)#,region) ## bind city and region to dataset
citydata # finished with cities

### next step: get the partnercities
startline<-c(cities)
endline<-c(cities[2:length(cities)],length(web))
citydata2<-rbind(citydata,startline,endline)
head(web)
partnercities<-grep("<span class=\"flagicon\"",web) #has the number of row where the main cities are in

partnersplit<-strsplit(web[partnercities],"\">")
partnersplit2<-NULL
head(partnersplit)
for (i in 1:length(partnersplit)) partnersplit2[i]<-paste(partnersplit[[i]][4],partnersplit[[i]][5])
head(partnersplit2)
partnersplit3<-strsplit(partnersplit2,"</a>")


#getting partnercity and country
pcity<-partlist(partnersplit3,1)
pcountry<-partlist(partnersplit3,2)
pyear<-partlist(partnersplit3,3)
head(pcity)
head(pcountry)
head(pyear)
pcountry2<-sub(", <a href=\"/wiki/","",pcountry)
pcountry2
pcountry2<-strsplit(pcountry2,"title=\"")
pcountry3<-splitlist(pcountry2,1,"\"")
pcountry3<-partlist(pcountry3,1)
pcountry3<-gsub("_"," ",pcountry3)
pcountry3
pcountry5<-pcountry3
pyear

pyear2<-gsub("/","",pyear)
pyear2<-gsub("<i>","",pyear2)
pyear2<-gsub("<small>","",pyear2)
pyear2<-gsub("<li>","",pyear2)
pyear2<-gsub("<sup id=\"cite_ref-","",pyear2)
pyear3<-strsplit(pyear2,"\\(")
pyear3
pyear4<-partlist(pyear3,2)
pyear4<-removena(pyear4)

pyear5<-strsplit(pyear4,"\\)")

pyear6<-partlist(pyear5,1)
pyear6
pyear7<-sub("since ","",pyear6)
pyear7<-sub("since","",pyear7)
pyear7
citycountry<-data.frame(pcity,pcountry5,pyear7)
citycountry
#export for further editing

write.csv(citycountry,paste("data/PARTNERCITIES-",land,".csv",sep=""))
##edited - load again
citycountry2<-read.csv(paste("data/PARTNERCITIES-",land,".csv",sep=""),colClasses = "character")
citycountry2<-citycountry2[,2:4]
citycountry3<-citycountry2

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
gaw[1:10,2]
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
#DENMARK: drop 88 was a failed scarp
citycom<-rbind(citycom[1:87,],citycom[89:length(citycom[,1]),])

write.csv(citycom,paste("data/Partnercities ",land,".csv",sep=""))
