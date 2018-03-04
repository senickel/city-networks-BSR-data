land<-"Norway"
url <- paste("http://en.wikipedia.org/wiki/List_of_twin_towns_and_sister_cities_in_",land,sep="")
web_page <- readLines(url)
wstart<-grep("<h2><span class=\"mw-headline\" id=\".C3.85l\"><a href=\"/wiki/%C3%85l",web_page)
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
citysplit2<-NULL
regionmark<-NULL
citysplit3<-NULL
regionsplit<-NULL
region<-NULL
citysplit[1:10]
for (i in 1:length(citysplit)) {
  citysplit2[i]<-strsplit(citysplit[[i]][4],"<")
  #if(length(citysplit[[i]])>4) {
  # ifelse(length(grep("#",citysplit[[i]][5]))==0,regionmark[i]<-1,regionmark[i]<-0) #mark if there is an extra region which identifies the city further
  # }
  # else { regionmark[i]<-0}
}
#regionmark
#for (i in 1:length(regionmark)) {
# if (regionmark[i]==1) regionsplit[i]<-strsplit(citysplit[[i]][5],"<")
#  else { regionsplit[i]<-0}
#}  
#for (i in 1:length(regionsplit)) region[i]<-regionsplit[[i]][1]
citysplit2

for (i in 1:length(citysplit2)) citysplit3[i]<-citysplit2[[i]][1]
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
partnersplit3

#getting partnercity and country
pcity<-NULL
pcountry<-NULL
pyear<-NULL #Norway
for (i in 1:length(partnersplit3)) {
  pcity[i]<-partnersplit3[[i]][1]
  pcountry[i]<-partnersplit3[[i]][2]
  ifelse(length(partnersplit3[[i]])>2,pyear[i]<-partnersplit3[[i]][3],pyear[i]<-0)#Norway
}
pcity
head(pcity)
head(pcountry)
pyear#Norway
pcountry2<-sub(", ","",pcountry)
pcountry2<-sub("<a href=\"/wiki/","",pcountry2)
pcountry2<-strsplit(pcountry2,"title=\"")
pcountry3<-NULL #Norway
for (i in 1:length(pcountry2)) pcountry3[i]<-sub("\"","",pcountry2[[i]][1])#Norway
pcountry4<-sub(" ","",pcountry3)#Norway#Norway
pcountry4<-sub("_"," ",pcountry4)
pcountry4#Norway
#partnersplit3[793]
# NOT FOR NORWAY
pcountry2
pcountry3<-NULL
for (i in 1:length(pcountry2)) {
  if (length(pcountry2[[i]])==1) pcountry3[i] <- pcountry2[[i]][1]
  if (length(pcountry2[[i]])==2) pcountry3[i] <- pcountry2[[i]][2]
}
pcountry3

pcountry4<-sub("<small><i>","",pcountry3)
pcountry4<-sub("</i></small>","",pcountry4)
pcountry4<-sub("</i>","",pcountry4)
pcountry4<-sub("<i>","",pcountry4)
pcountry4<-sub("</b>","",pcountry4)
pcountry4<-strsplit(pcountry4,"<sup")
# END NOT FOR NORWAY
##just for Norway
pcountry5<-pcountry4
###End just for Norway
# NOT FOR NORWAY
pcountry4
pcountry5<-NULL
for (i in 1:length(pcountry4)) pcountry5[i]<-pcountry4[[i]][1]
pcountry5


citycountry<-data.frame(pcity,pcountry5)
citycountry[30,]
#END NOT FOR NOWAY
#JUST FOR NORWAY
pyear2<-gsub("/","",pyear)
pyear2<-gsub("<i>","",pyear2)
pyear2<-gsub("<small>","",pyear2)
pyear2<-gsub("<sup id=\"cite_ref-","",pyear2)
pyear3<-strsplit(pyear2,"\\(")
pyear3
pyear4<-NULL
for (i in 1:length(pyear3)) ifelse(length(pyear3[[i]])>1,pyear4[i]<-pyear3[[i]][2],pyear4[i]<-0)
pyear4
pyear5<-strsplit(pyear4,"\\)")

pyear6<-partlist(pyear5,1)
pyear7<-sub("since ","",pyear6)
pyear7
citycountry<-data.frame(pcity,pcountry5,pyear7)
citycountry
#export for further editing

write.csv(citycountry,paste("data/PARTNERCITIES-",land,".csv",sep=""))
##edited - load again
citycountry2<-read.csv(paste("data/PARTNERCITIES-",land,".csv",sep=""),colClasses = "character")
citycountry2<-citycountry2[,2:4] #Norway

###get the year in an extra column
#get rid of the parantheses and since
#NOT FOR NORWAY
pcountry4<-citycountry2[,2]
pcountry5<-strsplit(pcountry4,"\\(")
pcountry7<-strsplit(pcountry4,"since") #split "since"
pcountry7
pcountry6<-matrix(0,length(pcountry5),3)
for (i in 1:length(pcountry5)) {
  pcountry6[i,1]<-pcountry5[[i]][1]
  pcountry6[i,2]<-sub("\\)","",pcountry5[[i]][2])
  pcountry6[i,3]<-sub("since","",pcountry7[[i]][2])
}
pcountry6
citycountry3<-cbind(citycountry2[,1],pcountry6)
citycountry3[1:10,]

#remove NAs
for (i in 1:length(citycountry3[,3])) {
  if (is.na(citycountry3[i,3])) citycountry3[i,3]<-0
  if (is.na(citycountry3[i,4])) citycountry3[i,4]<-0
}  
citycountry3
for (i in 1:length(citycountry3[,1])) if (citycountry3[i,4]!="0") citycountry3[i,3]<-citycountry3[i,4]
citycountry3<-citycountry3[,1:3]
for (i in 1:length(citycountry3[,1])) citycountry3[i,3]<-sub("\\)","",citycountry3[i,3])
citycountry3
##END NOT FOR NORWAY
#JUST FOR NORWAY
citycountry3<-citycountry2
#END JUST FOR NORWAY
#### now merge the partnercities under the russian cities
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

write.csv(citycom,paste("data/PARTNERCITIES ",land,".csv",sep=""))

