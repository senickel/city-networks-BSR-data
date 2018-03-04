library(XML)



url <- "http://en.wikipedia.org/wiki/List_of_twin_towns_and_sister_cities_in_Russia#Cities_starting_with_A"
web_page <- readLines(url)
wstart<-grep("Alexandrov",web_page)
wend<-grep("<span class=\"mw-headline\" id=\"References\">References</span>",web_page)-1
web<-web_page[wstart:wend]
Encoding(web)<-"UTF-8"
cities<-grep("<li><a",web) #has the number of row where the main cities are in
getdata<-NULL
for (i in 1:length(cities)) getdata[i]<-web[cities[i]]

#getting the cities out of the code
citysplit<-strsplit(getdata,">")
citysplit2<-NULL
regionmark<-NULL
citysplit3<-NULL
regionsplit<-NULL
region<-NULL
for (i in 1:length(citysplit)) {
  citysplit2[i]<-strsplit(citysplit[[i]][3],"<")
  if(length(citysplit[[i]])>4) {
    ifelse(length(grep("#",citysplit[[i]][5]))==0,regionmark[i]<-1,regionmark[i]<-0) #mark if there is an extra region which identifies the city further
  }
  else { regionmark[i]<-0}
}
regionmark
for (i in 1:length(regionmark)) {
  if (regionmark[i]==1) regionsplit[i]<-strsplit(citysplit[[i]][5],"<")
  else { regionsplit[i]<-0}
}  
for (i in 1:length(regionsplit)) region[i]<-regionsplit[[i]][1]
for (i in 1:length(citysplit2)) citysplit3[i]<-citysplit2[[i]][1]
citydata<-rbind(citysplit3,region) ## bind city and region to dataset
citydata # finished with cities

### next step: get the partnercities
startline<-c(cities)
endline<-c(cities[2:length(cities)],length(web))
citydata2<-rbind(citydata,startline,endline)
head(web)
partnercities<-grep("<span class=\"flagicon\"",web) #has the number of row where the main cities are in

partnersplit<-strsplit(web[partnercities],"\">")
partnersplit2<-NULL
for (i in 1:length(partnersplit)) partnersplit2[i]<-partnersplit[[i]][4]
partnersplit3<-strsplit(partnersplit2,"</a>")
partnersplit3
#getting partnercity and country
pcity<-NULL
pcountry<-NULL
for (i in 1:length(partnersplit3)) {
  pcity[i]<-partnersplit3[[i]][1]
  pcountry[i]<-partnersplit3[[i]][2]
}
pcity[793]
pcountry2<-sub(", ","",pcountry)
pcountry2<-sub("</dd>","",pcountry2)
pcountry2<-strsplit(pcountry2,"title=\"")
partnersplit3[793]
pcountry2
pcountry3<-NULL
for (i in 1:length(pcountry2)) {
  if (length(pcountry2[[i]])==1) pcountry3[i] <- pcountry2[[i]][1]
  if (length(pcountry2[[i]])==2) pcountry3[i] <- pcountry2[[i]][2]
}
citycountry<-data.frame(pcity,pcountry3)
citycountry[30,]
#export for further editing
write.csv(citycountry,"data/PARTNERCITIES-RUSSIA2.csv")
##edited - load again
citycountry2<-read.csv("data/PARTNERCITIES-RUSSIA.csv",colClasses = "character")
citycountry2<-citycountry2[,2:3]

###get the year in an extra column
#get rid of the parantheses and since
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
head(citycountry3)
length(citycountry3[,1])
citycom[,2:4]<-citycountry3
write.csv(citycom,"data/Partnercities Russia.csv")




citycom
head(citycom)
head(citydata)
citydata
cities
partnercities
merge.cities<-cbind(cities)
merge.cities
length(citydata[1,])













length(partnercities)
sum(gaw[,2])
gaw[which(gaw[,2]==0),]
gaw[1:23,]
partnercities[60]
partnercities[61]
partnercities[62]
cities2[21]
cities2[22]
cities2[23]
partnercities
cities2[21]>partnercities[57]
cities2[21]>partnercities[58]
cities2[21]>partnercities[59]
cities2[21]>partnercities[60]
cities2[21]>partnercities[61]
cities2[22]>partnercities[61]
cities2[23]>partnercities[61]
citycountry3[partnercities[61],]
citydata[22]
citydata
partnercities[59]
cities2[21]
cities2[22]
web[168]
web[164:176]
partnercities[58:65]
cities2[length(cities2)]>partnercities[length(partnercities)]
gaw
warnings()
k
gaw[1:5]
gaw2<-gaw
gaw[1:50]
citydata[,100]
citydata
cities2
cities2[101]-cities2[100]
length(partnercities)
length(cities2)
sum(is.na(partnercities))
sum(is.na(cities2))
sum(gaw)
cities
web[923]

web[cities2[21]]
is.numeric(partnercities)
is.numeric(cities2)
partnercities[1]<cities2[1]
partnercities[2]<cities[2]
partnercities[length(partnercities)]
cities[length(cities)]
web[2650]
wend
is.numeric(partnercities)
is.numeric(cities)
#699
tttest
strsplit(citycountry2[369,2]," ")

citycountry[1:50,]
pcountry[162]
pcountry3

length(pcountry2[[883]])
pcountry2[[883]][2]
pcountry[883]
length(pcountry2)
pcountry[1:10]


?gsub
partnersplit2[partnersplit3]
length(partnercities)

web[1:10]

citydata2
cities[length(cities)]
cities
web[2647:length(web)]
length(web)
web[1:50]
head(cities)

length(regionmark)




#test for one entry
strsplit(getdata[1],">")
test<-strsplit(getdata[1],">")
test[[1]][3]
test
test2<-c(strsplit(test[[1]][3],"<"),strsplit(test[[1]][5],"<"))
test
citysplit3
regionsplit
region
citysplit[[160]]
pmatch(citysplit[[160]][5],"<a href=\"#cite_note")
citysplit3
 length(grep("#",citysplit[[i]][5]))
  length(grep("#",citysplit[[1]][5]))==0
?interger(0)

length(citysplit[[20]])>4
  citysplit[[1]][5]
length(test[[1]])
length(test[[190]])


i<-1
strsplit(test[[i]][3],"<")
test
test2
length(test)
for (i in 1:length)

test[[1]][3]
test
test2<-c(strsplit(test[[1]][3],"<"),strsplit(test[[1]][5],"<"))
test2



head(getdata)
?Encoding
web[1]
cities
web2[183]