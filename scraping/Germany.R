land<-"Germany"
url <- paste("http://en.wikipedia.org/wiki/List_of_twin_towns_and_sister_cities_in_",land,sep="")
url
web_page <- readLines(url)
wstart<-grep("<li><a href=\"/wiki/Aachen\"",web_page)
wend<-grep("<span class=\"mw-headline\" id=\"References\">References</span>",web_page)-1
web<-web_page[wstart:wend]
Encoding(web)<-"UTF-8"
cities<-grep("<li><a href=\"",web) #has the number of row where the main cities are in
getdata<-NULL
for (i in 1:length(cities)) getdata[i]<-web[cities[i]]
length(getdata)
head(getdata)
#getting the cities out of the code
citysplit<-strsplit(getdata,">")

citysplit[1:10]
citysplit2<-splitlist(citysplit,3,"<")
citysplit2
citysplit3<-partlist(citysplit2,1)
citysplit3

citydata<-rbind(citysplit3)#,region) ## bind city and region to dataset
citydata # finished with cities

### next step: get the partnercities
startline<-c(cities)
endline<-c(cities[2:length(cities)],length(web))
citydata2<-rbind(citydata,startline,endline)
head(web)
web[1:50]
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
pcountry
pyear<-yearout(pcountry,"</dd>")
pcountry
pcountry2<-strsplit(pcountry,",")
pcountry2<-partlist(pcountry2,2)
pcountry2<-strsplit(pcountry2,"\\(")
pcountry2<-partlist(pcountry2,1)
pcountry2<-strsplit(pcountry2,"</dd>")
pcountry2<-partlist(pcountry2,1)
pcountry3<-removena(pcountry2)
pcountry3<-removeempty(pcountry3)
head(pcountry3)
"Sighi"<U+0219>"oara"
citycountry<-data.frame(pcity,pcountry3,pyear)
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
citycountry2[which(citycountry2[,2]=="Florida"),2]<-"United States"
citycountry2[which(citycountry2[,2]=="Florida"),1]<-paste(citycountry2[which(citycountry2[,2]=="Florida"),1],", Florida")
write.csv(citycountry2,paste("data/PARTNERCITIES-",land,"2.csv",sep=""))
  
          
          
             
          
               
                     

                                
                      

citycountry2
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
citycom[1:600,]
#Germany: drop NAs
row.names(citycom[which(is.na(citycom[,2])),])
citycom<-citycom[-346,]
citycom<-citycom[-657,]
citycom<-citycom[-809,]


write.csv(citycom,paste("data/PARTNERCITIES ",land,".csv",sep=""))
