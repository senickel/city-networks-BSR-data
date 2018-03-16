library(DiagrammeR)
library(igraph)
library(sebstoolkit2)
library(tidyverse)
options(stringsAsFactors = FALSE)
dyad.all<-read.csv2("./data/dyad.hans.csv")

dyad.all[,c("c1","c2")]<-dyad.all[,1:4] %>% 
  apply(1,function(x) data.frame(c1=paste(x[1],x[2],sep="@"),
                                 c2=paste(x[3],x[4],sep="@"))) %>% 
  do.call(rbind,.)

hanse<-dyad.all %>% 
  filter(k150==1)

g1<-graph_from_edgelist(hanse %>% 
                select(c1,c2) %>% 
                as.matrix,directed=FALSE)

cl<-clusters(g1)
del<-c(which(cl$membership!=which.max(cl$csize)),which(degree(g1)<4))
g2<-delete.vertices(g1,del)

plot(g2)
del2<-which(clusters(g2)$membership!=which.max(clusters(g2)$csize))
g3<-delete.vertices(g2,del2)

V(g3)$name<-V(g3)$name %>%
  strsplit(.,"@") %>% 
  get.element.from.list(.,1)
plot(g3)
g3_edge<-as_edgelist(g3) %>% 
  data.frame
g3_edge %>% head
g3_edge %>% filter(X1=="Kaliningrad"|X2=="Kaliningrad")
g3_edge %>% filter(X1=="Turku"|X2=="Turku")
hanse %>% 
  filter(City1=="Turku"|City2=="Turku")

node1<-"A B"
node2<-"C D"
edges<-c("A -- {B C D}")
all_nodes<-g3_edge %>% 
  unlist %>% 
  unique
hanse_nodes<-all_nodes[all_nodes%in%
                           c(dyad.all %>%
                               filter(H1==1) %>% 
                               select(City1) %>% 
                               unlist,
                             dyad.all %>%
                               filter(H2==1) %>% 
                               select(City2) %>% 
                               unlist) %>% 
                           unique]
normal_nodes<-all_nodes[!all_nodes%in%hanse_nodes]

help_df<-g3_edge %>% 
  gather(city,number) %>% 
  select(number) %>%
  table %>% 
  sort %>% 
  data.frame(freq=.,name=names(.)) %>% 
  rename(freq=freq.Freq) %>% 
  select(freq,name) %>% 
  arrange(desc(freq))

help_df2<-help_df
edges<-lapply(help_df$name,function(x) {
  h_df<-g3_edge %>% 
    filter(X1==x|X2==x) %>% 
    unlist %>% 
    unique %>% 
    data.frame(r=.) %>% 
    filter(r!=x) %>% 
    filter(r%in%help_df$name)
    unlist
  message(length(h_df))
  help_df<<-help_df %>% 
    filter(name!=x)
  if (h_df %>% nrow==0) return(NULL)
  sapply(h_df,function(y) {
    paste0("\"",x,"\""," -- ","\"",y,"\""," [dir=none]")
  })
  
}) %>% 
  unlist

g1<-paste("","graph boxes_and_circles {",
          "graph [labelloc=\"b\",label=\"\",fontsize=30,overlap = false,layout=dot, rankdir = LR]",
          "",
          "node [shape = circo,fontname = Helvetica,fontsize=40,fillcolor=grey,style=filled]",
          paste(paste0("\"",hanse_nodes %>% gsub("Municipality","",.),"\"")  %>% paste(collapse=" "),"[color=black]"),
          "node [fillcolor=white]",
          paste(paste0("\"",normal_nodes %>% gsub("Municipality","",.),"\"") %>% paste(collapse=" "),"[color=black]"),
          "",
          paste0(edges %>% unlist %>% gsub("Municipality","",.),collapse="\n"),
          "}",collapse="\n",sep="\n")
grViz(g1)


g1<-paste("","graph boxes_and_circles {",
          "graph [layout=twopi,overlap = true]",
          "",
          "node [shape = rectangle,fontname = Helvetica,fontsize=30,fillcolor=grey,style = filled]",
          "\"A. A\" \"B B\"",
          "",
          "node [fillcolor=white]",
          "\"C C\" \"D D\"",
          "",
          "\"A. A\" -- {\"B B\" \"C C\"}",
          "}",collapse="\n",sep="\n")
grViz(g1)

height<-1000
exp_g1<-grViz(g1) %>% 
  export_svg 

width_height<-exp_g1 %>% 
  strsplit("<svg") %>% 
  get.element.from.list(.,2) %>% 
  strsplit("\n") %>% 
  get.element.from.list(.,1) %>% 
  gsub("\"","",.) %>% 
  strsplit(.," ") %>% 
  unlist %>% 
  gsub("pt","",.) %>% 
  gsub("height=","",.) %>% 
  gsub("width=","",.) %>% 
  data.frame(r=.) %>% 
  filter(r!="") %>% 
  unlist %>% 
  as.numeric

p1<-exp_g1 %>% 
  strsplit("<svg") %>% 
  get.element.from.list(.,1) %>% 
  paste0("<svg")

p2<-paste0(" width=\"",round((width_height[1]/width_height[2])*height),"pt\" height=\"",height,"pt\"")

p3<-exp_g1 %>%
  strsplit("<svg") %>%
  get.element.from.list(.,2) %>%
  strsplit("\n viewBox") %>%
  get.element.from.list(.,2) %>%
  paste0("\n viewBox",.)

exp_g2<-paste0(p1,p2,p3)

exp_g2 %>% 
  charToRaw %>% 
  rsvg %>% 
  png::writePNG(paste0("./output/figure_4.png"),
                dpi=c(round((width_height[1]/width_height[2])*height),
                      height))



