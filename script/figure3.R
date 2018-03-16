library(DiagrammeR)
library(igraph)
library(tidyverse)
library(svglite)
library(rsvg)
library(DiagrammeRsvg)
library(png)
options(stringsAsFactors = FALSE)
dyad.all<-read.csv2("./data/dyad.hans.csv")

max_weight<-3
height<-1100
year<-2010
subs<-sapply(seq(1980,2010,10),function(year) {
  d_s<-dyad.all %>% 
    filter(Year!=0&k150==1&Year<=year) 
  
  
  year_df<-d_s %>%
    select(Country1,Country2) %>% 
    unlist %>%
    table %>%
    data.frame()
  
  colnames(year_df)<-c("country","Freq")
  
  cl_df<-data.frame(year_df,rel=year_df$Freq/nrow(d_s)) %>% arrange(desc(rel))
  cl<-cl_df %>% 
    dplyr::select(country) %>% unlist %>% as.character
  
  # max_weight<-d_s %>% 
  #   select(Country1,Country2) %>% 
  #   apply(1,paste,collapse="@") %>% 
  #   table %>%
  #   max
  
  
  help_df<-d_s %>% select(Country1,Country2)
  
  edges<-lapply(cl,function(x) {
    message(x)
    if (x%in%(help_df %>% unlist %>% unique)) {
      h_df<-help_df %>% 
        filter(Country1==x|Country2==x) %>% 
        unlist %>% 
        data.frame(r=.) %>% 
        filter(r!=x) %>% 
        table %>% 
        data.frame(Freq=.,name=names(.)) %>% 
        rename(Freq="Freq.Freq") %>% 
        select(Freq,name)
      
      visible<-apply(h_df,1,function(y) {
        paste0(x," -- ",y[2]," [dir=none, penwidth=",round(as.numeric(y[1])/max_weight,3),"]")
      })
      ## add self loops
      self_loop<-help_df %>% 
        filter(Country1==x&Country2==x) %>% nrow
      if (self_loop>0) {
        visible<-c(visible,paste0(x," -- ",x," [dir=none, penwidth=",round(self_loop/max_weight,3),"]"))
      }
      
      ## make new help_df
      help_df<<-help_df %>% 
        filter(Country1!=x&Country2!=x)
      message(nrow(help_df))
      
      
      #
      c_h<-cl_df %>% 
        slice(which(country==x):nrow(cl_df))
      c_h<-c_h %>% 
        filter(rel < (c_h %>% 
                        filter(country==x) %>% 
                        select(rel) %>% 
                        unlist)) %>% 
        filter(!country%in%c(x,h_df$name))
      if (nrow(c_h)==0) return(visible) 
      unvisible<-sapply(c_h$country,function(y) {
        paste0(x," -- ",y," [dir=none, penwidth=0]")
      })
      return(c(visible,unvisible))
    }
    c_h<-cl_df %>% 
      slice(which(country==x):nrow(cl_df))
    c_h<-c_h %>% 
      filter(rel < (c_h %>% 
                      filter(country==x) %>% 
                      select(rel) %>% 
                      unlist)) %>%
      filter(!country%in%c(x))
    if (nrow(c_h)==0) return(NULL) 
    sapply(c_h$country,function(y) {
      paste0(x," -- ",y," [dir=none, penwidth=0]")
    })
  }) %>% unlist
  
  
  g1<-paste("","graph boxes_and_circles {",
        "graph [labelloc=\"b\",label=\"",year,"\",fontsize=30,splines=ortho,overlap = true]",
        "node [shape = box,fontname = Helvetica,fontsize=20]",
        cl %>% paste(collapse=" "),"",
        paste0(edges,collapse="\n"),
        "}",collapse="\n",sep="\n")
  # pdf(paste0("./output/graph_",year,".pdf"))
  # con<-file(paste0("./output/graph_",year,".dot"))
  # writeLines(g1,con)
  # close(con)
  # dev.off()
  
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
  
  # p2<-paste0(" width=\"",round((width_height[1]/width_height[2])*height),"pt\" height=\"",height,"pt\"\n ",
  #            "viewBox=\"0.00 0.00 ",round((width_height[1]/width_height[2])*height)," ",height,"\" ")
  # 
  # p3<-exp_g1 %>% 
  #   strsplit("<svg") %>% 
  #   get.element.from.list(.,2) %>% 
  #   strsplit("xmlns=\"http") %>% 
  #   get.element.from.list(.,2) %>% 
  #   paste0("xmlns=\"http",.)
 
   exp_g2<-paste0(p1,p2,p3)
  
  exp_g2 %>% 
    charToRaw %>% 
    rsvg %>% 
    png::writePNG(paste0("./output/figure_3_",year,".png"),
                  dpi=c(round((width_height[1]/width_height[2])*height),
                        height))
  g1
})  

# list.files("./output")
# grViz(subs[1])
# grViz(subs[4])
# 
# #grViz(subs[1]) %>% export_svg %>% 
# ?export_svg
# 
# strsplit(t1,"width")
# substr(t1,200,400)
# substr(paste0(p1,p2,p3),250,350)
# 
# p1<-t1 %>% 
#   strsplit("<svg") %>% 
#   get.element.from.list(.,1) %>% 
#   paste0("<svg")
# 
# p2<-paste0(" width=\"",round((width_height[1]/width_height[2])*height),"pt\" height=\"",height,"pt\"\n ",
#            "viewBox=\"0.00 0.00 ",round((width_height[1]/width_height[2])*height)," ",height,"\" ")
# 
# #p3<-
# t1 %>% 
#   strsplit("<svg") %>% 
#   get.element.from.list(.,2) %>% 
#   strsplit("xmlns=\"http") 
# %>% length
#   get.element.from.list(.,2) %>% 
#   paste0("xmlns=\"http",.)
# 
# 
# 
# p3
# %>% 
#   strsplit("\n") %>% 
# 
# t1%>% 
#   charToRaw %>% 
#   rsvg
# 
# 
# render_graph(g,output="SVG")
# t1<-paste("","graph boxes_and_circles {","graph [splines=ortho]",
#           "graph [overlap = true, fontsize = 10]",
#           "node [shape = box,fontname = Helvetica]",
#           subs,"}",collapse="\n",sep="\n")
# grViz(t1)
# 
# t1<-paste("","graph boxes_and_circles {","graph [splines=ortho]",
#           # a 'graph' statement
#           "graph [overlap = true, fontsize = 10]",
#           # several 'node' statements
#           "node [shape = box,
#           fontname = Helvetica]",
#           V(g1)$name %>% paste(collapse=" "),"",
#           paste0(edges,collapse="\n"),
#           #paste0("Finland -- {Denmark Denmark Norway}")
#           "}",collapse="\n",sep="\n")

#",V(g1)$name %>% paste(collapse=" "),"
# grViz(t1)
