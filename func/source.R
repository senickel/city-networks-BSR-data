files<-list.files("func",full.names = TRUE)
files<-files[!grepl("source.R",files)]
sapply(files,source)
