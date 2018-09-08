library(XML)
library(tidyverse)
#code taken from:
#https://rpubs.com/dgolicher/cricket_download

i<-1
url<-paste("http://stats.espncricinfo.com/ci/engine/stats/index.html?class=1;page=",i,";template=results;type=batting;view=innings",sep="")
tables <-readHTMLTable(url, stringsAsFactors = F)
t <- tables$"Innings by innings list"


for (i in 2:1856)
{
  print(paste0(i," of 1856"))
  print(paste0(round(100*i/1856),"%"))
  url<-paste("http://stats.espncricinfo.com/ci/engine/stats/index.html?class=1;page=",i,";template=results;type=batting;view=innings",sep="")
  try(tables <-readHTMLTable(url, stringsAsFactors = F))
  try(tt <- tables$"Innings by innings list")
  try(t<-rbind(t,tt))
}

write.csv(t,"cricketdata.csv")

