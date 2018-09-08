library(tidyverse)
library(lubridate)

t <- read.csv("cricketdata.csv",stringsAsFactors = FALSE)
colnames(t) <- c("","Player","Runs","Mins","BF","4s","6s","SR","Inns","","Opposition","Ground","Start Date","")
data <- t[,-c(1,10,14)]

data %>% mutate(year=year(as.Date(`Start Date`,format="%d %b %Y"))) -> data

#filter out text values of Runs (i.e DNB)
drop <- data$Runs %in% c("TDNB","DNB","absent")
data <- data[!drop,]

#get rid of *s for not out
data$Runs <- gsub("\\*","",data$Runs)


data %>% group_by(`Player`) %>% 
  summarise(TotalRuns=sum(as.numeric(Runs),na.rm=TRUE),Total4s=sum(as.numeric(`4s`),na.rm=TRUE),Total6s=sum(as.numeric(`6s`),na.rm=TRUE)) %>% 
  mutate(TotalBoundaries=Total4s+Total6s) %>%
  mutate(RunsFrom4s=4*Total4s,RunsFrom6s=6*Total6s,RunsFromBoundaries=RunsFrom4s+RunsFrom6s) %>%
  mutate(PropFrom4s=RunsFrom4s/TotalRuns,PropFrom6s=RunsFrom6s/TotalRuns,PropFromBoundaries=RunsFromBoundaries/TotalRuns) %>%
  filter(TotalRuns >= 1000) %>%
  arrange(-PropFromBoundaries) %>% top_n(15) %>%
  mutate(order=(16-(1:n())))%>% 
  select(order,Player,PropFrom4s,PropFrom6s,TotalRuns,PropFromBoundaries) %>%
  mutate(PropFromOther=1-(PropFrom4s+PropFrom6s)) %>%
  gather(key="key",value="value",-c(order,Player,TotalRuns,PropFromBoundaries)) %>%
  mutate(TotalRuns=if_else(key=="PropFrom4s",paste0(scales::comma(TotalRuns)," runs"),NULL)) %>% 
  mutate(PropFromBoundariesLabel=if_else(key=="PropFromOther",scales::percent(PropFromBoundaries),NULL)) %>%
  ggplot(aes(y=value,x=reorder(Player,order))) +
  geom_col(aes(fill=key),position=position_stack(reverse=TRUE)) +
  geom_text(aes(y=1.15,label=TotalRuns),colour="grey50") +
  geom_text(aes(y=PropFromBoundaries,label=PropFromBoundariesLabel),colour="grey50",hjust=0,nudge_y=0.01) +
  coord_flip() +
  scale_y_continuous("",labels=scales::percent,limits=c(0,1.30),breaks=c(0,.20,.40,.60,.80,1.00)) +
  scale_x_discrete("") +
  scale_fill_manual("Source of batsmen's runs",values=c("#B22726","#FF7675","#FFFEAD"), labels=c("Fours","Sixes","Non-boundary")) +
  theme_minimal() +
  theme(text = element_text(colour="grey50"),
        panel.grid = element_blank(),
        title = element_text(hjust=1,size=10)
  )+
  labs(title="Shahid Afridi has the highest proportion of runs from boundaries in Test Cricket",
       subtitle="Filtered to players with at least 1000 test runs, total test runs shown alongside bars",
       caption="Data from ESPNcricinfo - Design by @stevejburr")



ggsave("cricketplot1.png",height=8,width=8)



data %>% group_by(`Player`) %>% 
  summarise(TotalRuns=sum(as.numeric(Runs),na.rm=TRUE),Total4s=sum(as.numeric(`4s`),na.rm=TRUE),Total6s=sum(as.numeric(`6s`),na.rm=TRUE)) %>% 
  mutate(TotalBoundaries=Total4s+Total6s) %>%
  mutate(RunsFrom4s=4*Total4s,RunsFrom6s=6*Total6s,RunsFromBoundaries=RunsFrom4s+RunsFrom6s) %>%
  mutate(PropFrom4s=RunsFrom4s/TotalRuns,PropFrom6s=RunsFrom6s/TotalRuns,PropFromBoundaries=RunsFromBoundaries/TotalRuns) %>%
  arrange(-RunsFromBoundaries) %>% top_n(15,RunsFromBoundaries) %>% 
  mutate(order=16-(1:n())) %>%
  mutate(RunsFromOther=TotalRuns-RunsFromBoundaries) %>%
  group_by(Player) %>%
  gather(key="key",value="value",-c(order,Player)) %>%
  filter(key %in% c("RunsFrom4s","RunsFrom6s","RunsFromOther")) %>%
  arrange(order) %>%
  ggplot(aes(y=value,x=reorder(Player,order))) +
  geom_col(aes(fill=key),position =position_stack(reverse=TRUE)) +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(colour="grey50"),
        panel.grid = element_blank(),
  )+
  labs(title="Batsmen who score the most runs, tends to have the most boundaries",
       subtitle="There are few exceptions - particularly Gayle and Sehwag but also Lara to a lesser degree",
       caption="Data from ESPNcricinfo - Design by @stevejburr") +
  scale_x_discrete("") +
  scale_y_continuous("Test runs",labels=scales::comma ) +
  scale_fill_manual("Source of batsmen's runs",values=c("#B22726","#FF7675","#FFFEAD"), labels=c("Fours","Sixes","Non-boundary"))

ggsave("cricketplot2.png",height=8,width=8)

data %>% group_by(year) %>%
  summarise(TotalRuns=sum(as.numeric(Runs),na.rm=TRUE),Total4s=sum(as.numeric(`4s`),na.rm=TRUE),Total6s=sum(as.numeric(`6s`),na.rm=TRUE)) %>% 
  mutate(TotalBoundaries=Total4s+Total6s) %>%
  mutate(RunsFrom4s=4*Total4s,RunsFrom6s=6*Total6s,RunsFromBoundaries=RunsFrom4s+RunsFrom6s) %>%
  mutate(PropFrom4s=RunsFrom4s/TotalRuns,PropFrom6s=RunsFrom6s/TotalRuns,PropFromBoundaries=RunsFromBoundaries/TotalRuns) %>%
  mutate(PropFromOther=1-PropFromBoundaries) %>%
  arrange(-year) %>%
  gather(key="key",value="value",-year) %>%
  filter(key %in% c("PropFrom4s","PropFrom6s","PropFromOther")) %>%
  filter(year>=1960) %>%
  ggplot(aes(x=year,y=value)) + geom_area(aes(fill=key)) +
  theme_minimal() +
  theme(text = element_text(colour="grey50"),
        panel.grid = element_blank(),
  )+
  scale_x_continuous("",expand=c(0,0)) +
  scale_y_continuous("% of runs",labels=scales::percent) +
  scale_fill_manual("Source of batsmen's runs",values=c("#B22726","#FF7675","#FFFEAD"), labels=c("Fours","Sixes","Non-boundary")) +
  labs(title="The proportion of batsmen's runs coming from boundaries increased from 1960-2010",
       subtitle="Surprisingly, it has been stable or falling since then - Twenty20 hasn't changed tests as much as you might think",
       caption="Data from ESPNcricinfo - Design by @stevejburr")

ggsave("cricketplot3.png",width=8,height=8)
