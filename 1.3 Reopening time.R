#Find the reopening date
library(dplyr)

setwd("/Code and data/Data")

name <- read.csv('other data/Final name list.csv') %>% 
  mutate(ID=as.character(ID))
#The reopening date from the median
open_media <- read.csv('Reopening/Opentime from media_NEW.csv'
                       , header=TRUE, sep=',') %>% 
  mutate(Country=as.character(Country),ID=as.character(ID),Date=as.Date(Open_time))
#Estimating reopening date
open_est <- read.csv('Policy/syl-time_series_country_policy_reopen_time_estimate.csv'
                     , header=TRUE, sep=',') %>% 
  mutate(Country=as.character(Country),ID=as.character(ID),Date=as.Date(Reopen_date))

#Choose the reopen date
Reopen <- tibble(Country=NA,ID=NA,Open_media=as.Date("2020-01-22"),Open_est=as.Date("2020-01-22"))
for(i in name$ID){
  print(i)
  r = tibble(Country=NA,ID=NA,Open_media=as.Date("2020-01-22"),Open_est=as.Date("2020-01-22"))
  m <- open_media %>% filter(ID==i)
  e <- open_est %>% filter(ID==i)
  if(is.na(m$Date)){
    r <- r %>% mutate(Country=m$Country,ID=i,Open_media=m$Date,Open_est=e$Date,Date=e$Date)
  }
  else {
    r <- r %>% mutate(Country=m$Country,ID=i,Open_media=m$Date,Open_est=e$Date,Date=min(m$Date,e$Date,na.rm=T))
  }
  if(i == "CHN"){
    r <- r%>% mutate(Country=m$Country,ID=i,Open_media=m$Date,Open_est=e$Date,Date=m$Date)
  }
  Reopen <- Reopen %>% full_join(r)
}
Reopen <- Reopen[-1,]
#Save the data
write.table(Reopen,"Reopening/syl-reopen time.csv",
            row.names=FALSE,col.names=TRUE,sep=",")

