library(reshape2)
library(lubridate)
library(dplyr)

setwd("/Code and data/Data")


name <- read.table('Original data/Final name list.csv',header=TRUE, sep=',') %>% mutate(ID=as.character(ID))
my_death <- read.table('Deaths/syl_time_series_covid19_deaths_country.csv',
                       header=TRUE, sep=',') %>% mutate(ID=as.character(ID)) %>% filter(ID %in% name$ID)


startdate <- as.Date("2020-01-22") 
enddate <- as.Date("2023-2-7")
ndays <- enddate - startdate + 1
ndaystt <- ts(1:ndays, frequency =1, start =as.Date("2020-01-22"))
ss <- as.Date("2020-01-22")
dates <- seq(from=ss, by=1, length.out=ndays)

l_country <- length(name$ID)
ndays <- as.numeric(ndays)
len <- ndays *l_country


for (i in c(1:len)){
  if (i%%ndays==1 || i==1) {
    my_death$New_death[i] <- 0
  }
  else{
    my_death$New_death [i] <- my_death$Death[i] - my_death$Death [i-1]
    if (my_death$New_death[i] < 0){
      my_death $New_death[i] <- 0 
    }
  }
}

#The data in China from the Chinese CDC
my_death <- my_death %>% mutate(Date=as.Date(Date))
china <- read.table('Deaths/china.csv',header=TRUE, sep=',') %>% mutate(Date=as.Date(Date))
my_death$New_death[my_death$ID=="CHN" & my_death$Date>=as.Date("2022-12-1")] <- china$New_death


write.table(my_death,"Deaths/syl_time_series_covid19_deaths_country_to_0.csv",row.names=FALSE,col.names=TRUE,sep=",")





