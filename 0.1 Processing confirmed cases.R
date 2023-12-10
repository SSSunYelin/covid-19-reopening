library(reshape2)
library(lubridate)
library(dplyr)

setwd("/Code and data/Data/Original data")

name <- read.table('Final name list.csv',header=TRUE, sep=',')
  
#Set the start and the end date
startdate <- as.Date("2020-01-22") 
enddate <- as.Date("2023-2-7")
ndays <- enddate - startdate + 1
ndaystt <- ts(1:ndays, frequency =1, start =as.Date("2020-01-22"))
ss <- as.Date("2020-01-22")
dates <- seq(from=ss, by=1, length.out=ndays)
#the number of countries
l_country <- length(name$ID)
#days
ndays <- as.numeric(ndays)
#total length
len <- ndays *l_country

#calculating the daily increasing in cases
my_epidata <- read.table('syl-time_series_covid19_confirmed_country.csv',
                         header=TRUE, sep=',') %>% mutate(ID=as.character(ID)) %>% filter(ID %in% name$ID)
#计算每日病例数
for (i in c(1:len)){
  if (i%%ndays==1 || i==1) {
    my_epidata$New_cases[i] <- 0
  }
  else{
    my_epidata$New_cases [i] <- my_epidata$Confirmed[i] - my_epidata$Confirmed [i-1]
    if (my_epidata$New_cases[i] < 0){ #set the data to 0 when the data was below 0.
      my_epidata $New_cases[i] <- 0 
    }
  }
}
write.table(my_epidata,"/Code and data/Data/Infections/syl-time_series_covid19_confirmed_country_to_0.csv",
            row.names=FALSE,col.names=TRUE,sep=",")







