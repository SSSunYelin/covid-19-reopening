library(dplyr)
library(missForest)


setwd("/Desktop/Code and data/Data")

name <- read.table('Original data/Final name list.csv',header=TRUE, sep=',') %>% mutate(ID=as.character(ID))

#The data from our world in data dataset
vacc_wid <- read.table("Original data/vaccinations from owid.csv", header=TRUE, sep=',') %>% 
  mutate(location=as.character(location),iso_code=as.character(iso_code),date=as.Date(date)) %>% 
  filter(iso_code %in% name$ID)


#process the vaccination data
vacc_daily <- data.frame(matrix(NA,length(vacc_wid$iso_code)*10,8))
colnames(vacc_daily) <- c("Country","ID","Date","daily_vaccrate",
                          "Vacc_rate","Vacc_add","New_Vacc_rate","New_Add_rate")
vacc_daily$Country <- as.character(vacc_daily$Country)
vacc_daily$ID <- as.character(vacc_daily$ID)
vacc_daily$Date <- as.Date(vacc_daily$Date)
a <- 1
for (i in c(1:length(vacc_wid$location))) {
  #Whether the date is consecutive
  if(vacc_wid$date[i+1]-vacc_wid$date[i]>1 && i!=length(vacc_wid$location)){
    str <- vacc_wid$date[i]
    end <- vacc_wid$date[i+1]
    days <- end-str
    dates <- seq(from=str, by=1, length.out=days)
    for (j in c(1:days)) {
      vacc_daily$Country[a] <- vacc_wid$location[i]
      vacc_daily$ID[a] <- vacc_wid$iso_code[i]
      vacc_daily$Date[a] <- dates[j]
      vacc_daily$daily_vaccrate[a] <- NA
      vacc_daily$Vacc_rate[a] <- NA
      vacc_daily$Vacc_add[a] <- NA
      a <- a+1
    }
  }
  else{
    vacc_daily$Country[a] <- vacc_wid$location[i]
    vacc_daily$ID[a] <- vacc_wid$iso_code[i]
    vacc_daily$Date[a] <- vacc_wid$date[i]
    vacc_daily$daily_vaccrate[a] <- vacc_wid$daily_people_vaccinated_per_hundred[i]
    vacc_daily$Vacc_rate[a] <- vacc_wid$people_fully_vaccinated_per_hundred[i] #vaccination rate
    vacc_daily$Vacc_add[a] <- vacc_wid$total_boosters_per_hundred[i] #booster vaccination rate
    a <- a+1
  }
}

#interpolated the data (vaccination rate) to each day
a<-1
v1<-0
v2<-0
for (i in c(1:length(name$ID))) {
  v <- vacc_daily[which(vacc_daily$ID == name$ID[i]),]
  l <- length(v$ID)
  for(j in c(1:l)){
    if(is.na(v$Vacc_rate[j])){
      if(j==1){
        v$New_Vacc_rate[j] <-0
      }
      else{
        for (x1 in c(j:1)) {
          if(!is.na(v$Vacc_rate[x1])){
            v1 <- v$Vacc_rate[x1]
            break
          }
        }
        for(x2 in c(j:l)){
          if(!is.na(v$Vacc_rate[x2])){
            v2 <- v$Vacc_rate[x2]
            break
          }
        }
        if(v2 >0){
          d <- (v2-v1)/(x2-x1)
          v$New_Vacc_rate[j] <- v$New_Vacc_rate[j-1]+d
          v1<-0
          v2<-0
        }
        else{
          v$New_Vacc_rate[j] <- v1
          v1<-0
          v2<-0
        }
      }
    }
    else{
      v$New_Vacc_rate[j] <- v$Vacc_rate[j]
    }
  }
  b <- a+l-1
  vacc_daily$New_Vacc_rate[a:b] <- v$New_Vacc_rate
  a <- a+l
}

#interpolated the data (booster) to each day.
a<-1
v1<-0
v2<-0
for (i in c(1:length(name$ID))) {
  v <- vacc_daily[which(vacc_daily$ID == name$ID[i]),]
  l <- length(v$ID)
  v_wid <- vacc_wid[which(vacc_wid$iso_code == name$ID[i]),]
  v_wid <- v_wid [which(v_wid$total_boosters>0),]
  b_date <- v_wid$date[1]
  if(is.na(b_date)){
    b <- a+l-1
    v$New_Add_rate <- NA
    vacc_daily$New_Add_rate[a:b] <- v$New_Add_rate
    a <- a+l
  }
  else{
    for(j in c(1:l)){
      if(is.na(v$Vacc_add[j]) && v$Date[j]>=b_date){
        if(j==1){
          v$New_Add_rate[j] <-0
        }
        else{
          for (x1 in c(j:1)) {
            if(!is.na(v$Vacc_add[x1])){
              v1 <- v$Vacc_add[x1]
              break
            }
          }
          for(x2 in c(j:l)){
            if(!is.na(v$Vacc_add[x2])){
              v2 <- v$Vacc_add[x2]
              break
            }
          }
          if(v2 >0){
            d <- (v2-v1)/(x2-x1)
            v$New_Add_rate[j] <- v$New_Add_rate[j-1]+d
            v1<-0
            v2<-0
          }
          else{
            v$New_Add_rate[j] <- v1
            v1<-0
            v2<-0
          }
        }
      }
      else{
        v$New_Add_rate[j] <- v$Vacc_add[j]
      }
    }
    b <- a+l-1
    vacc_daily$New_Add_rate[a:b] <- v$New_Add_rate
    a <- a+l
  }
}

vacc_daily <- vacc_daily %>% filter(!is.na(ID))
write.table(vacc_daily,"Vaccination/syl-time_series_vaccinations_new.csv",row.names=FALSE,col.names=TRUE,sep=",")















