#Choose the Omicron period and Gather the data for plot
#load package
library(dplyr)

#set workplace
setwd("/Code and data/Data")

#Load data
name <- read.csv("other data/Final name list.csv") 
Est_infections <- read.csv("Estimated infections/GG_estimate country_right.csv") %>% 
  mutate(Date=as.Date(Date)) %>% select(-X)

Gather <- read.csv("Gather dataset/syl-time_series_gather_for estimating.csv") %>% 
  mutate(Date=as.Date(date)) %>% select(-c(X,Lat,Long,date))

Reopen <- read.csv("Reopening/syl-reopen time.csv") %>% 
  mutate(Reopen = as.Date(Date)) %>% filter(!is.na(Reopen)) %>% select(-Date)
Pop <- read.csv("other data/covid_other_data.csv") %>% select(Country=location,ID=iso_code,Population=population)
Pop <- unique(Pop %>% ungroup() %>% select(Country,ID,Population))

#Link
Est_infections <- Est_infections %>% full_join(Gather) %>% filter(Variant=="Omicron")
Est_infections <- Est_infections %>% left_join(Reopen,by=c('Country','ID')) %>% select(-c(Open_media,Open_est)) %>%
  rename(Est = Est_Infections) %>% filter(!is.na(Reopen))
write.csv(Est_infections,'Wave/GG_estimate country_right_link_Omicron.csv')


####################################
#1.Calculate the wave after reopening#
####################################
#1.1 Defining wave after reopening
wave_after <- tibble(Country=NA,ID=NA,Group=NA,Strdate=as.Date("2020-1-1"),Enddate=as.Date("2020-1-1"),days=NA)
for (c in name$ID){
  print(c)
  est <- Est_infections %>% filter(ID==c)
  est <- est %>% filter(Date >= Reopen[1])
  threshold <- max(est$Est)*0.05
  if(threshold==0){next}
  w <- est %>% filter(Est >= threshold) %>% mutate(dif = c(1,diff(Date,1)))
  #find continue 14 days
  n <- 0
  for (i in c(1:length(w$ID))) {
    if(i==1){
      n <- 1
    }else if(w$dif[i]==1){
      n <- n+1
      if(i==length(w$ID)){
        if(n>=14){
          wave <- tibble(Country=w$Country[1],ID=c,Group=w$Group[1],Strdate=w$Date[i]-n+1,Enddate=w$Date[i],days=n)
          wave_after <- wave_after %>% full_join(wave)
        }
      }
    }else if(w$dif[i]>1){
      if(n>=14){
        wave <- tibble(Country=w$Country[1],ID=c,Group=w$Group[1],Strdate=w$Date[i-1]-n+1,Enddate=w$Date[i-1],days=n)
        wave_after <- wave_after %>% full_join(wave)
        n <- 1
      }
      else{
        n <- 1
      }
    }
  }
}
wave_after <- wave_after[-1,]

#1.2 Calculating the peak number during wave
wave_after$Peak <- 0
wave_after$Peak_infectionrate <- 0
wave_after$Cases <- 0
wave_after$Infections <- 0
wave_after$P_date <- as.Date("2019-1-1")
wave_after$Duration <- 0
for (i in c(1:length(wave_after$ID))) {
  est <- Est_infections %>% filter(ID==wave_after$ID[i] & Date>=wave_after$Strdate[i] & Date<= wave_after$Enddate[i])
  max_est <- max(est$Est,na.rm = T)
  date <- est %>% filter(Est==max_est) #peak number
  date <- date$Date[1] #Date of peak number
  pop <- Pop %>% filter(ID==wave_after$ID[i])
  wave_after$Peak[i] <- max_est
  wave_after$Peak_infectionrate[i] <- max_est/pop$Population*100
  wave_after$Cases[i] <- sum(est$Est,na.rm = T)
  wave_after$Infections[i] <- wave_after$Cases[i]/pop$Population*100
  wave_after$P_date[i] <- date
  wave_after$Duration[i] <- wave_after$Enddate[i]-wave_after$Strdate[i]+1
}
#Save the data
write.csv(wave_after,'Wave/Wave_country_Peak and time_after.csv')
wave_after_table <- tibble(Variable=NA,Mean=0,Num=0,SD=0,sqrt_N=0,low95=0,high95=0)
#Mean Freq and Duration
mean_duration <- mean(wave_after$Duration)
Num <- length(wave_after$ID)
sd <- sd(wave_after$Duration)
sqrt_N <- sd/sqrt(Num)
low95 <- mean_duration-1.96*sqrt_N
high95 <- mean_duration+1.96*sqrt_N
w <- tibble(Variable="Duration",Mean=mean_duration,Num=Num,SD=sd,sqrt_N=sqrt_N,low95=low95,high95=high95)
wave_after_table <- wave_after_table %>% full_join(w)

#Mean peak infection rate
mean_peakinfectionrate <- mean(wave_after$Peak_infectionrate)
Num <- length(wave_after$ID)
sd <- sd(wave_after$Peak_infectionrate)
sqrt_N <- sd/sqrt(Num)
low95 <- mean_peakinfectionrate-1.96*sqrt_N
high95 <- mean_peakinfectionrate+1.96*sqrt_N
w <- tibble(Variable="Peakinfectionrate",Mean=mean_peakinfectionrate,Num=Num,SD=sd,sqrt_N=sqrt_N,low95=low95,high95=high95)
wave_after_table <- wave_after_table %>% full_join(w)

#Mean infection rate
mean_infectionrate <- mean(wave_after$Infections)
Num <- length(wave_after$ID)
sd <- sd(wave_after$Infections)
sqrt_N <- sd/sqrt(Num)
low95 <- mean_infectionrate-1.96*sqrt_N
high95 <- mean_infectionrate+1.96*sqrt_N
w <- tibble(Variable="Infectionrate",Mean=mean_infectionrate,Num=Num,SD=sd,sqrt_N=sqrt_N,low95=low95,high95=high95)
wave_after_table <- wave_after_table %>% full_join(w)

#1.3 Calculating the date reached peak after reopening
wave_time_after <- tibble(Country=NA,ID=NA,Group=NA,P_date=as.Date("2020-1-1"),Reopen=as.Date("2020-1-1"),Time=NA,Freq=NA)
for(c in name$ID){
  print(c)
  w <- wave_after %>% filter(ID==c)
  freq <- length(w$ID)
  re <- Reopen %>% filter(ID==c)
  Time <- w$P_date[1]-re$Reopen
  wave <- tibble(Country=w$Country[1],ID=c,Group=w$Group[1],P_date=w$P_date[1],Reopen=re$Reopen,Time=Time,Freq=freq)
  wave_time_after <- wave_time_after %>% full_join(wave)
}
wave_time_after <- wave_time_after[-1,]
write.csv(wave_time_after,'Wave/Wave_country_date reached peak after reopening.csv')

#Mean reached time
Mean_reach <- mean(wave_time_after$Time)
Mean_reach <- as.numeric(Mean_reach)
Num <- length(wave_time_after$ID)
sd <- sd(wave_time_after$Time)
sqrt_N <- sd/sqrt(Num)
low95 <- Mean_reach-1.96*sqrt_N
high95 <- Mean_reach+1.96*sqrt_N
w <- tibble(Variable="Reaching Time",Mean=Mean_reach,Num=Num,SD=sd,sqrt_N=sqrt_N,low95=low95,high95=high95)
wave_after_table <- wave_after_table %>% full_join(w)
wave_after_table <- wave_after_table[-1,]
write.csv(wave_after_table,'Wave/wave_after_table.csv')

#some countries do not wave after reopening
name <- unique(wave_after %>% ungroup() %>% select(ID))

####################################
#2.Calculate the wave before reopening#
####################################
Est_infections <- Est_infections %>% filter(ID %in% name$ID)
wave_before <- tibble(Country=NA,ID=NA,Group=NA,Strdate=as.Date("2020-1-1"),Enddate=as.Date("2020-1-1"),days=NA)
for (c in name$ID){
  print(c)
  est <- Est_infections %>% filter(ID==c)
  est <- est %>% filter(Date < Reopen[1])
  threshold <- max(est$Est)*0.05
  if(threshold==0 || threshold==-Inf){next}
  w <- est %>% filter(Est >= threshold) %>% mutate(dif = c(1,diff(Date,1)))
  #find continue 14 days
  n <- 0
  for (i in c(1:length(w$ID))) {
    if(i==1){
      n <- 1
    }else if(w$dif[i]==1){
      n <- n+1
      if(i==length(w$ID)){
        if(n>=14){
          wave <- tibble(Country=w$Country[1],ID=c,Group=w$Group[1],Strdate=w$Date[i]-n+1,Enddate=w$Date[i],days=n)
          wave_before <- wave_before %>% full_join(wave)
        }
      }
    }else if(w$dif[i]>1){
      if(n>=14){
        wave <- tibble(Country=w$Country[1],ID=c,Group=w$Group[1],Strdate=w$Date[i-1]-n+1,Enddate=w$Date[i-1],days=n)
        wave_before <- wave_before %>% full_join(wave)
        n <- 1
      }
      else{
        n <- 1
      }
    }
  }
}
wave_before <- wave_before[-1,]

#2.2 Calculating the peak number during wave before reopening
wave_before$Peak <- 0
wave_before$Peak_infectionrate <- 0
wave_before$Cases <- 0
wave_before$Infections <- 0
wave_before$P_date <- as.Date("2019-1-1")
wave_before$Duration <- 0
for (i in c(1:length(wave_before$ID))) {
  est <- Est_infections %>% filter(ID==wave_before$ID[i] & Date>=wave_before$Strdate[i] & Date<= wave_before$Enddate[i])
  max_est <- max(est$Est,na.rm = T)
  date <- est %>% filter(Est==max_est) #peak number
  date <- date$Date[1] #Date of peak number
  pop <- Pop %>% filter(ID==wave_before$ID[i])
  wave_before$Peak[i] <- max_est
  wave_before$Peak_infectionrate[i] <- max_est/pop$Population*100
  wave_before$Cases[i] <- sum(est$Est)
  wave_before$Infections[i] <- wave_before$Cases[i]/pop$Population*100
  wave_before$P_date[i] <- date
  wave_before$Duration[i] <- wave_before$Enddate[i]-wave_before$Strdate[i]+1
}
#some countries do not wave after reopening
name <- unique(wave_before %>% ungroup() %>% select(ID))

#Save the data
write.csv(wave_before,'Wave/Wave_country_Peak and time_before.csv')
#Mean Freq and Duration
mean_duration <- mean(wave_before$Duration)
wave_before_table <- tibble(Variable=NA,Mean=0,Num=0,SD=0,sqrt_N=0,low95=0,high95=0)
#Mean Freq and Duration
mean_duration <- mean(wave_before$Duration)
Num <- length(wave_before$ID)
sd <- sd(wave_before$Duration)
sqrt_N <- sd/sqrt(Num)
low95 <- mean_duration-1.96*sqrt_N
high95 <- mean_duration+1.96*sqrt_N
w <- tibble(Variable="Duration",Mean=mean_duration,Num=Num,SD=sd,sqrt_N=sqrt_N,low95=low95,high95=high95)
wave_before_table <- wave_before_table %>% full_join(w)

#Mean peak infection rate
mean_peakinfectionrate <- mean(wave_before$Peak_infectionrate)
Num <- length(wave_before$ID)
sd <- sd(wave_before$Peak_infectionrate)
sqrt_N <- sd/sqrt(Num)
low95 <- mean_peakinfectionrate-1.96*sqrt_N
high95 <- mean_peakinfectionrate+1.96*sqrt_N
w <- tibble(Variable="Peak_infectionrate",Mean=mean_peakinfectionrate,Num=Num,SD=sd,sqrt_N=sqrt_N,low95=low95,high95=high95)
wave_before_table <- wave_before_table %>% full_join(w)

#Mean infection rate
mean_infectionrate <- mean(wave_before$Infections)
Num <- length(wave_before$ID)
sd <- sd(wave_before$Infections)
sqrt_N <- sd/sqrt(Num)
low95 <- mean_infectionrate-1.96*sqrt_N
high95 <- mean_infectionrate+1.96*sqrt_N
w <- tibble(Variable="Infections",Mean=mean_infectionrate,Num=Num,SD=sd,sqrt_N=sqrt_N,low95=low95,high95=high95)
wave_before_table <- wave_before_table %>% full_join(w)
wave_before_table <- wave_before_table[-1,]
write.csv(wave_before_table,'Wave/wave_before_table.csv')

########################################################
#3.Calculate the differences before and after reopening#
########################################################
Wave_B <- wave_before %>% group_by(Country,ID) %>% 
  summarise(days=mean(days),Peak=mean(Peak),Peak_infectionrate=mean(Peak_infectionrate),
            Cases=mean(Cases),Infections=mean(Infections))
Wave_A <- wave_after %>% group_by(Country,ID) %>% 
  summarise(days=mean(days),Peak=mean(Peak),Peak_infectionrate=mean(Peak_infectionrate),
            Cases=mean(Cases),Infections=mean(Infections)) %>% filter(ID %in% Wave_B$ID)

Diffe <- Wave_A %>% left_join(Wave_B,by=c("Country","ID")) %>% 
  mutate(days=days.x-days.y,Peak=Peak.x-Peak.y,Peak_infectionrate=Peak_infectionrate.x-Peak_infectionrate.y,
         Cases=Cases.x-Cases.y, Infections=Infections.x-Infections.y) %>%
  select(Country,ID,days,Peak,Peak_infectionrate,Cases,Infections)

write.csv(Diffe,'Wave/Differences between before and after.csv')





