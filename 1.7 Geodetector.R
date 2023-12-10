#Geodetector for analyzing the relationship between some factors with population density, policies and vaccination
library(geodetector)
library(GD)
library(dplyr)

#set workplace
setwd("/Code and data/Data")

#Load data
name <- read.csv("other data/Final name list.csv") 
wave_before <- read.csv("Wave/Wave_country_Peak and time_before.csv") %>% 
  select(-X) %>% mutate(Strdate=as.Date(Strdate),Enddate=as.Date(Enddate))
wave_after <- read.csv("Wave/Wave_country_Peak and time_after.csv") %>% 
  select(-X) %>% mutate(Strdate=as.Date(Strdate),Enddate=as.Date(Enddate))
Diff <- read.csv("Wave/Differences between before and after.csv") %>% 
  select(-X) 

Pop <- read.csv("other data/covid_other_data.csv") %>% 
  select(Country=location,ID=iso_code,Population=population,Pop_denstiy=population_density) %>%
  filter(ID %in% name$ID)
Pop <- unique(Pop %>% ungroup() %>% select(Country,ID,Population,Pop_denstiy))
Policy <- read.csv("Policy/syl-time_series_country_policy_index.csv") %>% 
  mutate(Date = as.Date(Date),ID=as.character(ID)) %>% filter(ID %in% name$ID)
Gather <- read.csv("Gather dataset/syl-time_series_gather_for estimating.csv") %>% 
  mutate(Date=as.Date(Date)) %>% select(-c(X,Lat,Long))


Reopen <- read.csv("Reopening/syl-reopen time.csv") %>% 
  mutate(Reopen = as.Date(Date)) %>% filter(!is.na(Reopen)) %>% select(-Date) %>% filter(ID %in% name$ID)

#归因分析元素
Workers <- read.csv("other data/Total workers.csv") 
colnames(Workers) <- c("ID","Workers")
Cities <- read.csv("other data/Share population in cities.csv") 
colnames(Cities) <- c("ID","Cities")
#Masks
mask_2021 <- read.csv("other data/data_download_file_reference_2021.csv") %>% 
  select(-c(location_id,version_name,mask_use_obs)) %>% mutate(date=as.Date(date)) %>% 
  rename(Date=date,Country=location_name,masks=mask_use_mean)
mask_2022 <- read.csv("other data/data_download_file_reference_2022.csv") %>% 
  select(-c(location_id,version_name,mask_use_obs)) %>% mutate(date=as.Date(date)) %>% 
  rename(Date=date,Country=location_name,masks=mask_use_mean)
mask_2023 <- read.csv("other data/data_download_file_reference_2023.csv") %>% 
  select(-c(location_id,version_name,mask_use_obs)) %>% mutate(date=as.Date(date)) %>% 
  rename(Date=date,Country=location_name,masks=mask_use_mean)
mask <- rbind(mask_2021,mask_2022,mask_2023)
#NO2
NO <- read.csv("other data/NO.csv")
colnames(NO) <- c("ID","NO")
#temperature
Tem <- read.csv("other data/Temperture.csv") %>% mutate(Date=as.Date(Date))

#Link
wave_before <- wave_before %>% left_join(Pop,by="ID") %>% select(-Country.y) %>% rename(Country=Country.x)
wave_before <- wave_before %>% left_join(Workers,by="ID")
wave_before <- wave_before %>% left_join(Cities,by="ID")
wave_before <- wave_before %>% left_join(NO,by="ID")

#Calculating the mean data during the wave
wave_before$ID <- as.character(wave_before$ID)
for (i in c(1:length(wave_before$ID))){
  p <- Policy %>% filter(ID==wave_before$ID[i])
  p <- p %>% filter(Date>=wave_before$Strdate[i] & Date <= wave_before$Enddate[i])
  v <- Gather %>% filter(ID==wave_before$ID[i] & Date<=wave_before$Enddate[i])
  v <- v %>% filter(!is.na(New_Vacc))
  m <- mask %>% filter(ID==wave_before$ID[i])
  m <- m %>% filter(Date>=wave_before$Strdate[i] & Date <= wave_before$Enddate[i])
  t <- Tem %>% filter(ID==wave_before$ID[i])
  t <- t %>% filter(Date>=wave_before$Strdate[i] & Date <= wave_before$Enddate[i])
  vacc <- v$New_Vacc[length(v$ID)]#取最新
  wave_before$Vacc[i] <- vacc
  wave_before$Stringency[i] <- mean(p$Stringency)
  wave_before$masks[i] <- mean(m$masks)
  wave_before$Tem[i] <- mean(t$Tem)
}

#analyze the reason after reopening
#Calculating the mean data during the wave
wave_after <- wave_after %>% left_join(Pop,by="ID") %>% select(-Country.y) %>% rename(Country=Country.x)
wave_after <- wave_after %>% left_join(Workers,by="ID")
wave_after <- wave_after %>% left_join(Cities,by="ID")
wave_after <- wave_after %>% left_join(NO,by="ID")
wave_after$ID <- as.character(wave_after$ID)
for (i in c(1:length(wave_after$ID))){
  v <- Gather %>% filter(ID==wave_after$ID[i] & Date<=wave_after$Enddate[i])
  v <- v %>% filter(!is.na(New_Vacc))
  m <- mask %>% filter(ID==wave_after$ID[i])
  m <- m %>% filter(Date>=wave_after$Strdate[i] & Date <= wave_after$Enddate[i])
  t <- Tem %>% filter(ID==wave_after$ID[i])
  t <- t %>% filter(Date>=wave_after$Strdate[i] & Date <= wave_after$Enddate[i])
  w <- Gather %>% filter(ID==wave_after$ID[i] & Variant=="Omicron")
  r <- Reopen %>% filter(ID==wave_after$ID[i])
  vacc <- v$New_Vacc[length(v$ID)]#取最新
  wave_after$Vacc[i] <- vacc
  wave_after$masks[i] <- mean(m$masks)
  wave_after$Tem[i] <- mean(t$Tem)
  wave_after$Time[i] <- r$Reopen[1]-w$Date[1]
}

#保存数据
write.csv(wave_after,'Geodetector/Wave_after.csv')
write.csv(wave_before,'Geodetector/Wave_before.csv')

#算每一个国家开放前后每波的平均值
wave_after <- wave_after %>% group_by(Country,ID) %>% 
  summarise(Peak_infectionrate=mean(Peak_infectionrate),Infections=mean(Infections),days=mean(days),
            Pop_denstiy=mean(Pop_denstiy),Workers=mean(Workers),Cities=mean(Cities),
            NO=mean(NO),Vacc=max(Vacc),masks=mean(masks),Tem=mean(Tem),Time=mean(Time))
wave_before <- wave_before %>% group_by(Country,ID) %>% 
  summarise(Peak_infectionrate=mean(Peak_infectionrate),Infections=mean(Infections),days=mean(days),
            Pop_denstiy=mean(Pop_denstiy),Workers=mean(Workers),Cities=mean(Cities),
            NO=mean(NO),Vacc=max(Vacc),masks=mean(masks),Tem=mean(Tem))

Diff <- wave_after %>% left_join(wave_before,by=c("Country","ID")) %>% 
  mutate(days=days.x-days.y,Peak_infectionrate=Peak_infectionrate.x-Peak_infectionrate.y,
         Infections=Infections.x-Infections.y,Pop_denstiy=Pop_denstiy.y,
         Workers=Workers.y,Cities=Cities.y,NO=NO.y,Vacc=Vacc.x,
         masks=masks.x-masks.y,Tem=Tem.x-Tem.y,
         Time=Time) %>%
  select(Country,ID,days,Peak_infectionrate,Infections,
         Pop_denstiy,Workers,Cities,NO,Vacc,masks,Tem,Time)
Diff <- na.omit(Diff)
write.csv(Diff,'Geodetector/Differeces before and after.csv')

#判断什么因素主导了增加
discmethod <- c("quantile","geometric") #choose the best discreat method
disciv <- c(3:20)
Diff <- as.data.frame(Diff)

#感染率的变化
#find the best interval__infectionrate
opt_inf_diff <- optidisc(Infections ~ Vacc + Pop_denstiy + Workers + Cities + NO + masks + Tem + Time, 
                            data=Diff,discmethod = discmethod,discitv = disciv)
#Give interval based on the above results
oct_Vacc <- disc(Diff$Vacc,opt_inf_diff$Vacc$n.itv,method = opt_inf_diff$Vacc$method)
oct_Pop_density <- disc(Diff$Pop_denstiy,opt_inf_diff$Pop_denstiy$n.itv,method = opt_inf_diff$Pop_denstiy$method)
oct_Workers <- disc(Diff$Workers,opt_inf_diff$Workers$n.itv,method = opt_inf_diff$Workers$method)
oct_Cities <- disc(Diff$Cities,opt_inf_diff$Cities$n.itv,method = opt_inf_diff$Cities$method)
oct_NO <- disc(Diff$NO,opt_inf_diff$NO$n.itv,method = opt_inf_diff$NO$method)
oct_masks <- disc(Diff$masks,opt_inf_diff$masks$n.itv,method = opt_inf_diff$masks$method)
oct_Tem <- disc(Diff$Tem,opt_inf_diff$Tem$n.itv,method = opt_inf_diff$Tem$method)
oct_Time <- disc(Diff$Time,opt_inf_diff$Time$n.itv,method = opt_inf_diff$Time$method)

#Discreate
Diff$dis_vacc <- cut(Diff$Vacc,breaks = oct_Vacc$itv,include.lowest = TRUE)
Diff$dis_pop_denstiy <- cut(Diff$Pop_denstiy,breaks = oct_Pop_density$itv,include.lowest = TRUE)
Diff$dis_Workers<- cut(Diff$Workers,breaks = oct_Workers$itv,include.lowest = TRUE)
Diff$dis_Cities<- cut(Diff$Cities,breaks = oct_Cities$itv,include.lowest = TRUE)
Diff$dis_NO<- cut(Diff$NO,breaks = oct_NO$itv,include.lowest = TRUE)
Diff$dis_masks<- cut(Diff$masks,breaks = oct_masks$itv,include.lowest = TRUE)
Diff$dis_Tem<- cut(Diff$Tem,breaks = oct_Tem$itv,include.lowest = TRUE)
Diff$dis_Time<- cut(Diff$Time,breaks = oct_Time$itv,include.lowest = TRUE)
#Geodetector
Result_inf_diff <- gd(Infections ~ dis_vacc + dis_pop_denstiy  + dis_Workers + dis_Cities + dis_NO + 
                   dis_masks + dis_Tem + dis_Time, data = Diff)
#Interaction effect
Result_inf_diff_interaction <- gdinteract(Infections ~ dis_vacc + dis_pop_denstiy + 
                                            dis_Workers + dis_Cities + dis_NO + 
                                            dis_masks + dis_Tem + dis_Time, data = Diff)

Result_inf_diff <- as.data.frame(Result_inf_diff$Factor)
write.csv(Result_inf_diff,'Geodetector/Reason_Wave_after_interval_Infection.csv')

#Peak infection rate
#find the best interval__infectionrate
opt_peak_diff <- optidisc(Peak_infectionrate ~ Vacc + Pop_denstiy + Workers + Cities + NO + masks + Tem + Time, 
                         data=Diff,discmethod = discmethod,discitv = disciv)

#Give interval based on the above results
oct_Vacc <- disc(Diff$Vacc,opt_peak_diff$Vacc$n.itv,method = opt_peak_diff$Vacc$method)
oct_Pop_density <- disc(Diff$Pop_denstiy,opt_peak_diff$Pop_denstiy$n.itv,method = opt_peak_diff$Pop_denstiy$method)
oct_Workers <- disc(Diff$Workers,opt_peak_diff$Workers$n.itv,method = opt_peak_diff$Workers$method)
oct_Cities <- disc(Diff$Cities,opt_peak_diff$Cities$n.itv,method = opt_peak_diff$Cities$method)
oct_NO <- disc(Diff$NO,opt_peak_diff$NO$n.itv,method = opt_peak_diff$NO$method)
oct_masks <- disc(Diff$masks,opt_peak_diff$masks$n.itv,method = opt_peak_diff$masks$method)
oct_Tem <- disc(Diff$Tem,opt_peak_diff$Tem$n.itv,method = opt_peak_diff$Tem$method)
oct_Time <- disc(Diff$Time,opt_peak_diff$Time$n.itv,method = opt_peak_diff$Time$method)

#Discreate
Diff$dis_vacc <- cut(Diff$Vacc,breaks = oct_Vacc$itv,include.lowest = TRUE)
Diff$dis_pop_denstiy <- cut(Diff$Pop_denstiy,breaks = oct_Pop_density$itv,include.lowest = TRUE)
Diff$dis_Workers<- cut(Diff$Workers,breaks = oct_Workers$itv,include.lowest = TRUE)
Diff$dis_Cities<- cut(Diff$Cities,breaks = oct_Cities$itv,include.lowest = TRUE)
Diff$dis_NO<- cut(Diff$NO,breaks = oct_NO$itv,include.lowest = TRUE)
Diff$dis_masks<- cut(Diff$masks,breaks = oct_masks$itv,include.lowest = TRUE)
Diff$dis_Tem<- cut(Diff$Tem,breaks = oct_Tem$itv,include.lowest = TRUE)
Diff$dis_Time<- cut(Diff$Time,breaks = oct_Time$itv,include.lowest = TRUE)

#Geodetector
Result_peak_diff <- gd(Peak_infectionrate ~ dis_vacc + dis_pop_denstiy + dis_Workers + dis_Cities + dis_NO + 
                        dis_masks + dis_Tem + dis_Time, data = Diff)
#Interaction effect
Result_peak_diff_interaction <- gdinteract(Peak_infectionrate ~ dis_vacc + dis_pop_denstiy + dis_Workers + dis_Cities + dis_NO + 
                                            dis_masks + dis_Tem + dis_Time, data = Diff)
Result_peak_diff <- as.data.frame(Result_peak_diff$Factor)
write.csv(Result_peak_diff,'Geodetector/Reason_Wave_after_interval_peak_Infection.csv')

#Duration
#find the best interval__infectionrate
opt_days_diff <- optidisc(days ~ Vacc + Pop_denstiy + Workers + Cities + NO + masks + Tem + Time, 
                          data=Diff,discmethod = discmethod,discitv = disciv)
#Give interval based on the above results
oct_Vacc <- disc(Diff$Vacc,opt_days_diff$Vacc$n.itv,method = opt_days_diff$Vacc$method)
oct_Pop_density <- disc(Diff$Pop_denstiy,opt_days_diff$Pop_denstiy$n.itv,method = opt_days_diff$Pop_denstiy$method)
oct_Workers <- disc(Diff$Workers,opt_days_diff$Workers$n.itv,method = opt_days_diff$Workers$method)
oct_Cities <- disc(Diff$Cities,opt_days_diff$Cities$n.itv,method = opt_days_diff$Cities$method)
oct_NO <- disc(Diff$NO,opt_days_diff$NO$n.itv,method = opt_days_diff$NO$method)
oct_masks <- disc(Diff$masks,opt_days_diff$masks$n.itv,method = opt_days_diff$masks$method)
oct_Tem <- disc(Diff$Tem,opt_days_diff$Tem$n.itv,method = opt_days_diff$Tem$method)
oct_Time <- disc(Diff$Time,opt_days_diff$Time$n.itv,method = opt_days_diff$Time$method)

#Discreate
Diff$dis_vacc <- cut(Diff$Vacc,breaks = oct_Vacc$itv,include.lowest = TRUE)
Diff$dis_pop_denstiy <- cut(Diff$Pop_denstiy,breaks = oct_Pop_density$itv,include.lowest = TRUE)
Diff$dis_Workers<- cut(Diff$Workers,breaks = oct_Workers$itv,include.lowest = TRUE)
Diff$dis_Cities<- cut(Diff$Cities,breaks = oct_Cities$itv,include.lowest = TRUE)
Diff$dis_NO<- cut(Diff$NO,breaks = oct_NO$itv,include.lowest = TRUE)
Diff$dis_masks<- cut(Diff$masks,breaks = oct_masks$itv,include.lowest = TRUE)
Diff$dis_Tem<- cut(Diff$Tem,breaks = oct_Tem$itv,include.lowest = TRUE)
Diff$dis_Time<- cut(Diff$Time,breaks = oct_Time$itv,include.lowest = TRUE)
#Geodetector
Result_days_diff <- gd(days ~ dis_vacc + dis_pop_denstiy + dis_Workers + dis_Cities + dis_NO + 
                         dis_masks + dis_Tem + dis_Time, data = Diff)
#Interaction effect
Result_days_diff_interaction <- gdinteract(days ~ dis_vacc + dis_pop_denstiy + dis_Workers + dis_Cities + dis_NO + 
                                             dis_masks + dis_Tem + dis_Time, data = Diff)
Result_days_diff <- as.data.frame(Result_days_diff$Factor)
write.csv(Result_days_diff,'Geodetector/Reason_Wave_after_interval_days.csv')












