#Validation of GG method
#load package
library(dplyr)

#set workplace
setwd("/Code and data/Data")

#Load data
name <- read.csv("other data/Final name list.csv") 
Est_infections <- read.csv("Estimated infections/GG_estimate all_right.csv") %>% #_truncated
  mutate(Date=as.Date(Date)) %>% select(-X) 
Est_global <- read.csv("Estimated infections/GG_estimate global_right.csv") %>% #_truncated
  mutate(Date=as.Date(Date)) %>% select(-X) %>% filter(Date>=as.Date("2021-11-21"))

Gather <- read.csv("Gather dataset/syl-time_series_gather_for estimating.csv") %>% 
  mutate(Date=as.Date(Date)) %>% select(-c(X,Lat,Long)) %>% filter(ID %in% name$ID)
All <- Gather %>% group_by(Date) %>% summarise(New_cases=sum(New_cases)) %>% filter(Date>=as.Date("2021-11-21"))
Est_global <- Est_global %>% left_join(All, by=c("Date"))

#Link
Est_infections <- Est_infections %>% full_join(Gather) %>% filter(Variant=="Omicron") %>% 
  select(c(Country,ID,Date,Est_Infections,New_cases))
Est_infections <- rbind(Est_global,Est_infections)

ID <- unique(Est_infections %>% ungroup() %>% select(ID))

#Pearson's correlation between two sequance
Validation <- tibble(Country=NA,ID=NA,Cor=NA,P=NA)
for(c in ID$ID){
  print(c)
  est <- Est_infections %>% filter(ID==c & !is.na(Est_Infections) & Est_Infections !=Inf)
  r <- cor.test(est$Est_Infections,est$New_cases,method="pearson")
  result <- tibble(Country=est$Country[1],ID=c,Cor=r$estimate,P=r$p.value)
  Validation <- Validation %>% full_join(result)
}
Validation <- Validation[-1,]
write.csv(Validation,'Estimated infections/GG_estimate global_right_validation.csv')






