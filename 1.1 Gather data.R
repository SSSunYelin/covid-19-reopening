library(lubridate)

setwd("/Code and data/Data/")


name <- read.csv("other data/Final name list.csv")

strdate <- as.Date("2020-1-22")
enddate <- as.Date("2023-2-7")
ndays <- enddate - strdate + 1
ndays <- as.numeric(ndays)
l_country <- length(name$ID)
len <- l_country*ndays

#Infections
epi <- read.csv('Infections/syl-time_series_covid19_confirmed_country_to_0.csv'
                       , header=TRUE, sep=',') %>% filter(ID %in% name$ID) %>% 
  mutate(Country=as.character(Country),ID=as.character(ID),Date=as.Date(Date)) %>% 
  filter(Date>=strdate & epi$Date<=enddate)

#Death
death <- read.csv('Deaths/syl_time_series_covid19_deaths_country_to_0.csv'
                  , header=TRUE, sep=',') %>% filter(ID %in% name$ID) %>% 
  mutate(Country=as.character(Country),ID=as.character(ID),Date=as.Date(Date)) %>% 
  filter(Date>=strdate & Date<=enddate)
#Variant
variant <- read.csv('Variants/syl-variant_freq_country.csv', 
                    header=TRUE, sep=',') %>% filter(ID %in% name$ID) %>%
  mutate(Country=as.character(Country),ID=as.character(ID),Variant=as.character(Variant),
         Strdate=as.Date(Strdate),Enddate=as.Date(Enddate))

#Vaccination
vacc <- read.csv('Vaccination/syl-time_series_vaccinations_new.csv', 
                 header=TRUE, sep=',') %>% filter(ID %in% name$ID) %>%
  mutate(Country=as.character(Country),ID=as.character(ID),Date=as.Date(Date)) %>%
  filter(Date>=strdate & Date<=enddate)


gather <- data.frame(matrix(NA,len,15))#包含所有流行病学数据，SDI指数和分组，开始接种疫苗日期
epi_name <- colnames(epi)
colnames(gather) <- c(epi_name,"Death","New_death","Variant",
                      "daily_vaccrate","Vacc_rate","Add_rate","New_Vacc","New_Add")
gather$Date <- as.Date(gather$Date)
#gathering
a<-1
for (i in c(1:length(epi$ID))) {
  e <- epi%>% filter(ID==epi$ID[i] & Date==epi$Date[i])
  gather$Country[a] <- e$Country[1]
  gather$ID[a] <- e$ID[1]
  gather$Lat[a] <- e$Lat[1]
  gather$Long[a] <- e$Long[1]
  gather$Date[a] <- e$Date[1]
  gather$Confirmed[a] <- e$Confirmed[1]
  gather$New_cases[a] <- e$New_cases[1]
  d <- death %>% filter(ID==epi$ID[i] & Date==epi$Date[i])
  gather$Death[a] <- d$Death[1]
  gather$New_death[a] <- d$New_death[1]
  #Vaccination
  s <- vacc %>% filter(ID==epi$ID[i] & Date == epi$Date[i])
  if(length(s$ID)==0){
    gather$daily_vaccrate[a] <- NA
    gather$Vacc_rate[a] <- NA
    gather$Add_rate[a] <- NA
    gather$New_Vacc[a] <- NA
    gather$New_Add[a] <- NA
  }
  else{
    gather$daily_vaccrate[a] <- s$daily_vaccrate[1]
    gather$Vacc_rate[a] <- s$Vacc_rate[1]
    gather$Add_rate[a] <- s$Vacc_add[1]
    gather$New_Vacc[a] <- s$New_Vacc_rate[1]
    gather$New_Add[a] <- s$New_Add_rate[1]
  }
  #Variant
  va <- variant[which(variant$ID == epi$ID[i]),]
  if(epi$Date[i] < va$Strdate[1]){
    gather$Variant[a] <- "Normal"
  }
  else if(epi$Date[i] >= va$Strdate[1]){
    gather$Variant[a] <- "Omicron"
  }
  a <- a+1
}
write.table(gather,"Gather dataset/syl-time_series_gather.csv",
            row.names=FALSE,col.names=TRUE,sep=",")



























