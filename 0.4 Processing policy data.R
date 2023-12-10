library(dplyr)

setwd("/Code and data/Data")

name <- read.table('Original data/Final name list.csv',header=TRUE, sep=',') %>% mutate(ID=as.character(ID))
policy <- read.csv('Original data/syl-origin_time_series_country_policy.csv',header=TRUE, sep=',') %>% 
  filter(CountryCode %in% name$ID)


l_country <- length(name$ID)
strdate <- as.Date("2020-1-1")
enddate <- as.Date("2022-12-31")
ndays <- as.numeric(enddate-strdate+1)
dates <- seq(from=strdate,by=1,length.out=ndays)


#Processing data
my_policy <- policy %>% select(Country=CountryName,ID=CountryCode,Date=Date2,Stringency=StringencyIndex_Average,C1=C1M_School.closing,
                              C2=C2M_Workplace.closing,C3=C3M_Cancel.public.events,C4=C4M_Restrictions.on.gatherings,C5=C5M_Close.public.transport,
                              C6=C6M_Stay.at.home.requirements,C7=C7M_Restrictions.on.internal.movement,C8=C8EV_International.travel.controls,
                              H1=H1_Public.information.campaigns,H2=H2_Testing.policy,H3=H3_Contact.tracing,H6=H6M_Facial.Coverings) %>%
  mutate(Country=as.character(Country),ID=as.character(ID))
write.table(my_policy,"Policy/syl-time_series_country_policy_index.csv",
            row.names=FALSE,col.names=TRUE,sep=",")


relax <- data.frame(matrix(NA,length(name$ID)*10000,3))
colnames(relax) <- c("Country","ID","Date")
relax$Date <- as.Date("2022-1-1")
relax$Date <- as.Date(relax$Date)
a <- 1
#找出开放
for (i in c(1:length(my_policy$ID))){
  n <- 8
  if(is.na(my_policy$C1[i]) || my_policy$C1[i]<1){
    n <- n-1
  }
  if(is.na(my_policy$C2[i]) || my_policy$C2[i]<1){
    n <- n-1
  }
  if(is.na(my_policy$C3[i]) ||my_policy$C3[i]<1){
    n <- n-1
  }
  if(is.na(my_policy$C4[i]) ||my_policy$C4[i]<1){
    n <- n-1
  }
  if(is.na(my_policy$C5[i]) ||my_policy$C5[i]<1){
    n <- n-1
  }
  if(is.na(my_policy$C6[i]) ||my_policy$C6[i]<1){
    n <- n-1
  }
  if(is.na(my_policy$C7[i]) ||my_policy$C7[i]<1){
    n <- n-1
  }
  if(is.na(my_policy$C8[i]) ||my_policy$C8[i]<1){
    n <- n-1
  }
  if(n <= 4){
    relax$Country[a] <- my_policy$Country[i]
    relax$ID[a] <- my_policy$ID[i]
    relax$Date[a] <- my_policy$Date[i]
    a <- a+1
  }
}
relax <- na.omit(relax)

#Find the reopening date from OxCGGT (some countries may already be reopened when omicron variant began to be dominant)
relax <- relax %>% 
  mutate(Country=as.character(Country),ID=as.character(ID),Date=as.Date(Date))

variant <- read.csv('Variants/syl-variant_freq_country.csv', 
                    header=TRUE, sep=',')
variant$Country <- as.character(variant$Country)
variant$ID <- as.character(variant$ID)
variant$Variant <- as.character(variant$Variant)
variant$Strdate <- as.Date(variant$Strdate)
variant$Enddate <- as.Date(variant$Enddate)


reopen <- data.frame(matrix(NA,length(name$ID),3))
colnames(reopen) <- c("Country","ID","Reopen_date")
reopen$Country <- as.character(reopen$Country)
reopen$ID <- as.character(reopen$ID)
reopen$Reopen_date <- as.Date("2020-1-1")
a<-1
for (c in name$ID) {
  print(c)
  v <- variant %>% filter(ID==c)
  r <- relax %>% filter(ID==c & Date>=v$Strdate[1])
  if(length(v$ID)==0){
    reopen$Country[a] <- r$Country[1]
    reopen$ID[a] <- c
    reopen$Reopen_date[a] <- NA
    a <- a+1
  }else{
    if(length(r$ID)==0){
      reopen$Country[a] <- r$Country[1]
      reopen$ID[a] <- c
      reopen$Reopen_date[a] <- NA
      a <- a+1
    }else{
      strdate <- r$Date[1]
      for (i in c(2:length(r$ID))) {
        if(r$Date[i]-r$Date[i-1]>1){
          strdate <- r$Date[i]
        }
        if(i==length(r$ID) && r$Date[i]<as.Date("2022-12-31")){
          strdate <- NA
        }
      }
      reopen$Country[a] <- r$Country[1]
      reopen$ID[a] <- c
      reopen$Reopen_date[a] <- strdate
      a <- a+1
    }

  }
}

write.table(reopen,"Policy/syl-time_series_country_policy_reopen_time_estimate.csv",
            row.names=FALSE,col.names=TRUE,sep=",")






