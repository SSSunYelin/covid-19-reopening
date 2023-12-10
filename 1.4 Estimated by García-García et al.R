#Estimate infections was based on García-García et al and Mena et al.
#The code was re-written by García-García et al.
#Reference list: 
#García-García, M.I. Vigo, E.S. Fonfría, Z. Herrador, M. Navarro, C. Bordehore. (2021). 
#Retrospective methodology to estimate daily infections from deaths (REMEDID) in COVID-19: the Spain case study. Scientific Reports. 11.
#https://doi.org/10.1038/s41598-021-90051-7.
#G.E. Mena, P.P. Martinez, A.S. Mahmud, P.A. Marquet, C.O. Buckee, M. Santillana. (2021). 
#Socioeconomic status determines COVID-19 incidence and related mortality in Santiago, Chile. Science. 372. 
#https://doi.org/10.1126/science.abg5298.

#load package
library(dplyr)

#set workplace
setwd("/Code and data/Data")


#load functions
source("/Code and data/Code/Function/my_distribution.R")

#load data
name <- read.csv("other data/Final name list.csv") 
death_data <- read.csv("Gather dataset/syl-time_series_gather.csv") %>% 
  mutate(date=as.Date(Date))
IFR_Den <- read.csv("Estimated infections/IFR_estimated_SH.csv")

#Delete the countries with the total number of deaths less than 100
d <- death_data %>% filter(date==as.Date("2023-2-7") & Death >200)
d <- unique(d %>% ungroup() %>% select(ID))
death_data <- death_data %>% filter(ID %in% d$ID)
#Save the data
write.csv(death_data,'Gather dataset/syl-time_series_gather_for estimating.csv')

#including 127 countries
global <- death_data %>% filter(ID %in% name$ID) %>% group_by(date) %>% 
  summarise(New_death=sum(New_death,na.rm = T),New_cases=sum(New_cases,na.rm = T), Death=sum(Death,na.rm = T),
            Confirmed=sum(Confirmed,na.rm=T))
global$Country <- "Global"
global$ID <- "All"
global <- global %>% select(Country,ID,date,death=New_death)
death_data <- death_data  %>% select(Country,ID,date,death=New_death)
death_data <- rbind(global,death_data)
death_data <- death_data %>% left_join(IFR_Den,by=c("Country","ID")) %>% select(-X) %>% rename(IFR=q975) ##median

#ID
ID <- unique(death_data %>% ungroup() %>% select(ID))


#parameters
#Others
h_step <- 0.1
N <- 60
min_percentage <- 95

#Distribution of xip and xiod
#Omicron studies
Distribution <- my_distribution_omicron()

x_IP <- Distribution[[1]]
y_IP <- Distribution[[2]]
x_IOD <- Distribution[[3]]
y_IOD <- Distribution[[4]]


#The distribution of infection to death
pdf_aux <- matrix(NA,length(x_IP),2*length(x_IP)-1)
for (x in 1:length(x_IOD)){
  b <- x+length(x_IP)-1
  pdf_aux[x,x:b] = y_IP[x]*y_IOD*h_step
}
prd_convolution <- colSums(pdf_aux,na.rm = T)
x_convolution <- seq(0,2*N,h_step)

#Daily version: Infection to death (ID)
x <- seq(0,N)
k <- 0
l <- seq(1,length(prd_convolution)-1,1/h_step)
pdf_convolution_daily <- matrix(NA,1,120)
for (i in l) {
  k <- k+1
  b <- i+9
  pdf_convolution_daily[k] <- mean(prd_convolution[i:b])
}
pdf_ID <- pdf_convolution_daily[1:length(x)]

#Cumulative Distribution Fuction of pdf_convolution
cdf_ID <- matrix(NA,61)
for (i in 1:length(pdf_ID)) {
  cdf_ID[i] <- sum(pdf_ID[1:i])
}
ind <- which(cdf_ID < min_percentage/100)

#Minumun nunber of days needed according to min_percentage
e <- length(ind)
Nmin <- ind[e]

#save distribution data
distribution1 <- data.frame(x_IP,y_IP,x_IOD,y_IOD)
distribution2 <- data.frame(x_convolution,prd_convolution)
write.csv(distribution1,'Estimated infections/Distribution of Xip and Xiod_right.csv') 
write.csv(distribution2,'Estimated infections/Distribution of Xip_iod_right.csv')


Nextra_aux <- 200
#Estimating daily infections
country_infections <- tibble(Country=NA,ID=NA,Date=as.Date("2020-01-22"),Est_Infections=NA)
for (c in unique(ID$ID)){
  print(c)
  c_infection <- tibble(Country=NA,ID=NA,Date=as.Date("2020-01-22"),Est_Infections=NA)
  d <- death_data %>% filter(ID==c)

  death <- as.matrix(d$death)
  ndeath <- dim(death)
  if(ndeath[1]==1){
    death <- death
  }else if(ndeath[2] == 1){
    death <- t(death)
  }else{
    print('Check dimension of deaths time series.')
  }
  #Check dimension of CFR
  IFR <- as.matrix(d$IFR)
  nIFR <- dim(IFR)
  if(nIFR[1]==1 && nIFR[2]==1){
    IFR2=IFR*rep(1,length(IFR))
  }else if(nIFR[1]==1 && nIFR[2]==length(IFR)){
    IFR2=IFR
  }else if(nIFR[1]==length(IFR) && nIFR[2]==1){
    IFR2=t(IFR)
  }
  death_extended <- c(rep(0,Nextra_aux),death)
  IFR_extended <- c(rep(1,Nextra_aux)*IFR2[1],IFR2)
  
  Nextented <- length(death_extended)
  infection_IFR100 <- rep(0,Nextented)
  
  aux<-1
  for (i in c(1:Nextented)) {
    aux <- death_extended[i:Nextented]
    if(length(aux) > (N+1)){
      k=length(aux)- (N+1)
      aux_pd <- c(pdf_ID,rep(0,k))
    }else{
      pp <- length(aux)
      aux_pd <- pdf_ID[1:pp]
    }
    #Equation 1
    infection_IFR100[i] <- sum(aux*aux_pd)
  }
  
  #Equation 2
  infection_aux <- round(infection_IFR100/IFR_extended*100)
  
  ind_positive <- which(infection_aux>0)
  if(ind_positive[1] <= Nextra_aux){
    a <- ind_positive[1]
    b <- length(infection_aux)
    infections <- infection_aux[a:b]
  }else{
    a <- Nextra_aux+1
    b <- length(infection_aux)
    infections <- infection_aux[a:b]
  }
  
  Nextra <- length(infections)-length(death)
  strdate <- as.Date("2020-1-22")-Nextra
  date <- seq(strdate,as.Date("2023-2-7"),1)
  time <- Nextra+1113
  c_infection <- tibble(Country=rep(d$Country[1],time),ID=rep(c,time),Date=date,Est_Infections=infections)
  country_infections <- country_infections %>% full_join(c_infection)
}
country_infections <- country_infections[-1,]

#country
country <- country_infections %>% filter(ID!="All")
#Save the data
write.csv(country,'Estimated infections/GG_estimate country_right_q975.csv') #_q975 _q025
#global
global <- country_infections %>% filter(ID=="All")
#Save the data
write.csv(global,'Estimated infections/GG_estimate global_right_q975.csv') 
write.csv(country_infections,'Estimated infections/GG_estimate all_right_q975.csv') 

