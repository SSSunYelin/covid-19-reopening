#Estimate real infections by the study in Shanghai
#The code was re-written based on Mena et al. 2021.
#Refernce: Mena, G. E., P. P. MartinezA. S. Mahmud et al. (2021). Socioeconomic status determines COVID-19 incidence and related mortality in Santiago, Chile. Science, 372:https://doi.org/10.1126/science.abg5298

library(tidyr)
library(dplyr)
library(tibble)

#Set the workplace
setwd("/Code and data/Data")

#Load the function.R
source("/Code and data/Code/Function/IFR_adjust.R")

#Load the dataset
#1.Place ID
name <- read.csv("other data/Final name list.csv") %>% 
  mutate(ID=as.character(ID)) %>% as_tibble()
#2.Input data
#2.1 Input population data
pop_SH <- read.csv("other data/Pop_Global_admin_SH.csv")  %>% 
  select(-c(X)) %>% mutate(Pop=Pop*1000)

#Estimate the real IFR by NY methods for Omicron
adj_SH <- IFR_estimate(pop_SH,1000,'Shanghai')
adj_countries_SH <- adj_SH[[1]] %>% 
  summarize(median=median(IFR_adj), q975=quantile(IFR_adj,0.975),q025=quantile(IFR_adj,0.025))

#Save the dataset
write.csv(adj_countries_SH,
          'Estimated infections/IFR_estimated_SH.csv')



