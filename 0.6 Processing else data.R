library(lubridate)
library(dplyr)
library(tidyr)

setwd("/Code and data/Data/Original data")

name <- read.csv("Final name list.csv") 
pop <- read.csv("population aged.csv") %>% filter(ID!="#N/A") %>% filter(ID %in% name$ID)
pop_1 <- pop %>% as_tibble() %>% select(Country,ID,
                                        P5.9,P10.14,P15.19,
                                        P20.24,P25.29,P30.34,P35.39,
                                        P40.44,P45.49,P50.54,P55.59,
                                        P60.64,P65.69,P70.74,P75.79,
                                        P80.84,P85.89,P90.94,P95.99,P100) %>%
  pivot_longer(-c("Country","ID"), names_to = "Age",values_to = "Count") %>%
  mutate(Age=rep(c(17,17,17,39,39,39,39,59,59,59,59,79,79,79,79,80,80,80,80,80),length(pop$ID)),Count=as.numeric(Count)) %>%
  group_by(Country,ID,Age) %>%
  summarise(Count=sum(Count)) %>%
  rename(Pop=Count)


global <- tibble(Country="Global",ID="All",Age=c(17,39,59,79,80))
pop_2 <- pop_1 %>% filter(ID %in% name$ID)
t_global <- pop_2 %>% group_by(Age) %>% summarise(Pop=sum(Pop))
global <- global %>% full_join(t_global)
pop_1 <- as.data.frame(pop_1)
population <- rbind(global,pop_1)

#Save as csv.
write.csv(population,'/Code and data/Data/other data/Pop_Global_admin_SH.csv')



