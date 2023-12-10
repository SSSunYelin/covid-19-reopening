library(gbm)
library(patchwork)
library(lattice)
library(ggplot2)
library(caret)
library(recipes)
library(dplyr)
library(tibble)
library(splines)

source("/Code and data/Code/Function/brt.functions.R")

setwd("/Code and data/Data")

#Load data
#Target
name <- read.csv("other data/Final name list.csv") 
Est_infections <- read.csv("Estimated infections/GG_estimate country_right.csv") %>% 
  mutate(Date=as.Date(Date)) %>% select(-X) %>% filter(ID %in% name$ID)
Gather <- read.csv("Gather dataset/syl-time_series_gather_for estimating.csv") %>% 
  mutate(Date=as.Date(Date)) %>% select(-c(X,Lat,Long,date)) %>% filter(ID %in% name$ID)

Pop <- read.csv("other data/covid_other_data.csv") %>% 
  select(Country=location,ID=iso_code,Population=population,Pop_denstiy=population_density) %>%
  filter(ID %in% name$ID)
Pop <- unique(Pop %>% ungroup() %>% select(Country,ID,Population,Pop_denstiy))
Reopen <- read.csv("Reopening/syl-reopen time.csv") %>% 
  mutate(Reopen = as.Date(Date)) %>% filter(!is.na(Reopen)) %>% select(-Date)

Policy <- read.csv("Policy/syl-time_series_country_policy_index.csv") %>% 
  mutate(Date = as.Date(Date)) %>% filter(ID %in% name$ID) %>% select(c("Country","ID","Date","Stringency"))
Workers <- read.csv("other data/Total workers.csv") 
colnames(Workers) <- c("ID","Workers")
Cities <- read.csv("other data/Share population in cities.csv") 
colnames(Cities) <- c("ID","Cities")
#Masks
mask_2021 <- read.csv("other data/data_download_file_reference_2021.csv") %>% 
  select(-c(location_id,version_name)) %>% mutate(date=as.Date(date)) %>% 
  rename(Date=date,Country=location_name,masks=mask_use_mean,mask_obs=mask_use_obs)
mask_2022 <- read.csv("other data/data_download_file_reference_2022.csv") %>% 
  select(-c(location_id,version_name)) %>% mutate(date=as.Date(date)) %>% 
  rename(Date=date,Country=location_name,masks=mask_use_mean,mask_obs=mask_use_obs)
mask_2023 <- read.csv("other data/data_download_file_reference_2023.csv") %>% 
  select(-c(location_id,version_name)) %>% mutate(date=as.Date(date)) %>% 
  rename(Date=date,Country=location_name,masks=mask_use_mean,mask_obs=mask_use_obs)
mask <- rbind(mask_2021,mask_2022,mask_2023)
#NO2
NO <- read.csv("other data/NO.csv")
colnames(NO) <- c("ID","NO")
#temperature
Tem <- read.csv("other data/Temperture.csv") %>% mutate(Date=as.Date(Date))

#Link
data <- Est_infections %>% full_join(Gather) %>% filter(Variant=="Omicron") %>% 
  select(-c(Confirmed,Death,daily_vaccrate,Vacc_rate,Add_rate,New_Add))#link with real infection
data <- data %>% left_join(Reopen,by=c("Country","ID")) %>%  select(-c(Open_media,Open_est))
data <- data %>% left_join(Pop,by="ID") %>% select(-Country.y) %>% rename(Country=Country.x)
data <- data %>% mutate(Est = Est_Infections/Population*100)
data <- data %>% left_join(Policy,by=c("Country","ID","Date"))
data <- data %>% left_join(Workers,by=c("ID"))
data <- data %>% left_join(Cities,by=c("ID"))
data <- data %>% left_join(mask,by=c("ID","Date")) %>% select(-Country.y) %>% rename(Country=Country.x)
data <- data %>% left_join(NO,by=c("ID"))
data <- data %>% left_join(Tem,by=c("ID","Date")) %>% select(-X)

#Fill the data <- data %>% left_join(hands,by=c("ID"))vaccination data
write.csv(data,'BRT/Data for BRT.csv')


#BRT
#After reopening
data_after <- tibble(Country=NA,ID=NA)
for (c in name$ID) {
  print(c)
  d <- data %>% filter(ID==c)
  if(length(d$ID)>0){
    d$Time <-d$Reopen[1]-d$Date[1]
    d <- d %>% filter(Date>=Reopen[1])
    data_after <- data_after %>% full_join(d)
  }
}
data_after <- data_after[-1,]
data_after <- as.data.frame(data_after)
#80% data for training, 20% data for testing
parts <- createDataPartition(data_after$Est,p=.8,list = F)
data_train_after <- data_after[parts,]
data_test_after <- data_after[-parts,]

#BRT
par(mfrow=c(1,1))
BRT_after <- gbm.step(data=data_train_after,
                  gbm.x = c(8,11,14,15,16,18,19,20),
                  gbm.y = 12,
                  family = "gaussian",
                  tree.complexity = 10,
                  learning.rate = 0.01,
                  bag.fraction = 0.5)

#marginal effect
par(mfrow=c(3,3))
gbm.plot(BRT_after, n.plots=8, write.title = F)

interactive_after <- gbm.interactions(BRT_after)

#Response
responses_after <- list(rep(NA,8))
for(i in c(1:8)){
  response.matrix <- plot.gbm(BRT_after, i.var = i, n.trees = BRT_after$n.trees, return.grid = TRUE)
  responses_after[[i]] <- list(data.frame(response.matrix[,1],response.matrix[,2] - mean(response.matrix[,2])))
}
Vacc_marginal <- as.data.frame(responses_after[[1]])
Pop_density_marginal <- as.data.frame(responses_after[[2]])
Workers_marginal <- as.data.frame(responses_after[[3]])
Cities_marginal <- as.data.frame(responses_after[[4]])
masks_marginal <- as.data.frame(responses_after[[5]])
NO2_marginal <- as.data.frame(responses_after[[6]])
Tem_marginal <- as.data.frame(responses_after[[7]])
Time_marginal <- as.data.frame(responses_after[[8]])
#Save for ploting
#Fill the vaccination data
write.csv(Vacc_marginal,'BRT/Marginal effect after reopening/01.Vacc_marginal_after.csv')
write.csv(Pop_density_marginal,'BRT/Marginal effect after reopening/02.Pop_density_marginal_after.csv')
write.csv(Workers_marginal,'BRT/Marginal effect after reopening/03.Workers_marginal_after.csv')
write.csv(Cities_marginal,'BRT/Marginal effect after reopening/04.Cities_marginal_after.csv')
write.csv(masks_marginal,'BRT/Marginal effect after reopening/05.masks_marginal_after.csv')
write.csv(NO2_marginal,'BRT/Marginal effect after reopening/06.NO2_marginal_after.csv')
write.csv(Tem_marginal,'BRT/Marginal effect after reopening/07.Tem_marginal_after.csv')
write.csv(Time_marginal,'BRT/Marginal effect after reopening/08.Time_marginal_after.csv')

#The accuracy of the model
preds_after_test <- predict.gbm(BRT_after,data_test_after,n.trees = BRT_after$gbm.call$best.trees,type = "response")
rmse_after <- RMSE(data_test_after$Est,preds_after_test)
r2_after <- R2(data_test_after$Est,preds_after_test,form="traditional")
#The pearson correlation
#test data
cor.test(data_test_after$Est,preds_after_test,method = "pearson")
#train data
preds_after_train <- predict.gbm(BRT_after,newdata=data_train_after,n.trees = BRT_after$gbm.call$best.trees,type = "response")
cor.test(data_train_after$Est,preds_after_train,method = "pearson")

##interactive effect
#Baseline: without any protection
X1 <- rep(0,50) #Vaccination
X2 <- rep(mean(data_after$Pop_denstiy),50) 
X3 <- rep(mean(data_after$Workers),50) 
X4 <- rep(mean(data_after$Cities),50) 
X5 <- rep(0,50) #mask usage
X6 <- rep(mean(data_after$NO),50) 
X7 <- rep(mean(data_after$Tem),50) 
X8 <- rep(0,50) #reopening time
data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
colnames(data_pres) <- BRT_after$var.names
preds <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
par(mfrow=c(1,1))
plot(preds,type = "o")
lines(lowess(preds))
S1 <- data.frame(baseline=preds)
write.csv(S1,'BRT/Scenario after reopening/01.non-mitigation strategies.csv')

#(1.1)The interaction effect between vaccination and mask usage
predictions_1 <- gbm.perspec(BRT_after,5,1,z.range = c(-0.1,1))
inter_mask <- data.frame(predictions_1[[1]])
write.csv(inter_mask,'BRT/Interactive effect/01.vaccination and mask using/01.mask using_3D.csv')
inter_vacc <- data.frame(predictions_1[[2]])
write.csv(inter_vacc,'BRT/Interactive effect/01.vaccination and mask using/02.vacc_3D.csv')
inter_effect <- data.frame(predictions_1[[3]])
write.csv(inter_effect,'BRT/Interactive effect/01.vaccination and mask using/03.inter_effect_3D.csv')


##mask=0%,only vaccination
X1 <- inter_vacc
X5 <- rep(0,length(X1))
X8 <- rep(0,length(X1))
data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
colnames(data_pres) <- BRT_after$var.names
preds_1 <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
##mask=20%
X5 <- rep(0.20,length(X1))
data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
colnames(data_pres) <- BRT_after$var.names
preds_2 <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
##mask=40%
X5 <- rep(0.40,length(X1))
data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
colnames(data_pres) <- BRT_after$var.names
preds_3 <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
##mask=80%
X5 <- rep(0.60,length(X1)) 
data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
colnames(data_pres) <- BRT_after$var.names
preds_4 <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
#the differences with baseline
S1.1 <- data.frame(Vacc=X1,preds_1,preds_2,preds_3,preds_4,S1$baseline)
colnames(S1.1) <- c("Vacc","preds_1","preds_2","preds_3","preds_4","baseline")
S1.1 <- S1.1 %>% mutate(d1=preds_1-baseline,d2=preds_2-baseline,d3=preds_3-baseline,d4=preds_4-baseline)
write.csv(S1.1,'BRT/Scenario after reopening/02.vaccination with mask.csv')

##(1.2)The interative effect between mask usage and vaccination
##vacc=0%
X5 <- inter_mask 
X1 <- rep(0,length(X5))
X8 <- rep(0,length(X5)) 
data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
colnames(data_pres) <- BRT_after$var.names
preds_1 <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
##vacc=20%
X1 <- rep(20,length(X5))
data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
colnames(data_pres) <- BRT_after$var.names
preds_2 <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
##vacc=40%
X1 <- rep(40,length(X5))
data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
colnames(data_pres) <- BRT_after$var.names
preds_3 <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
##vacc=80%
X1 <- rep(80,length(X5))
data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
colnames(data_pres) <- BRT_after$var.names
preds_4 <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
#the differences with baseline
S1.2 <- data.frame(Mask=X5,preds_1,preds_2,preds_3,preds_4,S1$baseline)
colnames(S1.2) <- c("Mask","preds_1","preds_2","preds_3","preds_4","baseline")
S1.2 <- S1.2 %>% mutate(d1=preds_1-baseline,d2=preds_2-baseline,d3=preds_3-baseline,d4=preds_4-baseline)
write.csv(S1.2,'BRT/Scenario after reopening/02.mask with vaccination.csv')


#(2)Vaccination and reopening time
predictions_2 <- gbm.perspec(BRT_after,8,1,z.range = c(-0.1,1))
inter_time <- data.frame(predictions_2[[1]])
write.csv(inter_time,'BRT/Interactive effect/02.vaccination and time/01.time_3D.csv')
inter_vacc <- data.frame(predictions_2[[2]])
write.csv(inter_vacc,'BRT/Interactive effect/02.vaccination and time/02.vacc_3D.csv')
inter_effect <- data.frame(predictions_2[[3]])
write.csv(inter_effect,'BRT/Interactive effect/02.vaccination and time/03.inter_effect_3D.csv')
##(2.1)vaccination rate and reopening time
##time=0 days
X1 <- inter_vacc
X5 <- rep(0,length(X1))
X8 <- rep(0,length(X1))
data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
colnames(data_pres) <- BRT_after$var.names
preds_1 <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
##time=100 days
X8 <- rep(100,length(X1))
data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
colnames(data_pres) <- BRT_after$var.names
preds_2 <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
##time=200 days
X8 <- rep(200,length(X1))
data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
colnames(data_pres) <- BRT_after$var.names
preds_3 <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
##time=300 days
X8 <- rep(300,length(X1))
data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
colnames(data_pres) <- BRT_after$var.names
preds_4 <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
#the differences with baseline
S2.1 <- data.frame(Vacc=X1,preds_1,preds_2,preds_3,preds_4,S1$baseline)
colnames(S2.1) <- c("Vacc","preds_1","preds_2","preds_3","preds_4","baseline")
S2.1 <- S2.1 %>% mutate(d1=preds_1-baseline,d2=preds_2-baseline,d3=preds_3-baseline,d4=preds_4-baseline)
write.csv(S2.1,'BRT/Scenario after reopening/03.vaccination with time.csv')

##(2.2)The interaction effect 
##vacc=0%
X8 <- inter_time
X1 <- rep(0,50)
X5 <- rep(0,50)
data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
colnames(data_pres) <- BRT_after$var.names
preds_1 <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
##vacc=20%
X1 <- rep(20,50)
data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
colnames(data_pres) <- BRT_after$var.names
preds_2 <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
##vacc=20%
X1 <- rep(20,50)
data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
colnames(data_pres) <- BRT_after$var.names
preds_3 <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
##vacc=80%
X1 <- rep(80,50)
data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
colnames(data_pres) <- BRT_after$var.names
preds_4 <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
#the differences with baseline
S2.2 <- data.frame(Time=X8,preds_1,preds_2,preds_3,preds_4,S1$baseline)
colnames(S2.2) <- c("Time","preds_1","preds_2","preds_3","preds_4","baseline")
S2.2 <- S2.2 %>% mutate(d1=preds_1-baseline,d2=preds_2-baseline,d3=preds_3-baseline,d4=preds_4-baseline)
write.csv(S2.2,'BRT/Scenario after reopening/03.time with vaccination.csv')


#(3)The interactive effect of mask usage and reopening time
predictions_3 <- gbm.perspec(BRT_after,8,5,z.range = c(-0.1,1))
inter_time <- data.frame(predictions_3[[1]])
write.csv(inter_time,'BRT/Interactive effect/03.mask using and time/01.time_3D.csv')
inter_mask <- data.frame(predictions_3[[2]])
write.csv(inter_mask,'BRT/Interactive effect/03.mask using and time/02.mask_3D.csv')
inter_effect <- data.frame(predictions_3[[3]])
write.csv(inter_effect,'BRT/Interactive effect/03.mask using and time/03.inter_effect_3D.csv')

##(3.1)The interactive effect between mask usage and reopening time
##time=0 days
X5 <- inter_mask 
X8 <- rep(0,50) 
X1 <- rep(0,50)
data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
colnames(data_pres) <- BRT_after$var.names
preds_1 <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
##time=100 days
X8 <- rep(100,length(X1)) 
data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
colnames(data_pres) <- BRT_after$var.names
preds_2 <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
##time=200 days
X8 <- rep(200,length(X1)) 
data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
colnames(data_pres) <- BRT_after$var.names
preds_3 <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
##time=300 days
X8 <- rep(300,length(X1)) 
data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
colnames(data_pres) <- BRT_after$var.names
preds_4 <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
#the differences with baseline
S3.1 <- data.frame(Mask=X5,preds_1,preds_2,preds_3,preds_4,S1$baseline)
colnames(S3.1) <- c("Mask","preds_1","preds_2","preds_3","preds_4","baseline")
S3.1 <- S3.1 %>% mutate(d1=preds_1-baseline,d2=preds_2-baseline,d3=preds_3-baseline,d4=preds_4-baseline)
write.csv(S3.1,'BRT/Scenario after reopening/04.mask with time.csv')

##(3.2)The interactive effect between reopening time and mask usage
##mask=0%
X8 <- inter_time 
X5 <- rep(0,50) 
data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
colnames(data_pres) <- BRT_after$var.names
preds_1 <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
##mask=20%
X5 <- rep(0.2,50)
data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
colnames(data_pres) <- BRT_after$var.names
preds_2 <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
##mask=40%
X5 <- rep(0.4,50)
data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
colnames(data_pres) <- BRT_after$var.names
preds_3 <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
##mask=60%
X5 <- rep(0.6,50) 
data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
colnames(data_pres) <- BRT_after$var.names
preds_4 <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
#the differences with baseline
S3.2 <- data.frame(Time=X8,preds_1,preds_2,preds_3,preds_4,S1$baseline)
colnames(S3.2) <- c("Time","preds_1","preds_2","preds_3","preds_4","baseline")
S3.2 <- S3.2 %>% mutate(d1=preds_1-baseline,d2=preds_2-baseline,d3=preds_3-baseline,d4=preds_4-baseline)
write.csv(S3.2,'BRT/Scenario after reopening/04.time with mask.csv')


#(4)Vaccination and temperature
predictions_4 <- gbm.perspec(BRT_after,7,1,z.range = c(-0.1,1))
inter_tem <- data.frame(predictions_4[[1]])
write.csv(inter_tem,'BRT/Interactive effect/04.temperature and vaccination/01.tem_3D.csv')
inter_vacc <- data.frame(predictions_4[[2]])
write.csv(inter_vacc,'BRT/Interactive effect/04.temperature and vaccination/02.vacc_3D.csv')
inter_effect <- data.frame(predictions_4[[3]])
write.csv(inter_effect,'BRT/Interactive effect/04.temperature and vaccination/03.inter_effect_3D.csv')


#(5)mask usage and temparture
predictions_5 <- gbm.perspec(BRT_after,7,5,z.range = c(-0.1,1))
inter_tem <- data.frame(predictions_5[[1]])
write.csv(inter_tem,'BRT/Interactive effect/05.temperature and mask using/01.tem_3D.csv')
inter_mask <- data.frame(predictions_5[[2]])
write.csv(inter_mask,'BRT/Interactive effect/05.temperature and mask using/02.mask_3D.csv')
inter_effect <- data.frame(predictions_5[[3]])
write.csv(inter_effect,'BRT/Interactive effect/05.temperature and mask using/03.inter_effect_3D.csv')

#(6)reopening and temperature
predictions_6 <- gbm.perspec(BRT_after,7,8,z.range = c(-0.1,1))
inter_tem <- data.frame(predictions_6[[1]])
write.csv(inter_tem,'BRT/Interactive effect/06.temperature and time/01.tem_3D.csv')
inter_time <- data.frame(predictions_6[[2]])
write.csv(inter_time,'BRT/Interactive effect/06.temperature and time/02.time_3D.csv')
inter_effect <- data.frame(predictions_6[[3]])
write.csv(inter_effect,'BRT/Interactive effect/06.temperature and time/03.inter_effect_3D.csv')

#(7)share of urban population and vaccination
predictions_7 <- gbm.perspec(BRT_after,4,1,z.range = c(-0.1,1))
inter_cit <- data.frame(predictions_7[[1]])
write.csv(inter_cit,'BRT/Interactive effect/07.cities and vaccination/01.cities_3D.csv')
inter_vacc <- data.frame(predictions_7[[2]])
write.csv(inter_vacc,'BRT/Interactive effect/07.cities and vaccination/02.vacc_3D.csv')
inter_effect <- data.frame(predictions_7[[3]])
write.csv(inter_effect,'BRT/Interactive effect/07.cities and vaccination/03.inter_effect_3D.csv')

#(8)share of urban population and mask usage
predictions_8 <- gbm.perspec(BRT_after,4,5,z.range = c(-0.1,1))
inter_cit <- data.frame(predictions_8[[1]])
write.csv(inter_cit,'BRT/nteractive effect/08.cities and mask using/01.cities_3D.csv')
inter_mask <- data.frame(predictions_8[[2]])
write.csv(inter_mask,'BRT/Interactive effect/08.cities and mask using/02.mask_3D.csv')
inter_effect <- data.frame(predictions_8[[3]])
write.csv(inter_effect,'BRT/Interactive effect/08.cities and mask using/03.inter_effect_3D.csv')

#(9)share of urban population and reopening
predictions_9 <- gbm.perspec(BRT_after,4,8,z.range = c(-0.1,1))
inter_cit <- data.frame(predictions_9[[1]])
write.csv(inter_cit,'BRT/Interactive effect/09.cities and time/01.cities_3D.csv')
inter_time <- data.frame(predictions_9[[2]])
write.csv(inter_time,'BRT/Interactive effect/09.cities and time/02.time_3D.csv')
inter_effect <- data.frame(predictions_9[[3]])
write.csv(inter_effect,'BRT/Interactive effect/09.cities and time/03.inter_effect_3D.csv')


#Before reopening
data_before <- tibble(Country=NA,ID=NA)
for (c in name$ID) {
  print(c)
  d <- data %>% filter(ID==c)
  d <- d %>% filter(Date<Reopen[1])
  data_before <- data_before %>% full_join(d)
}
data_before <- data_before[-1,]
data_before <- as.data.frame(data_before)
#80% data for training, 20% data for testing
parts <- createDataPartition(data_before$Est,p=.8,list = F)
data_train <- data_before[parts,]
data_test <- data_before[-parts,]
#BRT
par(mfrow=c(1,1))
BRT_before <- gbm.step(data=data_train,
                       gbm.x = c(8,11,13,14,15,16,18,19),
                       gbm.y = 12,
                       family = "gaussian",
                       tree.complexity = 10,
                       learning.rate = 0.01,
                       bag.fraction = 0.5)
#marginal effect
par(mfrow=c(2,3))
gbm.plot(BRT_before, n.plots=8, write.title = F)
#Response
responses_before <- list(rep(NA,8)) 
for(i in c(1:8)){
  response.matrix <- plot.gbm(BRT_before, i.var = i, n.trees = BRT_before$n.trees, return.grid = TRUE)
  responses_before[[i]] <- list(data.frame(response.matrix[,1],response.matrix[,2] - mean(response.matrix[,2])))
}
Vacc_marginal <- as.data.frame(responses_before[[1]])
Pop_density_marginal <- as.data.frame(responses_before[[2]])
Str_marginal <- as.data.frame(responses_before[[3]])
Workers_marginal <- as.data.frame(responses_before[[4]])
Cities_marginal <- as.data.frame(responses_before[[5]])
masks_marginal <- as.data.frame(responses_before[[6]])
NO2_marginal <- as.data.frame(responses_before[[7]])
Tem_marginal <- as.data.frame(responses_before[[8]])
#Save for ploting
#Fill the vaccination data
write.csv(Vacc_marginal,'BRT/Marginal effect before reopening/01.Vacc_marginal_before.csv')
write.csv(Pop_density_marginal,'BRT/Marginal effect before reopening/02.Pop_density_marginal_before.csv')
write.csv(Str_marginal,'BRT/Marginal effect before reopening/08.Str_marginal_before.csv')
write.csv(Workers_marginal,'BRT/Marginal effect before reopening/03.Workers_marginal_before.csv')
write.csv(Cities_marginal,'BRT/Marginal effect before reopening/04.Cities_marginal_before.csv')
write.csv(masks_marginal,'BRT/Marginal effect before reopening/05.masks_marginal_before.csv')
write.csv(NO2_marginal,'BRT/Marginal effect before reopening/06.NO2_marginal_before.csv')
write.csv(Tem_marginal,'BRT/Marginal effect before reopening/07.Tem_marginal_before.csv')

#The accuracy of the model
preds <- predict.gbm(BRT_before,data_test,n.trees = BRT_before$gbm.call$best.trees,type = "response")
rmse <- RMSE(data_test$Est,preds)
r2 <- R2(data_test$Est,preds,form="traditional")
#The pearson correlation
cor.test(data_test$Est,preds,method = "pearson")
#train dataset
preds <- predict.gbm(BRT_before,data_train,n.trees = BRT_before$gbm.call$best.trees,type = "response")





















