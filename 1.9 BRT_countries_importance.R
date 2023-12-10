##Estimating the importance in countries

baseline <- S1$baseline[1]


country <- tibble(Country=NA,ID=NA,Vacc=0,Mask=0,Time=0,V_important=0,M_important=0,T_important=0)


for (i in c(1:length(data$ID))){
  g <- data %>% filter(ID==data$ID[i] & !is.na(New_Vacc))
  l <- length(g$ID)
  if(is.na(data$New_Vacc[i]) && data$Date[i]>g$Date[l]){
    data$New_Vacc[i] <- g$New_Vacc[l]
  }
}
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


#The different impacts of mitigation strategies in countries
for (i in name$ID) {
  print(i)
  c <- data_after %>% filter(ID==i)
  
  #Vaccination
  X1 <- rep(max(c$New_Vacc,na.rm=T),50) 
  X2 <- rep(mean(c$Pop_denstiy),50) 
  X3 <- rep(mean(c$Workers),50) 
  X4 <- rep(mean(c$Cities),50) 
  X5 <- rep(0,50) 
  X6 <- rep(mean(c$NO2),50) 
  X7 <- rep(mean(c$Tem),50)
  X8 <- rep(0,50) 
  data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
  colnames(data_pres) <- BRT_after$var.names
  preds <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
  v <- preds[1]-baseline
  
  #mask usage
  X1 <- rep(0,50) 
  X2 <- rep(mean(c$Pop_denstiy),50) 
  X3 <- rep(mean(c$Workers),50) 
  X4 <- rep(mean(c$Cities),50) 
  X5 <- rep(mean(c$masks),50) 
  X6 <- rep(mean(c$NO2),50) 
  X7 <- rep(mean(c$Tem),50) 
  X8 <- rep(0,50) 
  data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
  colnames(data_pres) <- BRT_after$var.names
  preds <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
  m <- preds[1]-baseline
  
  #reopening
  X1 <- rep(0,50) 
  X2 <- rep(mean(c$Pop_denstiy),50) 
  X3 <- rep(mean(c$Workers),50) 
  X4 <- rep(mean(c$Cities),50) 
  X5 <- rep(0,50) 
  X6 <- rep(mean(c$NO2),50) 
  X7 <- rep(mean(c$Tem),50) 
  X8 <- rep(mean(c$Time),50) 
  data_pres <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8)
  colnames(data_pres) <- BRT_after$var.names
  preds <- predict.gbm(BRT_after,data_pres,n.trees = BRT_after$gbm.call$best.trees,type = "response")
  t <- preds[1]-baseline
  
  cc <- tibble(Country=c$Country[1],ID=i,Vacc=v,Mask=m,Time=t,V_important=0,M_important=0,T_important=0)
  country <- country %>% full_join(cc)
}
country <- country[-1,]

country <- country %>% mutate(Total=abs(Vacc)+abs(Mask)+abs(Time)) %>% 
    mutate(V_important=abs(Vacc)/Total,M_important=abs(Mask)/Total,T_important=abs(Time)/Total)

country$Main <- "NA"
for (i in c(1:127)) {
  if(country$V_important[i]>country$M_important[i] && country$V_important[i]>country$T_important[i]){
    country$Main[i] <- "Vaccination"
  }else if(country$M_important[i]>country$V_important[i] && country$M_important[i]>country$T_important[i]){
    country$Main[i] <- "Mask using"
  }else if(country$T_important[i]>country$V_important[i] && country$T_important[i]>country$M_important[i]){
    country$Main[i] <- "Reopening time"
  }
}

write.csv(country,'BRT/Country_differences/01.country_differnces.csv')




