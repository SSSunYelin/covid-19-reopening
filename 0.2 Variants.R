library(xlsx)
library(dplyr)
library(reshape2)


#定位
setwd("/Code and data/Data")


name <- read.table('Original data/Final name list.csv',header=TRUE, sep=',') %>% mutate(ID=as.character(ID))

Omicron <- read.csv('Original data/syl-time_series_omicron_freq_country.csv') %>%  
  mutate(Date=as.Date(Date),ID=as.character(ID),Country=as.character(Country)) %>% filter(ID %in% name$ID)
Delta <- read.csv("Original data/syl-time_series_delta_freq_country.csv",1) %>% 
  mutate(Date=as.Date(Date),ID=as.character(ID),Country=as.character(Country)) %>% filter(ID %in% name$ID)


#计算病毒感染者每日占比
#设置参数
#生成时间序列
strdate <- as.Date("2019-12-29")
enddate <- as.Date("2023-2-12")
ndays <- (enddate-strdate)/7+1
dates <- seq.Date(from=strdate,by="week",length.out = ndays)
ndays <- as.numeric(ndays)
#一些参数
#符合要求的研究国家数量
l_country <- length(name$ID)
len <- l_country*ndays

#计算每种变异病毒的时间范围
variant_freq <- data.frame(matrix(NA,l_country*2,5))
colnames(variant_freq) <- c("Country","ID","Variant","Strdate","Enddate")
variant_freq$Country <- as.character(variant_freq$Country)
variant_freq$ID <- as.character(variant_freq$ID)
variant_freq$Strdate <- as.Date(variant_freq$Strdate)
variant_freq$Enddate <- as.Date(variant_freq$Enddate)
a<-1
for (i in c(1:l_country)) {
  
   d <- Delta %>% filter(ID==name$ID[i] & Freq>=90)
   ds <- length(d$Country)
   
   o <- Omicron %>% filter(ID==name$ID[i] & Freq>=90) #above 90%
   os <- length(o$Country)
   
   if(os>1 && ds>=1){
     for (j in c(1:os)) { 
       #Ensure that there was not the dominance of Delta variant after the dominance of Omicron variant.
       if(o$Date[j] > max(d$Date)){ 
         variant_freq$Country[a] <- o$Country[1]
         variant_freq$ID[a] <- name$ID[i]
         variant_freq$Variant[a] <- "Omicron"
         variant_freq$Strdate[a] <- o$Date[j]-7
         variant_freq$Enddate[a] <- enddate 
         a <- a+1
         break
       }
     }
   }else if(os>1 && ds==0){ #The data of delta variant is NA, the data of Omicron variant could be used.
     variant_freq$Country[a] <- o$Country[1]
     variant_freq$ID[a] <- name$ID[i]
     variant_freq$Variant[a] <- "Omicron"
     variant_freq$Strdate[a] <- o$Date[1]-7
     variant_freq$Enddate[a] <- enddate 
     a <- a+1
   }
   
}

variant_freq <- na.omit(variant_freq)
write.table(variant_freq,"Variants/syl-variant_freq_country.csv",
            row.names=FALSE,col.names=TRUE,sep=",")







