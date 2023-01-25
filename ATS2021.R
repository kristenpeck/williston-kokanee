 
### This script analyses data collected during the 2021 Williston Reservoir 
 ### Acoustic Trawl and Gillnetting survey, withsome comparisons to
 ### the 2000 and 2008 surveys

# Author: Kristen Peck
# Date: January 2023

library(tidyverse)
library(lubridate)
library(readxl)
library(sf)
library(mapview)


#### Import GN Data ####

#2021 data
site.GN21 <- read_excel("Williston_Gillnet_Set_data_20211014_revised_KP.xlsx",
                   sheet="Site Data_KP", 
                   col_types = c("text","date","date","text","numeric","text",
                                 "numeric","numeric","date","date","numeric",
                                 "numeric","numeric","text","text","text"))
fish.GN21 <- read_excel("Williston_Gillnet_Set_data_20211014_revised_KP.xlsx",
                   sheet="Fish Data") %>% 
              select(-c("...22","...23","...24"))

GN2021 <- site.GN21 %>% 
  left_join(fish.GN21, by=c("Site","Net"="Net#")) %>% 
  mutate(year = 2021) %>% 
  mutate(set.hr = hour(`Set Time`), set.min = minute(`Set Time`),
         retrieve.hr = hour(`Retrieval Time`),
         retrieve.min = minute(`Retrieval Time`)) %>%
  mutate(start.datetime = ymd_hm(paste(`Set Date`,set.hr,set.min)),
         end.datetime = ymd_hm(paste(`Retrieval Date`,
                                      retrieve.hr,retrieve.min))) %>%
  #mutate(soak.time= as.duration(end.datetime-start.datetime)) %>% 
  select(year,Site,start.datetime,end.datetime,Net,net.type=Net_Type,
         net.depth=Depth,
         MaxDepth,Latitude,Longitude,sp=Species,FL=`Fork Length (mm)`,
         wt=`Weight (g)`,Sex,Maturity,age=`Age Guess`,comments=Comments)

str(GN2021)
  


#2008 data

GN2008 <- read_excel("Williston Fish Data (DS Working- corrected) 2008_KP.xls",
           sheet="All GN sites", skip=5) %>% 
  mutate(year = 2008) %>% 
  mutate(set.hr = hour(`Set Time`), set.min = minute(`Set Time`),
         retrieve.hr = hour(`Retrieval Time`),
         retrieve.min = minute(`Retrieval Time`)) %>%
  mutate(start.datetime = ymd_hm(paste(`Set Date`,set.hr,set.min)),
         end.datetime = ymd_hm(paste(`Retrieval Date`,
                                      retrieve.hr,retrieve.min))) %>%
  select(year,Site,start.datetime,end.datetime,Net=`Net #`,
         MaxDepth,Latitude,Longitude,sp=Species,FL=`Length (mm)`,
         wt=`Weight (g)`,Sex,Maturity,age=Age,comments=Comments) %>% 
  mutate(net.type="RIC6",net.depth=0.5)


str(GN2008)

rbind(GN2008,GN2021)




#2000 data

site.GN00 <- read_excel("Rawdata(DS)_KP.xls", sheet="Sample Data_KP") %>% 
  mutate(Net = as.numeric(substr(Net,5,5)))
fish.GN00 <- read_excel("Rawdata(DS)_KP.xls", sheet="LWdata")

GN2000 <- site.GN00 %>% 
  left_join(fish.GN00, by=c("Site","Net"="Net No.")) %>% 
  mutate(year = 2000) %>% 
  mutate(set.hr = hour(`Time Set`), set.min = minute(`Time Set`),
         retrieve.hr = hour(`Time Pulled`),
         retrieve.min = minute(`Time Pulled`)) %>%
  mutate(start.datetime = ymd_hm(paste(`Set Date`,set.hr,set.min)),
         end.datetime = ymd_hm(paste(`Retrieval Date`,
                                      retrieve.hr,retrieve.min))) %>% 
  select(year,Site,start.datetime,end.datetime,Net,
         MaxDepth,Latitude,Longitude,sp=Species,FL=Length,
         wt=Weight,Sex,Maturity,age=Age,comments=Comment) %>% 
  mutate(net.type="RIC6",net.depth=0)
  
colnames(GN2000)

#combine three years of data

GN <- rbind(GN2000,GN2008,GN2021) %>% 
  mutate(method="GN") %>% 
  mutate(sp=recode(sp,"PC" = "PCC")) %>% 
  mutate(Sex=recode(Sex, "f"="F","m"="M","IM"="UN","?"="UN")) %>% 
  mutate(Maturity=recode(Maturity, "imm"="IM","IMM"="IM","MTC"="MT",
                         "?"="UN","U"="UN")) %>% 
  mutate(ageN = as.numeric(recode(age,"1+"="1","2+"="2",
                                  "3+"="3","4+"="4","5+"="5")))

#write_csv(GN, "check.GN.allyrs.csv", na="")


#check KO age scatter by year

ggplot(GN[which(GN$sp %in% "KO"),])+
  geom_jitter(aes(x=ageN,y=FL, col=as.factor(year)))+
  facet_wrap(~Sex)


rm(fish.GN00,fish.GN21,site.GN00,site.GN21)

# GN maps #







#### Import Trawl Data ####

# TR 2021
fish.TR21 <- read_excel("Williston2021_TILLsData.xlsx", sheet="Sample Data")
str(fish.TR21)


#get the degree decimal minutes into decimal degrees
latitudes21 <- fish.TR21[,c("Station_Num","start.lat.deg",
                         "start.lat.minsec","end.lat.deg",
                         "end.lat.minsec")] %>% 
  mutate(start.lat.minutes= floor(start.lat.minsec),
         end.lat.minutes= floor(end.lat.minsec)) %>% 
  mutate(start.lat.seconds= (start.lat.minsec-start.lat.minutes)*60,
         end.lat.seconds= (end.lat.minsec-end.lat.minutes)*60) 


longitudes21 <- fish.TR21[,c("start.long.deg",
                          "start.long.minsec","end.long.deg",
                          "end.long.minsec")] %>% 
  mutate(start.long.deg= start.long.deg) %>% 
  mutate(end.long.deg= end.long.deg) %>% 
  mutate(start.long.minutes= floor(start.long.minsec),
         end.long.minutes= floor(end.long.minsec)) %>% 
  mutate(start.long.seconds= (start.long.minsec-start.long.minutes)*60,
         end.long.seconds= (end.long.minsec-end.long.minutes)*60) 

locations21 <- cbind(latitudes21,longitudes21) %>% 
  select(Station_Num,start.lat.deg,start.lat.minutes,start.lat.seconds,
         start.long.deg,start.long.minutes,start.long.seconds,
         end.lat.deg,end.lat.minutes,end.lat.seconds,
         end.long.deg,end.long.minutes,end.long.seconds) %>% 
  mutate(start.Latitude = start.lat.deg+(start.lat.minutes/60)+(start.lat.seconds/3600)) %>% 
  mutate(start.Longitude = start.long.deg-(start.long.minutes/60)-(start.long.seconds/3600)) %>% 
  mutate(end.Latitude = end.lat.deg+(end.lat.minutes/60)+(end.lat.seconds/3600)) %>% 
  mutate(end.Longitude = end.long.deg-(end.long.minutes/60)-(end.long.seconds/3600)) %>% 
  select(start.Latitude,start.Longitude,end.Latitude,end.Longitude)

fish.TR21 <- fish.TR21 %>% 
  cbind(locations21) 

TR2021 <- fish.TR21 %>% 
  mutate(method="TR") %>% 
  mutate(start.date=ymd(paste(Year,Month,Day))) %>% 
  select(year=Year,Station_ID = Station_Code,Trawl_num,start.date,
         start.Latitude,start.Longitude,end.Latitude,end.Longitude,
         sp=Species,FL=Fork_Length,wt=Weight, sex=Gender,Maturity) %>% 
  mutate(age=ifelse(FL<100, 0, #age FL cutoffs in ReadMe_DJ in gillnet datasheet
                    ifelse(100<=FL&FL<155,1,
                           ifelse(FL>=155&FL<197,2,
                                  ifelse(FL>=197,3,NA)))))







# TR 2000
fish.TR00 <- read_excel("Williston 2000 Trawl data.xls",sheet="TRAWL DATA",
                        range="A1:S42") %>% 
  mutate(Tloc = paste0("WL0",TLOC1,"0",TNUM)) 

log.TR00 <- read_excel("Williston 2000 Trawl data.xls",sheet="trawl log",
                       range = "A4:W18",col_names = c("year","month","day",
                                                      "Tloc","layer","start.lat.deg",
                                                      "start.lat.minsec","end.lat.deg",
                                                      "end.lat.minsec","Nscaler",
                                                      "start.long.deg",
                                                      "start.long.minsec","end.long.deg",
                                                      "end.long.minsec","Escaler",
                                                      "distance_m","no.layers",
                                                      "start.hr","start.min",
                                                      "end.hr","end.min","total.mins",
                                                      "boat.speed_ms")) 

#get the degree decimal minutes into decimal degrees
latitudes <- log.TR00[,c("Tloc","start.lat.deg",
            "start.lat.minsec","end.lat.deg",
            "end.lat.minsec")] %>% 
  mutate(start.lat.minutes= floor(start.lat.minsec),
         end.lat.minutes= floor(end.lat.minsec)) %>% 
  mutate(start.lat.seconds= (start.lat.minsec-start.lat.minutes)*60,
         end.lat.seconds= (end.lat.minsec-end.lat.minutes)*60) 


longitudes <- log.TR00[,c("start.long.deg",
            "start.long.minsec","end.long.deg",
            "end.long.minsec")] %>% 
  mutate(start.long.deg= start.long.deg*-1) %>% 
  mutate(end.long.deg= end.long.deg*-1) %>% 
  mutate(start.long.minutes= floor(start.long.minsec),
         end.long.minutes= floor(end.long.minsec)) %>% 
  mutate(start.long.seconds= (start.long.minsec-start.long.minutes)*60,
         end.long.seconds= (end.long.minsec-end.long.minutes)*60) 

locations <- cbind(latitudes,longitudes) %>% 
  select(Tloc,start.lat.deg,start.lat.minutes,start.lat.seconds,
         start.long.deg,start.long.minutes,start.long.seconds,
         end.lat.deg,end.lat.minutes,end.lat.seconds,
         end.long.deg,end.long.minutes,end.long.seconds) %>% 
  mutate(start.Latitude = start.lat.deg+(start.lat.minutes/60)+(start.lat.seconds/3600)) %>% 
  mutate(start.Longitude = start.long.deg-(start.long.minutes/60)-(start.long.seconds/3600)) %>% 
  mutate(end.Latitude = end.lat.deg+(end.lat.minutes/60)+(end.lat.seconds/3600)) %>% 
  mutate(end.Longitude = end.long.deg-(end.long.minutes/60)-(end.long.seconds/3600)) %>% 
  select(Tloc,start.Latitude,start.Longitude,end.Latitude,end.Longitude)

log.TR00 <- log.TR00 %>% 
  left_join(locations, by="Tloc") 

#combine fish and trawl log info

TR2000 <- log.TR00 %>% 
  left_join(fish.TR00, by="Tloc") %>% 
  mutate(start.datetime=ymd_hm(paste(year,month,day,start.hr,start.min))) %>% 
  mutate(start.date=ymd(paste(year,month,day))) %>% 
  mutate(end.datetime=ymd_hm(paste(year,month,day,end.hr,end.min))) %>% 
  mutate(method="TR") %>% 
  select(year,Station_ID=Tloc,Trawl_num=TNUM,start.date,start.datetime,end.datetime,layers=DEPTH,
         start.Latitude,start.Longitude,end.Latitude,end.Longitude,
         distance_m,total.mins, boat.speed_ms,sp=SPEC,FL=LEN,wt=WT,
         age=AGE,sex=SEX,Maturity=MAT,comments=COMMENTS)

str(TR2000)


# combine TR2000 and TR2021

TR2000short <- TR2000 %>% 
  select(-c("start.datetime","end.datetime","layers","distance_m",
            "total.mins","comments", "boat.speed_ms"))
names(TR2000short)
names(TR2021)

TR <- rbind(TR2000short,TR2021) %>% 
  mutate(sp=recode(sp,"Kokanee"="KO","Lake Whitefish"="LW",
                   "Minnow (General)"="C")) %>% 
  mutate(Maturity=recode(Maturity,"Immature"="IM"))


str(TR)
unique(TR$age)


ggplot(TR[which(TR$sp %in% "KO"),])+
  geom_histogram(aes(x=FL, fill=as.factor(age)), binwidth = 3) +
  facet_wrap(~year, nrow=2)
  
ggplot(GN[which(GN$sp %in% "KO"),])+
  geom_histogram(aes(x=FL, fill=as.factor(ageN)), binwidth = 3) +
  facet_wrap(~year, nrow=2)


#### Combine all GN and TR catch ####

names(GN)
names(TR)






