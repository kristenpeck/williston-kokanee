 
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
  select(-c("...22","...23","...24")) %>%
  mutate(ifelse(`Weight (g)` <=300,NA,`Weight (g)`))#the measurement of fish below 300mm likely off 
              

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
  
# At Teare Crk they set an extra two nets in 0m and 25m that 
#   both were either inefficient or partially lost. 
#   Exclude at least the 25m net for any analysis and view net 1 with caution

exclude <- GN2021$Site %in% "Teare Creek" & GN2021$Net %in% "6"

GN2021 <- GN2021 %>% 
  filter(!exclude)

# At Forebay, Floating RIC nets were mistakenly used and pulled right away. 
#   Should exclude these nets for any CPUE since they did not sit out 
#   overnight. Will leave in for now.

#view catch by net per site (note that NA is NoFishCaught)
xtabs(~sp+net.depth+Site, data=GN2021, exclude=NULL, na.action=na.pass)



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

#view catch by net per site (no nets had NFC in 2008)
xtabs(~sp+Net+Site, data=GN2008, exclude=NULL, na.action=na.pass)



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
  
#view catch by net per site (no nets had NFC in 2000)
xtabs(~sp+Net+Site, data=GN2000, exclude=NULL, na.action=na.pass)




#combine three years of data

GN <- rbind(GN2000,GN2008,GN2021) %>% 
  mutate(method="GN") %>% 
  mutate(sp=recode(sp,"PC" = "PCC")) %>% 
  mutate(Sex=recode(Sex, "f"="F","m"="M","IM"="UN","?"="UN")) %>% 
  mutate(Maturity=recode(Maturity, "imm"="IM","IMM"="IM","MTC"="MT",
                         "?"="UN","U"="UN")) %>% 
  mutate(ageN = as.numeric(recode(age,"1+"="1","2+"="2",
                                  "3+"="3","4+"="4","5+"="5"))) %>% 
  mutate(yearF = as.factor(year))

#remove transient objects
rm(fish.GN00,fish.GN21,site.GN00,site.GN21, exclude)



#which nets had NFC:
GN[which(is.na(GN$sp)),c("Site","start.datetime","Net","net.depth","FL")]

#Number of each sp caught by year (note that NA = NFC)
xtabs(~sp+year, data=GN, exclude=NULL, na.action=na.pass)

#number of each sp measured for FL
xtabs(~sp+year, data=GN[!is.na(GN$FL),], exclude=NULL, na.action=na.pass)



#check KO age scatter by year and sex. NOTE that 2021 is only estimated ages still

ggplot(GN[which(GN$sp %in% "KO"),])+
  geom_jitter(aes(x=ageN,y=FL, col=as.factor(year)))+
  facet_wrap(~Sex)+
  labs(col="year", title="KO")

#check KO age scatter by year and Maturity. NOTE that 2021 is only estimated ages still

ggplot(GN[which(GN$sp %in% "KO"),])+
  geom_jitter(aes(x=ageN,y=FL, col=as.factor(year)))+
  facet_wrap(~Maturity) 
# Likely some mis-IDs of maturity in here. All "M" should likely be "MT"


#plot FL and Age by species
ggplot(GN[!is.na(GN$FL),])+
  geom_jitter(aes(x=ageN,y=FL, col=sp))+
  facet_wrap(~year, nrow=3)
#note that only estimated KO ages avail for 2021








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
  mutate(age=ifelse(sp%in%"Kokanee"&FL<100, 0, 
                    ifelse(sp%in%"Kokanee"&100<=FL&FL<155,1,
                           ifelse(sp%in%"Kokanee"&FL>=155&FL<197,2,
                                  ifelse(sp%in%"Kokanee"&FL>=197,3,NA)))))
# age FL cutoffs for KO in ReadMe_DJ in gillnet datasheet
# In prelim report mention cut-off of age 0s at 65 mm in acoustic data?
# will replace with confirmed ages when have them






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

TR <- rbind(TR2000short,TR2021) %>% 
  mutate(sp=recode(sp,"Kokanee"="KO","Lake Whitefish"="LW",
                   "Minnow (General)"="C")) %>% 
  mutate(Maturity=recode(Maturity,"Immature"="IM")) %>% 
  mutate(yearF = as.factor(year))

unique(TR$age)

str(TR)


#which trawls had NFC:
TR[which(is.na(TR$sp)),c("Station_ID","start.date","Trawl_num",
                         "sp")]

#Number of each sp caught by year (note that NA = NFC)
xtabs(~sp+year, data=TR, exclude=NULL, na.action=na.pass)

#number of each sp measured for FL
xtabs(~sp+year, data=TR[!is.na(TR$FL),], exclude=NULL, na.action=na.pass)



#check KO age scatter by year and sex. NOTE that 2021 is only estimated ages still

ggplot(TR[which(TR$sp %in% "KO"),])+
  geom_jitter(aes(x=age,y=FL, col=yearF))+
  facet_wrap(~sex)+
  labs(col="year", title="KO")

#check KO age scatter by year and Maturity. NOTE that 2021 is only estimated ages still

ggplot(TR[which(TR$sp %in% "KO"),])+
  geom_jitter(aes(x=age,y=FL, col=yearF))+
  facet_wrap(~Maturity)+
  labs(col="year", title="KO") 
# All "M" should likely be "MT" at the time these surveys took place?


#plot FL and Age by species
ggplot(TR[!is.na(TR$FL),])+
  geom_jitter(aes(x=age,y=FL, col=sp))+
  facet_wrap(~year, nrow=3)
#note that only estimated KO ages avail for 2021





# GN maps #

str(GN)

GN.pts <- st_as_sf(GN, coords = c("Longitude","Latitude"),crs=4326)  #assume WGS84 for all yrs


#this is setting the default colours for all mapviews
mapviewOptions(vector.palette = colorRampPalette(c("white", "cornflowerblue", "grey60")))


GN.map <- mapview(list(GN.pts), 
                  zcol="yearF", 
                  #col.lines = c("snow", "grey"),
                  layer.name = "year")
#legend = list(TRUE, FALSE),
#lwd=2) 
print(GN.map)


# TR maps #
#could not F***ing figure pivot_longer in R, so exported, fixed and re-imported
# to get lats and longs in same columns
TRmapping <- read_excel("TR.transform.xls") %>% 
  mutate(yearF = as.factor(yearF)) 
str(TRmapping)


TR.pts <- st_as_sf(TRmapping, coords = c("Longitude","Latitude"),
                         crs=4326)  #assume WGS84 for all yrs
TR.lines<- TR.pts %>%
  group_by(yearF, Station_ID, Trawl_num) %>%
  dplyr::summarize() %>%
  st_cast("LINESTRING")

TR.map <- mapview(list(TR.pts,TR.lines), 
                  zcol="yearF", 
                  col.lines = c("snow", "grey"),
                  layer.name = "year",
                  legend = list(T, F))

print(TR.map)
# fix points for clearwater trawls - off for both 2000 and 2021


GNTR.map <- mapview(list(GN.pts,TR.lines), 
                    zcol="yearF", 
                    col.lines = c("snow", "grey"),
                    layer.name = "GNyear",
                    legend = list(T, F),
                    lwd=2) 
print(GNTR.map)



#### Combine all GN and TR catch ####

names(GNshort)

GNshort <- GN %>% 
  mutate(start.date = as_date(start.datetime), method="GN") %>% 
  select(method,year,site=Site,start.date,net.trawl.number=Net,sp,FL,wt,sex=Sex,
         mat=Maturity,age=ageN)
TRshort <- TR %>% 
  mutate(method="TR") %>% 
  select(method,year,site=Station_ID,start.date,net.trawl.number=Trawl_num,sp,FL,wt,sex,
         mat=Maturity, age)


GNTR <- rbind(GNshort,TRshort) %>% 
  mutate(yearF = as.factor(year)) %>% 
  mutate(wt = as.numeric(wt)) %>% 
  mutate(k = 10000*(wt/FL*3))

GNTR[which(!is.numeric(GNTR$wt)),]

#KO ages from trawl and gillnet combined:
ggplot(GNTR[which(GNTR$sp %in%"KO"),]) +
  geom_histogram(aes(x=FL, fill=as.factor(age)), binwidth=5,position = "stack")+
  facet_wrap(~year, nrow=3)
#some very old kokanee in 2000 and 2008? No age 0s in 2008 b/c no trawling

ggplot(GNTR[which(GNTR$sp %in%"KO"),])+
  geom_jitter(aes(x=age,y=FL,col=yearF))

ggplot(data=GNTR) +
  geom_histogram(aes(x=k, fill=fl.cat), colour = "black", binwidth= 0.05)+
  geom_vline(xintercept = 1, linetype="dashed")+
  facet_wrap(~lake)+
  labs(y="Frequency", x="Fulton's k", fill="Category")+
  theme_bw()
  




