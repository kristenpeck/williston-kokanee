 
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
library(FSA)
library(nnet)
library(car)

options(warn = 1)
mapviewOptions(vector.palette = colorRampPalette(c("white", "cornflowerblue", "grey60")))


#### Import GN Data ####

#2021 data
site.GN21 <- read_excel("Williston_Gillnet_Set_data_20211014_revised_KP.xlsx",
                   sheet="Site Data_KP", 
                   col_types = c("text","date","date","text","numeric","text",
                                 "numeric","numeric","date","date","numeric",
                                 "numeric","numeric","text","text","text"))
fish.GN21 <- read_excel("Williston_Gillnet_Set_data_20211014_revised_KP.xlsx",
                   sheet="Fish Data",
                   col_types = c("text","text","text","text","text","numeric",
                                 "numeric","numeric","numeric","text","text",
                                 "text","text","text","text","text","text",
                                 "text","text","numeric","numeric","text","text","text")) %>% 
  select(-c("...22","...23","...24")) #%>%
 # mutate(`Weight (g)` = ifelse(`Weight (g)` <=300,NA,`Weight (g)`))#the measurement of fish below 300mm likely off 
              

GN2021 <- site.GN21 %>% 
  full_join(fish.GN21, by=c("Site","Net"="Net#"),multiple="all") %>% 
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

#exclude <- GN2021$Site %in% "Teare Creek" & GN2021$Net %in% "6"

#GN2021 <- GN2021 %>% 
#  filter(!exclude)

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
fish.GN00 <- read_excel("Rawdata(DS)_KP.xls", sheet="LWdata",
                        col_types = c("text","text","text","numeric",
                        "text","numeric","numeric","numeric","numeric",
                        "text","text","numeric","text","text","numeric") )
str(fish.GN00)

GN2000 <- site.GN00 %>% 
  full_join(fish.GN00, by=c("Site","Net"="Net No."), multiple="all") %>% 
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
  mutate(sp=dplyr::recode(sp,"PC" = "PCC")) %>% 
  mutate(Sex=dplyr::recode(Sex, "f"="F","m"="M","IM"="UN","?"="UN")) %>% 
  mutate(Maturity=dplyr::recode(Maturity, "imm"="IM","IMM"="IM","MTC"="MT",
                         "?"="UN","U"="UN")) %>% 
  mutate(ageN = as.numeric(dplyr::recode(age,"1+"="1","2+"="2",
                                  "3+"="3","4+"="4","5+"="5"))) %>% 
  mutate(yearF = as.factor(year)) %>% 
  mutate(soak.time = as.numeric(difftime(end.datetime,start.datetime, units="hours"))) %>% 
  mutate(Reach = dplyr::recode(Site, "Forebay"="Peace", "Clearwater"="Peace",
                        "Nabesche"="Peace", "Adams Creek"="Peace",
                        "Finlay Forks"="Junction", "Teare Creek"="Finlay",
                        "Factor Ross"="Finlay","Blackwater"="Parsnip",
                        "Heather Point"="Parsnip"))
# summary of catch per year per site and depth.
xtabs(~sp+net.depth+Site+year, data=GN, na.action=na.omit,drop.unused.levels = F)
#which nets had NFC:
GN[which(is.na(GN$sp)),c("Site","start.datetime","Net","net.depth","FL")]
#Number of each sp caught by year (note that NA = NFC)
xtabs(~sp+year, data=GN, exclude=NULL, na.action=na.pass)
#number of each sp measured for FL
xtabs(~sp+year, data=GN[!is.na(GN$FL),], exclude=NULL, na.action=na.pass)
#number of each sp measured for wt
xtabs(~sp+year, data=GN[!is.na(GN$wt),], exclude=NULL, na.action=na.pass)


#### GN map ####

str(GN)

GN.pts <- st_as_sf(GN, coords = c("Longitude","Latitude"),crs=4326)  #assume WGS84 for all yrs

GN.map <- mapview(list(GN.pts), 
                  zcol="yearF", 
                  layer.name = "year")

print(GN.map)


#### GN CPUE ####

#the added panel on RIC7 nets should include more fish between 130-160mm:
#which fish are between 130 and 160 mm FL in RIC6 nets?

(RIC6.fish <- GN %>% 
  filter(net.type %in% "RIC6") %>% 
  filter(FL >130 & FL<160) %>% 
  select(year,Reach,Site,net.depth,net.type,sp,FL))
RIC6plot <- ggplot(RIC6.fish)+
  geom_histogram(aes(x=FL, fill=sp),col="black", binwidth=1)+
  facet_wrap(~year,ncol=1)+
  labs(title="RIC6 catch, FL 130-160mm")
RIC6plot
#and which fish are between 130 and 160 mm FL in RIC7 nets?
(RIC7.fish <- GN %>% 
  filter(net.type %in% c("RIC7","RIC7-hole","RIC7-inefficient")) %>% 
  filter(FL >130 & FL<160) %>% 
  select(year,Reach,Site,net.depth,net.type,sp,FL))

RIC7plot <- ggplot(RIC7.fish)+
  geom_histogram(aes(x=FL, fill=sp), col="black",binwidth=1)+
  facet_wrap(~year,ncol=1)+
  labs(title="RIC7 catch in FL 130-160mm")
RIC7plot



#in 2008, called station Nabesche but close to Clearwater for our purposes...

GN$Site[which(GN$Site %in% "Nabesche")] <- "Clearwater"


# catch by effort with 130-160mm FL removed from ALL nets
catch.per.effort <- GN %>% 
  filter(Site != "Adams Creek") %>% 
  #filter(net.type %in% c("RIC6","RIC7","RIC7-hole")) %>% #exclude inefficient sets of 2021
  #filter(FL < 130 | FL > 160) %>% #exclude FLs between130 and 160mm
  group_by(year,Reach,Site,net.depth=round(net.depth,0),Net) %>% 
  summarize(start = first(start.datetime),end = first(end.datetime),
            KO=sum(sp%in%"KO"),LW=sum(sp%in%"LW")) %>% 
  mutate(soak.time = as.numeric(difftime(end,start,units="hours"))) %>% 
  mutate(cpue.KO=KO/soak.time, cpue.LW = LW/soak.time)




CPUEKOplot <- ggplot(catch.per.effort)+
  geom_col(aes(x=net.depth,y=cpue.KO, fill=as.character(Site)),col="black")+
  coord_flip()+
  scale_x_reverse()+
  facet_grid(rows=vars(year),cols=vars(Reach))+
  labs(x="net depth (m)", fill="")
CPUEKOplot

CPUELWplot <- ggplot(catch.per.effort)+
  geom_col(aes(x=net.depth,y=cpue.LW, fill=as.character(Site)), col="black")+
  coord_flip()+
  scale_x_reverse()+
  facet_grid(rows=vars(year),cols=vars(Reach))+
  labs(x="net depth (m)", fill="")
CPUELWplot

#summary of efforts per year
efforts.per.year <- catch.per.effort %>% 
  group_by(Site,Reach,year) %>% 
  summarize(no.nets=length(unique(Net)), KO=sum(KO),LW=sum(LW),
            soak.time=sum((soak.time)),KOcpue = round(KO/soak.time,1),
            LWcpue = round(LW/soak.time,1)) 
efforts.per.year

#summary of efforts per year from 0-10m depth
efforts.per.year.under10 <- catch.per.effort %>% 
  filter(net.depth <= 10) %>% 
  group_by(Site,Reach,year) %>% 
  summarize(no.nets=length(unique(Net)), KO=sum(KO),LW=sum(LW),
            soak.time=sum((soak.time)),KOcpue = round(KO/soak.time,1),
            LWcpue = round(LW/soak.time,1)) 
efforts.per.year.under10


ggplot(efforts.per.year.under10) +
  geom_histogram(aes(x=KOcpue, fill=as.character(Site)), binwidth=0.2)+
  facet_grid(rows=vars(year),cols=vars(Reach))+
  labs(x="KO CPUE", fill="")

ggplot(efforts.per.year.under10) +
  geom_histogram(aes(x=LWcpue, fill=as.character(Site)), binwidth=0.2)+
  facet_grid(rows=vars(year),cols=vars(Reach))+
  labs(x="LW CPUE", fill="")


cpue.stats <- catch.per.effort %>% 
  filter(net.depth <= 10) %>% 
  group_by(Site, Reach, year) %>% 
  summarize(nets= length(unique(Net)),mn.KO = mean(KO),sd.KO = sd(KO), 
            mn.LW = mean(LW), sd.LW = sd(LW),
            mn.soak.time=mean(soak.time),se.KO = sd(KO)/sqrt(length(Net)),
            se.LW = sd(LW)/sqrt(length(Net))) %>% 
  mutate(mn.cpue.KO = mn.KO/mn.soak.time, mn.cpue.LW = mn.LW/mn.soak.time,
         sd.cpue.KO = sd.KO/mn.soak.time, sd.cpue.LW = sd.LW/mn.soak.time,
         se.cpue.KO = se.KO/mn.soak.time, se.cpue.LW = se.LW/mn.soak.time,
         CI.cpue.KO = se.cpue.KO*1.96, CI.cpue.LW = se.cpue.LW*1.96)
cpue.stats
str(cpue.stats)

table.cpue.stats <- cpue.stats %>% 
  select(Reach, Site, Year=year, Nets=nets, `Mean Soak Time (hrs)`=mn.soak.time,
         `Mean CPUE KO`=mn.cpue.KO, `StDev CPUE KO`=sd.cpue.KO,
         `Mean CPUE LW`=mn.cpue.LW,`StDev CPUE LW`=sd.cpue.LW)

#pick only sites with multipleyears of data
compare.sites.raw <- catch.per.effort %>% 
  filter(net.depth <= 10) %>% 
  filter(Site %in% c("Clearwater","Finlay Forks","Forebay","Heather Point"))

compare.sites <- cpue.stats %>% 
  filter(Site %in% c("Clearwater","Finlay Forks","Forebay","Heather Point"))


plot.mn.cpue <- ggplot(compare.sites)+
  geom_point(aes(x=year,y=mn.cpue.KO), col="purple", size=4,alpha=0.5) +
  geom_errorbar(aes(x=year,
                    ymin=mn.cpue.KO-CI.cpue.KO,ymax=mn.cpue.KO+CI.cpue.KO), 
                width=0.4, linewidth=1,alpha=0.5, col="purple")+
  geom_point(aes(x=year,y=mn.cpue.LW), col="blue", size=4,alpha=0.5) +
  geom_errorbar(aes(x=year,
                    ymin=mn.cpue.LW-CI.cpue.LW,ymax=mn.cpue.LW+CI.cpue.LW), 
                width=0.4, linewidth=1, alpha=0.5, col="blue")+
  geom_smooth(aes(x=year,y=mn.cpue.KO),method = "lm", se=F, alpha=0.25,col="purple")+
  geom_smooth(aes(x=year,y=mn.cpue.LW),method = "lm", se=F, col="blue",alpha=0.25)+
  facet_wrap(~Site)+
  labs(y="Mean CPUE (KO in purple, LW in blue)")
plot.mn.cpue

ggplot(cpue.stats)+
  geom_point(aes(x=year,y=mn.cpue.LW)) +
  geom_errorbar(aes(x=year,
                    ymin=mn.cpue.LW-sd.cpue.LW,ymax=mn.cpue.LW+sd.cpue.LW), 
                width=0.4)+
  geom_smooth(aes(x=year,y=mn.cpue.LW),method = "lm",se = F)+
  facet_wrap(~Site)

summary(lm(data=compare.sites.raw,formula= KO~year))

summary(lm(data=compare.sites.raw,formula= LW~year))





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
  mutate(sp=dplyr::recode(sp,"Kokanee"="KO","Lake Whitefish"="LW",
                   "Minnow (General)"="C")) %>% 
  mutate(Maturity=dplyr::recode(Maturity,"Immature"="IM")) %>% 
  mutate(yearF = as.factor(year))



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

#print(TR.map)
# fix points for clearwater trawls - off for both 2000 and 2021


#### GNTR map ####
GNTR.map <- mapview(list(GN.pts,TR.lines), 
                    zcol="yearF", 
                    col.lines = c("snow", "grey"),
                    layer.name = "GNyear",
                    legend = list(T, F),
                    lwd=2) 
#print(GNTR.map)




#### QAQC Trawl ####

#which trawls had NFC:
TR[which(is.na(TR$sp)),c("Station_ID","start.date","Trawl_num",
                         "sp")]
#Number of each sp caught by year (note that NA = NFC)
xtabs(~sp+year, data=TR, exclude=NULL, na.action=na.pass)

#number of each sp measured for FL
xtabs(~sp+year, data=TR[!is.na(TR$FL),], exclude=NULL, na.action=na.pass)

#number of each sp measured for wt
xtabs(~sp+year, data=TR[!is.na(TR$wt),], exclude=NULL, na.action=na.pass)


#check KO scatter by year and sex. 
# appears that sex may have been assigned strangely in 2021- small fish only females
# will assume all small fish (e.g.<200mm) too hard to distinguish sex -> IM
ggplot(TR[which(TR$sp %in% "KO"),])+
  geom_jitter(aes(x=log10(FL),y=log10(wt), col=yearF))+
  facet_wrap(~sex)+
  labs(col="year", title="KO")

#check LW scatter by year and sex
ggplot(TR[which(TR$sp %in% "LW"),])+
  geom_jitter(aes(x=log10(FL),y=log10(wt), col=yearF))+
  facet_wrap(~sex)+
  labs(col="year", title="LW")

#check KO age scatter by year and Maturity. NOTE that 2021 is only estimated ages still

ggplot(TR[which(TR$sp %in% "KO"),])+
  geom_jitter(aes(x=age,y=FL, col=yearF))+
  facet_wrap(~Maturity)+
  labs(col="year", title="KO") 
# All "M" should likely be "MT" at the time these surveys took place?

#check LW age scatter by year and Maturity. NOTE that 2021 is only estimated ages still

ggplot(TR[which(TR$sp %in% "LW"),])+
  geom_jitter(aes(x=age,y=FL, col=yearF))+
  facet_wrap(~Maturity)+
  labs(col="year", title="LW") 







#plot FL and Age by species
ggplot(TR[!is.na(TR$FL),])+
  geom_jitter(aes(x=age,y=FL, col=sp))+
  facet_wrap(~year, nrow=3)
#note that only estimated KO ages avail for 2021














#### Combine GN and TR catch ####

GNshort <- GN %>% 
  mutate(start.date = as_date(start.datetime), method="GN") %>% 
  select(method,year,reach=Reach,site=Site,start.date,net.trawl.number=Net,sp,FL,wt,sex=Sex,
         mat=Maturity,age=ageN)

site.reach <- data.frame(Station_ID=unique(TR$Station_ID), reach=c(rep("Parsnip",3),rep("Junction",3),
                                                             rep("Finlay",2),rep("Peace",7),
                                                             "Junction","Finlay","Peace","Peace"))
TRshort <- TR %>% 
  mutate(method="TR") %>% 
  left_join(site.reach, by="Station_ID") %>% 
  select(method,year,reach,site=Station_ID,start.date,net.trawl.number=Trawl_num,sp,FL,wt,sex,
         mat=Maturity, age)
  


GNTR <- rbind(GNshort,TRshort) %>% 
  mutate(yearF = as.factor(year)) %>% 
  mutate(k = 100000*(wt/FL^3)) %>% 
  mutate(logFL = log10(FL), logwt = log10(wt)) %>% 
  mutate(lcat10=lencat(FL, w=10)) #categorize age into 10 mm groups




#### FL-wt comparisons #### 
#check for changes in fish condition among years 

(plotlength.weight.rawKO <- ggplot(GNTR[which(GNTR$sp %in% "KO"),])+
  geom_point(aes(x=logFL, y=logwt, col=yearF))+
  #scale_x_continuous(limits = c(2,2.5), breaks = seq(2,2.5,.1))+
  #scale_y_continuous(limits = c(1,2.5), breaks = seq(1,2.5,0.1))+
  facet_wrap(~method))+
  labs(title="KO all length-weights by year")

#find extreme outliers:
GNTR %>% #first fish measured in the Forebay in bad net sets
  filter(method %in% "GN", logFL >2.25 & logwt <1.5)
GNTR %>% #KO caught at Finlay Forks FL 250, wt 110
  filter(method %in% "GN", year %in% 2021, logFL >=2.36 & logwt <2.06)
GNTR %>% #KO caught at Finlay Forks FL 141, wt 70
  filter(method %in% "GN", logFL <2.17 & logwt >1.8)

(plotlength.weightKO.2021 <- ggplot(GNTR[which(GNTR$sp %in% "KO"&GNTR$year %in% 2021),])+
    geom_point(aes(x=logFL, y=logwt, col=method))+
  labs(title="KO all length-weights in 2021 by collection method"))

#it there good overlap between methods (GN and TR) in KO fish condition?

plot.compare.methods <- ggplot(GNTR[which(GNTR$sp %in% c("KO","LW")& GNTR$year %in% 2000),])+
  geom_point(aes(x=logFL, y=logwt, col=method))+
  geom_smooth(aes(x=logFL, y=logwt, col=method), method="lm",se = F)+
  facet_wrap(~sp)+
  labs(title="2000 compare KO and LW caught in GN and TR")
plot.compare.methods


# eject 2021 GN weights for KO and insert predicted weights from TR
GNTR.measuredwts <- GNTR %>% 
  filter(year %in% 2021, method %in% "TR", sp %in% "KO") %>% 
  filter(!is.na(FL), !is.na(wt)) %>% 
  mutate(wt.pred = as.character("measured"))
nrow(GNTR.measuredwts) #85 KO were measured for weight in the trawl

GNTR.predwts <- GNTR %>% 
  filter(year %in% 2021, method %in% "GN", sp %in% "KO") %>% 
  filter(!is.na(FL)) %>% 
  mutate(wt = NA, logwt = NA, wt.pred = as.character("predicted"))
nrow(GNTR.predwts) #138 KO weights need to be predicted


fitflwt.ko21 <- lm(logwt~logFL, data=GNTR.measuredwts) #this is the fit of the 2021 length-weight from trawl
summary(fitflwt.ko21)
par(mfrow = c(2, 2))
plot(fitflwt.ko21) #check for length-wt outliers in residual plot

pred.logwt <- predict(fitflwt.ko21, GNTR.predwts, interval ="prediction") #this is predicting individual fish weights, not average
cf <- logbtcf(fitflwt.ko21, 10)
back.trans <- cf*10^pred.logwt

GNTR.predwts$wt <- as.numeric(back.trans[,1])
GNTR.predwts$pred.wt.lwr <- back.trans[,2]
GNTR.predwts$pred.wt.upr <- back.trans[,3]

GNTRwts <- GNTR.measuredwts %>% 
    full_join(GNTR.predwts) %>% 
  mutate(logwt = log10(wt))

(plotpred.weightKO.2021 <- ggplot(GNTRwts)+
    geom_errorbar(aes(x=logFL, ymin=log10(pred.wt.lwr), 
                      ymax=log10(pred.wt.upr)), col="light blue")+
    geom_point(aes(x=logFL, y=logwt, col=wt.pred))+
    labs(title="KO measured and predicted weights in 2021",
         y="log weight (+/- 95% CI)"))

#viewed together with other years of data:

GNTRwts.allyrsKO <- GNTR %>% 
  filter(year != 2021, sp %in% "KO") %>% 
  mutate(wt.pred = as.character("measured")) %>% 
  full_join(GNTRwts) %>% 
  mutate(k = 100000*(wt/FL^3))


# all weights in here with predicted
ggplot(GNTRwts.allyrsKO)+
  geom_errorbar(aes(x=logFL, ymin=log10(pred.wt.lwr), 
                    ymax=log10(pred.wt.upr)), col="light blue")+
  geom_point(aes(x=logFL, y=logwt, col=yearF))+
  geom_smooth(aes(x=logFL, y=logwt, col=yearF), method="lm")

# only measured weights
(plot.FLwtbyyear <- ggplot(GNTRwts.allyrsKO[which(GNTRwts.allyrsKO$wt.pred %in% "measured"),])+
  geom_errorbar(aes(x=logFL, ymin=log10(pred.wt.lwr), 
                    ymax=log10(pred.wt.upr)), col="light blue")+
  geom_point(aes(x=logFL, y=logwt, col=yearF))+
  geom_smooth(aes(x=logFL, y=logwt, col=yearF), linewidth=0.75,method="lm", se=T)+
  labs(title="Compare weight and fork length by year for Kokanee (measured only)"))

#compare slopes between years (fit1 does not use predicted weights)

fit1.ko <- lm(logwt~logFL*yearF, data=GNTRwts.allyrsKO[which(GNTRwts.allyrsKO$wt.pred %in% "measured"),])
car::Anova(fit1.ko)





#truncate to larger sizes only (>100mm):
#actually instead of using the size cut-off, maybe should use GN v TR?
#In the end I kept the size cut-off since there were only really GN fish over
# this size range, so 2021 would have had to be left out.

GNTRwts.allyrsbigKO <- GNTRwts.allyrsKO %>%
  filter(logFL>2)
# GNTRwts.allyrsbigKO <- GNTRwts.allyrsKO %>% 
#     filter(method %in% "GN")

# only bigger KO (>100mm) - using predicted weights
plot.FLwtbyyearbigpred <- ggplot(GNTRwts.allyrsbigKO)+
    geom_errorbar(aes(x=logFL, ymin=log10(pred.wt.lwr), 
                      ymax=log10(pred.wt.upr)), col="light blue")+
    geom_point(aes(x=logFL, y=logwt, col=yearF))+
    geom_smooth(aes(x=logFL, y=logwt, col=yearF), linewidth=0.75,method="lm", se=T)+
  labs(title="Compare weight and fork length by year for Kokanee >100mm- with Predicted",
       y="logwt (+/- 95CI for predicted points")
plot.FLwtbyyearbigpred

#fit2 uses predicted weights ## shouldn't we generate a range of points within the 95% CI to capture variability in test?
fit2.ko <- lm(logwt~logFL*yearF, data=GNTRwts.allyrsbigKO)
car::Anova(fit2.ko)



# only bigger KO (>100mm) - NOT using predicted weights
(plot.FLwtbyyearbignopred <- ggplot(GNTRwts.allyrsbigKO[which(GNTRwts.allyrsbigKO$wt.pred %in% "measured"),])+
    geom_point(aes(x=logFL, y=logwt, col=yearF))+
    geom_smooth(aes(x=logFL, y=logwt, col=yearF), linewidth=0.75,method="lm", se=T)+
  labs(title="Compare weight and fork length by year for Kokanee >100mm- without Predicted"))


#fit3 does not use predicted weights 
fit3.ko <- lm(logwt~logFL*yearF, data=GNTRwts.allyrsbigKO[which(GNTRwts.allyrsbigKO$wt.pred %in% "measured"),])
car::Anova(fit3.ko)


#### Compare Fulton's K ####


(plot.Kbyagehistogram <-ggplot(GNTRwts.allyrsKO[GNTRwts.allyrsKO$wt.pred %in% "measured",])+
  geom_histogram(aes(x=k, fill=as.factor(age)), col="black", binwidth=0.02)+
  facet_wrap(~yearF, nrow=3))


GNTR.KO.byreach <- GNTRwts.allyrsKO %>% 
  filter(wt.pred %in% "measured", reach %in% "Peace")

ggplot(GNTR.KO.byreach)+
    geom_histogram(aes(x=k, fill=as.factor(age)), col="black", binwidth=0.02)+
    facet_wrap(~yearF, nrow=3)


# ggplot(GNTRwts.allyrsKO[GNTRwts.allyrsKO$wt.pred %in% "measured",])+
#   geom_jitter(aes(x=age, y=k, col=yearF), height=0, width=0.2)+
#   geom_smooth(aes(x=age, y=k, col=yearF), method="loess")

plot.Kagebyyear <- ggplot(GNTRwts.allyrsKO)+
  geom_jitter(aes(x=age, y=k, col=yearF), height=0, width=0.2)+
  geom_smooth(aes(x=age, y=k, col=yearF), method="loess")
plot.Kagebyyear

plot.Kagebyyear.reach <- ggplot(GNTRwts.allyrsKO)+
  geom_jitter(aes(x=age, y=k, col=yearF), height=0, width=0.2)+
  geom_smooth(aes(x=age, y=k, col=yearF), method="loess")+
  facet_wrap(~reach)
plot.Kagebyyear.reach








#### Length at age ####

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
ggplot(GNTR[!is.na(GNTR$FL),])+
  geom_jitter(aes(x=age,y=logFL, col=sp))+
  facet_wrap(~year, nrow=3)
#note that only estimated KO ages avail for 2021



#how many fish were aged in 2000 and 2008?

(aged <- GNTR %>%
  filter(year != 2021) %>%
  group_by(yearF, sp) %>%
  summarize(total = length(sp),aged = length(which(!is.na(age)))))
#looks like LW from 2000 would benefit from some ALK but 2008 aged the majority of their fish

ggplot(GNTR[GNTR$sp %in%"LW"&GNTR$year %in% 2000,])+
  geom_histogram(aes(x=FL,fill=is.na(age)), col="black")

agedLW2000 <- GNTR %>%
  dplyr::filter(year %in% 2000,  sp %in% "LW", !is.na(age)) %>%
  mutate(age.predict = "measured") %>%
  arrange(lcat10)

notagedLW2000 <- GNTR %>%
  dplyr::filter(year %in% 2000,  sp %in% "LW", is.na(age)) %>%
  mutate(age.predict = "predicted") %>%
  arrange(lcat10)

# cannot predict beyond the minimum ages lcat so filter out for predicting ages.
#are there any that are too small to be estimated?
(n.toosmall.prev <- length(which(notagedLW2000$FL < min(agedLW2000$lcat10, na.rm=T))))

(alk.freq <- xtabs(data=agedLW2000, ~lcat10+age))
rowSums(alk.freq)

alk <- prop.table(alk.freq,margin=1)
round(alk,3)

# predict individual ages for unaged sample (Isermann+Knight 2005 method)
notagedLW2000 <- alkIndivAge(alk,age~FL,data=notagedLW2000)
#if this says the max observed length exceeded max aged length, may want to take out of extrapolation

LW2000 <- rbind(agedLW2000,notagedLW2000)
(LW.ALKpredict <- ggplot(LW2000)+
  geom_point(aes(x=age,y=FL, col=age.predict))+
  geom_smooth(aes(x=age,y=FL, col=age.predict), method="loess")+
    labs(title= "Lake Whitefish ages predicted for measured/unaged fish in 2000"))

ggplot(LW2000)+
  geom_point(aes(x=age, y=lcat10, col=mat))

GNTR <- full_join(GNTR[-which(GNTR$year %in% 2000 & GNTR$sp %in% "LW" & is.na(GNTR$age)),],
          notagedLW2000)



#is there a difference between the length at age among years? Kinda pointless for LW
# until there are ages for 2021
#LW
mod1LW <- nnet::multinom(age~lcat10,data=GNTR[which(GNTR$sp %in% "LW"),],maxit=500)
mod2LW <- nnet::multinom(age~lcat10*yearF,data=GNTR[which(GNTR$sp %in% "LW"),],maxit=500)
anova(mod1LW,mod2LW) 

#is there a difference between the length at age among years?
#KO
mod1KO <- nnet::multinom(age~lcat10,data=GNTR[which(GNTR$sp %in% "KO"),],maxit=500)
mod2KO <- nnet::multinom(age~lcat10*yearF,data=GNTR[which(GNTR$sp %in% "KO"),],maxit=500)

anova(mod1KO,mod2KO) #signif difference among years



#KO ages from trawl and gillnet combined:
ggplot(GNTR[which(GNTR$sp %in%"KO"),]) +
  geom_histogram(aes(x=FL, fill=as.factor(age)), binwidth=5,position = "stack",
                 col="black")+
  facet_wrap(~year, nrow=3)+
  labs(title="KO count by age (note 2021 is prelim)")
#some very old kokanee in 2000? No age 0s in 2008 b/c no trawling

#LW ages from trawl and gillnet combined:
ggplot(GNTR[which(GNTR$sp %in%"LW"),]) +
  geom_histogram(aes(x=FL, fill=as.factor(age)), binwidth=5,position = "stack",
                 col="black")+
  facet_wrap(~year, nrow=3)+
  labs(title="LW count by age (note 2021 is prelim)")

#RB ages from trawl and gillnet combined:
ggplot(GNTR[which(GNTR$sp %in%"RB"),]) +
  geom_histogram(aes(x=FL, fill=as.factor(age)), binwidth=5,position = "stack",
                 col="black")+
  facet_wrap(~year, nrow=3)+
  labs(title="RB count by age (note 2021 is prelim)")




# Von Bert curves #### 

vbTyp <- function(age,Linf,K,t0) Linf*(1-exp(-K*(age-t0)))

#LW

#2000
LW2000
( svTyp <- FSA::vbStarts(FL~age,data=LW2000) )
#svTyp <- list(Linf=max(LW2000$FL,na.rm=TRUE),K=0.46,t0=0)


fitTyp <- stats::nls(FL~vbTyp(age,Linf,K,t0),data=LW2000,start=svTyp)

(LW2000.all.coeff <- coef(fitTyp))

confint(fitTyp) #one type of confidence interval...see below for other
#par(mfrow=c(2,2))
# plot(fitTyp,loess = T) # if this plot makes a funnel shape, see Ogle's IFAR book for alternate method

x.prev <- seq(0,max(LW2000$age, na.rm=T),length.out=199)        # ages for prediction
pTyp.prev <- vbTyp(x.prev,Linf=coef(fitTyp)[1], K = coef(fitTyp)[2], t0 = coef(fitTyp)[3])   # predicted lengths

LW2000.df <- data.frame(x.prev, pTyp.prev, yearF = as.factor("2000"))

(label.LW2000 <- paste0("2000",": ",round(LW2000.all.coeff[1],2),
                        "*","(1-exp(-",round(LW2000.all.coeff[2],2),
                        "*","(age - ",round(LW2000.all.coeff[3],2),"))"))


# 2008

#LW
LW2008 <- GNTR %>%
  filter(year %in% 2008, sp %in% "LW")

( svTyp <- vbStarts(FL~age,data=LW2008) )
#svTyp <- list(Linf=max(LW2000$FL,na.rm=TRUE),K=0.46,t0=0)

fitTyp <- nls(FL~vbTyp(age,Linf,K,t0),data=LW2008,start=svTyp)

(LW2008.all.coeff <- coef(fitTyp))

#confint(fitTyp) #did not work
# par(mfrow=c(2,2))
# plot(fitTyp,loess = T) # if this plot makes a funnel shape, see Ogle's IFAR book for alternate method

x.prev <- seq(0,max(LW2008$age, na.rm=T),length.out=199)        # ages for prediction
pTyp.prev <- vbTyp(x.prev,Linf=coef(fitTyp)[1], K = coef(fitTyp)[2], t0 = coef(fitTyp)[3])   # predicted lengths

LW2008.df <- data.frame(x.prev, pTyp.prev, yearF = as.factor("2008"))

# LW 2021
#cannot do LW 2021 b/c no ages yet...

#labels

(label.LW2008 <- paste0("2008",": ",round(LW2008.all.coeff[1],2),
                        "*","(1-exp(-",round(LW2008.all.coeff[2],2),
                        "*","(age - ",round(LW2008.all.coeff[3],2),"))"))


#combine
LW.ages <- rbind(LW2000,LW2008)
LW.ages.vonb <- rbind(LW2000.df,LW2008.df)

#plot together

LWages.plot <- ggplot()+
  geom_jitter(data=LW.ages, aes(x=age, y=FL, col=yearF),width = 0.15,
              size=3, alpha=0.6)+
  geom_text(aes(x=8,y=150, label=label.LW2000))+
  geom_text(aes(x=8,y=125, label=label.LW2008))+
  geom_line(data=LW.ages.vonb, aes(x=x.prev, y=pTyp.prev, col=yearF), linewidth=1)
LWages.plot




#KO (2021 age estimated at the moment)
KO2000 <- GNTR %>%
  filter(year %in% 2000, sp %in% "KO")

( svTyp <- vbStarts(FL~age,data=KO2000) )
#svTyp <- list(Linf=max(LW2000$FL,na.rm=TRUE),K=0.46,t0=0)

vbTyp <- function(age,Linf,K,t0) Linf*(1-exp(-K*(age-t0)))
fitTyp <- nls(FL~vbTyp(age,Linf,K,t0),data=KO2000,start=svTyp)

(KO2000.all.coeff <- coef(fitTyp))

confint(fitTyp) #one type of confidence interval...see below for other
# par(mfrow=c(2,2))
# plot(fitTyp,loess = T) # if this plot makes a funnel shape, see Ogle's IFAR book for alternate method

x.prev <- seq(0,max(KO2000$age, na.rm=T),length.out=199)        # ages for prediction
pTyp.prev <- vbTyp(x.prev,Linf=coef(fitTyp)[1], K = coef(fitTyp)[2], t0 = coef(fitTyp)[3])   # predicted lengths

KO2000.df <- data.frame(x.prev, pTyp.prev,yearF = as.factor("2000"))

(label.KO2000 <- paste0("2000",": ",round(KO2000.all.coeff[1],2),
                        "*","(1-exp(-",round(KO2000.all.coeff[2],2),
                        "*","(age - ",round(KO2000.all.coeff[3],2),"))"))

#KO 2008
KO2008 <- GNTR %>%
  filter(year %in% 2008, sp %in% "KO")

(svTyp <- vbStarts(FL~age,data=KO2008))
fitTyp <- nls(FL~vbTyp(age,Linf,K,t0),data=KO2008,start=svTyp)

(KO2008.all.coeff <- coef(fitTyp))
confint(fitTyp)

# par(mfrow=c(2,2))
# plot(fitTyp,loess = T) # if this plot makes a funnel shape, see Ogle's IFAR book for alternate method

x.prev <- seq(0,max(KO2008$age, na.rm=T),length.out=199)        # ages for prediction
pTyp.prev <- vbTyp(x.prev,Linf=coef(fitTyp)[1], K = coef(fitTyp)[2], t0 = coef(fitTyp)[3])   # predicted lengths

KO2008.df <- data.frame(x.prev, pTyp.prev,yearF = as.factor("2008"))

(label.KO2008 <- paste0("2008",": ",round(KO2008.all.coeff[1],2),
                        "*","(1-exp(-",round(KO2008.all.coeff[2],2),
                        "*","(age - ",round(KO2008.all.coeff[3],2),"))"))


#KO 2021
#KO (2021 age estimated at the moment)
KO2021 <- GNTR %>%
  filter(year %in% 2021, sp %in% "KO")

#( svTyp <- vbStarts(FL~age,data=KO2021) ) #currently does not work - provide with values
svTyp <- list(Linf=max(KO2021$FL,na.rm=TRUE),K=0.3,t0=0)

fitTyp <- nls(FL~vbTyp(age,Linf,K,t0),data=KO2021,start=svTyp)

(KO2021.all.coeff <- coef(fitTyp))

confint(fitTyp) #one type of confidence interval...see below for other
# par(mfrow=c(2,2))
# plot(fitTyp,loess = T) # if this plot makes a funnel shape, see Ogle's IFAR book for alternate method

x.prev <- seq(0,max(KO2021$age, na.rm=T),length.out=199)        # ages for prediction
pTyp.prev <- vbTyp(x.prev,Linf=coef(fitTyp)[1], K = coef(fitTyp)[2], t0 = coef(fitTyp)[3])   # predicted lengths

KO2021.df <- data.frame(x.prev, pTyp.prev, yearF = as.factor("2021"))

(label.KO2021 <- paste0("2021",": ",round(KO2021.all.coeff[1],2),
                        "*","(1-exp(-",round(KO2021.all.coeff[2],2),
                        "*","(age - ",round(KO2021.all.coeff[3],2),"))"))


# combine
KO.ages <- rbind(KO2000,KO2008,KO2021)
KO.ages.vonb <- rbind(KO2000.df,KO2008.df,KO2021.df)

# plot together
KOages.plot <- ggplot()+
  geom_jitter(data=KO.ages, aes(x=age, y=FL, col=yearF),width = 0.15,
              size=3, alpha=0.6)+
  geom_text(aes(x=4,y=100, label=label.KO2000))+
  geom_text(aes(x=4,y=80, label=label.KO2008))+
  geom_text(aes(x=4,y=60, label=label.KO2021))+
  geom_line(data=KO.ages.vonb, aes(x=x.prev, y=pTyp.prev,col=yearF), linewidth=1)
KOages.plot



#### Length comparison - dbpop and fish catch ####

#conversion formula 
x=11.2
log10(x)*23.909-68.216

y=-43.1
10^((y+68.216)/23.909)

dbpop <- read_excel("Dbpop _Williston_2021_27.35dB.xlsx", 
           sheet="Williston Dbpop ", range = c("A5:BE485"),
           col_names = T) %>% 
  mutate(Reach = ifelse(Transect %in% c(1:5),"Parsnip",
                        ifelse(Transect %in% c(6:10,21),"Junction",
                               ifelse(Transect %in% c(11:20),"Finlay",
                                      ifelse(Transect %in% c(22:32),"Peace",NA)))))
str(dbpop)

dbpop.long <- dbpop %>% 
  select(-c(low.end.noise, all.fish,big.fish,fry,onetothree,lg.fish,`NA`, habitat.strata)) %>% 
  pivot_longer(-c(Reach,Transect, Depth), names_to = "db", values_to="tot.fish") 

dbpop.reach <- dbpop.long %>% 
  group_by(Reach, Depth, db) %>% 
  summarize(tot.fish = sum(tot.fish)) %>% 
  mutate(db = as.numeric(db),
         FL = round(10^((db+68.216)/23.909)*10,2),
         lcat10 = lencat(FL, w=10)) %>% 
  filter(FL<= 300)

ggplot(dbpop.reach)+
  geom_bar(aes(x=lcat10, y=tot.fish, fill=Reach), stat="identity")+
  scale_x_continuous(breaks= seq(0,400,25))

GNTR.pelagics <- GNTR %>% 
  filter(sp %in% c("KO","LW","PCC"))
GNTR[which(GNTR$method %in% "GN"),]

ggplot(GNTR[which(GNTR$method %in% "GN"),])+
  geom_histogram(aes(x=lcat10, fill=reach, col=method))+
  scale_x_continuous(limits = c(0,400),breaks= seq(0,400,25))




# #OLD#
# 
# 
# 
# 
# 
# 
# 
# #KO length at age, all together
# ggplot(GNTR[which(GNTR$sp %in%"KO"),])+
#   geom_jitter(aes(x=age,y=FL,col=yearF))
# 
# 
# #condition for kokanee and lake whitefish by year
# ggplot(data=GNTR[which(GNTR$sp %in% c("KO","LW")),]) +
#   geom_histogram(aes(x=k, fill=sp), colour = "black")+
#   geom_vline(xintercept = 1, linetype="dashed")+
#   facet_wrap(~year, nrow=3)+
#   labs(y="Frequency", x="Fulton's k", fill="sp")
# 
# #condition by age for kokanee by year
# ggplot(data=GNTR[which(GNTR$sp %in% c("KO")),]) +
#   geom_histogram(aes(x=k, fill=as.factor(age)), colour = "black")+
#   geom_vline(xintercept = 1, linetype="dashed")+
#   facet_wrap(~year, nrow=3)+
#   labs(y="Frequency", x="Fulton's k", fill="age")
# 
# #remove transient objects
# # rm(fish.GN00,fish.GN21,site.GN00,site.GN21, GN2000,GN2008,GN2021,
# #    GNshort,latitudes,latitudes21,longitudes,longitudes21,log.TR00,TR2000,
# #    TR2021,TR2000short,TRshort,fish.TR00,fish.TR21, locations,locations21)
# 
# 
