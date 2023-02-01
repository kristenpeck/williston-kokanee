 
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
  mutate(yearF = as.factor(year)) %>% 
  mutate(soak.time = as.numeric(difftime(end.datetime,start.datetime, units="hours"))) %>% 
  mutate(Reach = recode(Site, "Forebay"="Peace", "Clearwater"="Peace",
                        "Nabesche"="Peace", "Adams Creek"="Peace",
                        "Finlay Forks"="Junction", "Teare Creek"="Finlay",
                        "Factor Ross"="Finlay","Blackwater"="Parsnip",
                        "Heather Point"="Parsnip"))
# summary of catch per year per site and depth.
xtabs(~sp+net.depth+Site+year, data=GN, na.action=na.omit,drop.unused.levels = T)
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
  filter(net.type %in% c("RIC6","RIC7","RIC7-hole")) %>% #exclude inefficient sets of 2021
  filter(FL < 130 | FL > 160) %>% #exclude FLs between130 and 160mm
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
  geom_point(aes(x=year,y=mn.cpue.KO), col="grey25") +
  geom_errorbar(aes(x=year,
                    ymin=mn.cpue.KO-CI.cpue.KO,ymax=mn.cpue.KO+CI.cpue.KO), 
                width=0.4)+
  geom_point(aes(x=year,y=mn.cpue.LW), col="blue") +
  geom_errorbar(aes(x=year,
                    ymin=mn.cpue.LW-CI.cpue.LW,ymax=mn.cpue.LW+CI.cpue.LW), 
                width=0.4, col="blue")+
  geom_smooth(aes(x=year,y=mn.cpue.KO),method = "lm", se=F, col="grey25")+
  geom_smooth(aes(x=year,y=mn.cpue.LW),method = "lm",se = F, col="blue")+
  facet_wrap(~Site)+
  labs(y="Mean CPUE (KO in black, LW in blue)")
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
  mutate(sp=recode(sp,"Kokanee"="KO","Lake Whitefish"="LW",
                   "Minnow (General)"="C")) %>% 
  mutate(Maturity=recode(Maturity,"Immature"="IM")) %>% 
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

print(TR.map)
# fix points for clearwater trawls - off for both 2000 and 2021


#### GNTR map ####
GNTR.map <- mapview(list(GN.pts,TR.lines), 
                    zcol="yearF", 
                    col.lines = c("snow", "grey"),
                    layer.name = "GNyear",
                    legend = list(T, F),
                    lwd=2) 
print(GNTR.map)




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














#### Combine all GN and TR catch ####

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
  mutate(k = 100000*(wt/FL^3)) %>% 
  mutate(logFL = log10(FL), logwt = log10(wt))


#### Fish comparisons #### 
#check for changes in fish condition among years and changes in length at age

(plotlength.weight.rawKO <- ggplot(GNTR[which(GNTR$sp %in% "KO"),])+
  geom_point(aes(x=logFL, y=logwt, col=yearF))+
  #scale_x_continuous(limits = c(2,2.5), breaks = seq(2,2.5,.1))+
  #scale_y_continuous(limits = c(1,2.5), breaks = seq(1,2.5,0.1))+
  facet_wrap(~method))+
  labs(title="KO all length-weights by year")

#find outliers:
GNTR %>% #first fish measured nthe Forebay in bad net sets
  filter(method %in% "GN", logFL >2.25 & logwt <1.5)
GNTR %>% #KO caught at Finlay Forks FL 250, wt 110
  filter(method %in% "GN", year %in% 2021, logFL >=2.36 & logwt <2.06)
GNTR %>% #KO caught at Finlay Forks FL 141, wt 70
  filter(method %in% "GN", logFL <2.17 & logwt >1.8)

(plotlength.weightKO.2021 <- ggplot(GNTR[which(GNTR$sp %in% "KO"&GNTR$year %in% 2021),])+
    geom_point(aes(x=logFL, y=logwt, col=method))+
  labs(title="KO all length-weights in 2021 by collection method"))



#GNTR[which(GNTR$sp %in% "KO"),]

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
    labs(title="KO measured and predicted weights in 2021"))

#viewed together with other years of data:

GNTRwts.allyrs <- GNTR %>% 
  filter(year != 2021, sp %in% "KO") %>% 
  mutate(wt.pred = as.character("measured")) %>% 
  full_join(GNTRwts)

GNTRwts.allyrs[which(GNTRwts.allyrs$wt.pred %in% "measured"),]

ggplot(GNTRwts.allyrs[which(GNTRwts.allyrs$wt.pred %in% "measured"),])+
  geom_errorbar(aes(x=logFL, ymin=log10(pred.wt.lwr), 
                    ymax=log10(pred.wt.upr)), col="light blue")+
  geom_point(aes(x=logFL, y=logwt, col=yearF))+
  geom_smooth(aes(x=logFL, y=logwt, col=yearF), method="lm")


#compare slopes between years (fit1 does not use predicted weights)
library(car)

fit1.ko <- lm(logwt~logFL*yearF, data=GNTRwts.allyrs[which(GNTRwts.allyrs$wt.pred %in% "measured"),])
fit2.ko <- lm(logwt~logFL*yearF, data=GNTRwts.allyrs)
car::Anova(fit1.ko)
car::Anova(fit2.ko)





# length at age 

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


#KO ages from trawl and gillnet combined:
ggplot(GNTR[which(GNTR$sp %in%"KO"),]) +
  geom_histogram(aes(x=FL, fill=as.factor(age)), binwidth=5,position = "stack")+
  facet_wrap(~year, nrow=3)
#some very old kokanee in 2000? No age 0s in 2008 b/c no trawling

#length at age, all together
ggplot(GNTR[which(GNTR$sp %in%"KO"),])+
  geom_jitter(aes(x=age,y=FL,col=yearF))


#condition for kokanee and lake whitefish by year
ggplot(data=GNTR[which(GNTR$sp %in% c("KO","LW")),]) +
  geom_histogram(aes(x=k, fill=sp), colour = "black")+
  geom_vline(xintercept = 1, linetype="dashed")+
  facet_wrap(~year, nrow=3)+
  labs(y="Frequency", x="Fulton's k", fill="sp")
  
#condition by age for kokanee by year
ggplot(data=GNTR[which(GNTR$sp %in% c("KO")),]) +
  geom_histogram(aes(x=k, fill=as.factor(age)), colour = "black")+
  geom_vline(xintercept = 1, linetype="dashed")+
  facet_wrap(~year, nrow=3)+
  labs(y="Frequency", x="Fulton's k", fill="age")

#remove transient objects
rm(fish.GN00,fish.GN21,site.GN00,site.GN21, exclude, GN2000,GN2008,GN2021,
   GNshort,latitudes,latitudes21,longitudes,longitudes21,log.TR00,TR2000,
   TR2021,TR2000short,TRshort,fish.TR00,fish.TR21, locations,locations21)


