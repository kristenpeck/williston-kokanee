 
### This script analyses data collected during the 2021 Williston Reservoir 
 ### Acoustic Trawl and Gillnetting survey, withsome comparisons to
 ### the 2000 and 2008 surveys

# Author: Kristen Peck
# Date: January 2023


library(tidyverse); citation("tidyverse")
library(lubridate); citation("lubridate")
library(readxl); citation("readxl")
library(sf)
library(mapview)
library(FSA)
library(nnet)
library(car)

options(warn = 1)
mapviewOptions(vector.palette = colorRampPalette(c("grey", "cornflowerblue", "purple")))


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
GN[which(is.na(GN$sp)),c("Site","start.datetime","Net","net.depth","sp","FL")]
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
  filter(net.type %in% "RIC6", sp %in% c("KO","LW")) %>% 
  filter(FL >130 & FL<160) %>% 
  select(year,Reach,Site,net.depth,net.type,sp,FL))
RIC6plot <- ggplot(RIC6.fish)+
  geom_histogram(aes(x=FL, fill=sp),col="black", binwidth=1)+
  facet_wrap(~year,ncol=1)+
  labs(title="RIC6 catch, FL 130-160mm")
RIC6plot
#and which fish are between 130 and 160 mm FL in RIC7 nets?
(RIC7.fish <- GN %>% 
  filter(net.type %in% c("RIC7","RIC7-hole","RIC7-inefficient"),sp %in% c("KO","LW")) %>% 
  filter(FL >130 & FL<160) %>% 
  select(year,Reach,Site,net.depth,net.type,sp,FL))

RIC7plot <- ggplot(RIC7.fish)+
  geom_histogram(aes(x=FL, fill=sp), col="black",binwidth=1)+
  facet_wrap(~year,ncol=1)+
  labs(title="RIC7 catch in FL 130-160mm")
RIC7plot



#in 2008, called station Nabesche but close to Clearwater for our purposes...

GN$Site[which(GN$Site %in% c("Nabesche","Clearwater"))] <- "Clearwater/Nabesche"
unique(GN$Site)

# catch by effort with 130-160mm FL removed from ALL nets

#exclude 2021 forebay, 0 and 25 m teare crk nets
catch.per.effort <- GN %>% 
  filter(Site != "Adams Creek") %>% 
  filter(!(year %in% 2021 & Site %in% "Forebay")) %>% 
  filter(!(year %in% 2021 & Site %in% "Teare Creek" & net.depth %in% c(0, 25))) %>% 
  #filter(net.type %in% c("RIC6","RIC7","RIC7-hole")) %>% #exclude inefficient sets of 2021
  mutate(exclude = ifelse((FL %in% 130:160), "yes","no")) %>% 
  filter(exclude %in% "no") %>%  #exclude FLs between 130 and 160mm
  group_by(year,Reach,Site,net.depth=round(net.depth,0),Net) %>% 
  summarize(start = first(start.datetime),end = first(end.datetime),
            KO=sum(sp%in%"KO"),LW=sum(sp%in%"LW"),PCC=sum(sp%in%"PCC"),LT=sum(sp%in%"LT"),
            RB=sum(sp%in%"RB"),BT=sum(sp%in%"BT")) %>% 
  mutate(soak.time = as.numeric(difftime(end,start,units="hours"))) %>% 
  mutate(cpue.KO=KO/soak.time, cpue.LW = LW/soak.time) %>% 
  mutate(reach.site = paste0(Reach,"-",Site))

#summary of efforts per year

efforts.per.year <- catch.per.effort %>%
  filter(Site %in% c("Teare Creek","Finlay Forks","Blackwater",
                     "Heather Point","Clearwater/Nabesche","Forebay")) %>%
  group_by(Reach,Site,year) %>%
  summarize(no.nets=length(unique(Net)), soak.time=sum((soak.time)),
            KO=sum(KO),LW=sum(LW),PCC=sum(PCC),
            RB=sum(RB),LT=sum(LT),BT=sum(BT))
efforts.per.year


CPUEKOplot <- ggplot(catch.per.effort)+
  geom_col(aes(x=net.depth,y=cpue.KO, fill=as.character(Site)),col="black")+
  coord_flip()+
  scale_x_reverse()+
  facet_grid(rows=vars(year),cols=vars(Reach))+
  labs(x="net depth (m)", fill="", y="KO catch per net hour")
CPUEKOplot

CPUELWplot <- ggplot(catch.per.effort)+
  geom_col(aes(x=net.depth,y=cpue.LW, fill=as.character(Site)), col="black")+
  coord_flip()+
  scale_x_reverse()+
  facet_grid(rows=vars(year),cols=vars(Reach))+
  labs(x="net depth (m)", fill="",y="LW catch per net hour")
CPUELWplot



#summary of efforts per year from 0-10m depth
efforts.per.year.under10 <- catch.per.effort %>% 
  filter(Site %in% c("Teare Creek","Finlay Forks","Blackwater",
                     "Heather Point","Clearwater/Nabesche","Forebay")) %>% 
  filter(net.depth <= 10) %>% 
  group_by(Reach,Site,year) %>% 
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
  filter(Site %in% c("Teare Creek","Finlay Forks","Blackwater",
                     "Heather Point","Clearwater/Nabesche","Forebay")) %>% 
  filter(net.depth <= 10) %>% 
  group_by(Reach,Site, year) %>% 
  summarize(nets= length(unique(Net)),mn.KO = mean(KO),sd.KO = sd(KO), 
            mn.LW = mean(LW), sd.LW = sd(LW),
            mn.soak.time=mean(soak.time),se.KO = sd(KO)/sqrt(length(Net)),
            se.LW = sd(LW)/sqrt(length(Net))) %>% 
  mutate(mn.cpue.KO = mn.KO/mn.soak.time, mn.cpue.LW = mn.LW/mn.soak.time,
         sd.cpue.KO = sd.KO/mn.soak.time, sd.cpue.LW = sd.LW/mn.soak.time,
         se.cpue.KO = se.KO/mn.soak.time, se.cpue.LW = se.LW/mn.soak.time,
         lowerCI.cpue.KO = ifelse((mn.cpue.KO-se.cpue.KO*1.96)<0,0,mn.cpue.KO-se.cpue.KO*1.96), 
         upperCI.cpue.KO = mn.cpue.KO+se.cpue.KO*1.96,
         lowerCI.cpue.LW = ifelse((mn.cpue.LW-se.cpue.LW*1.96)<0,0,mn.cpue.LW-se.cpue.LW*1.96),
         upperCI.cpue.LW = mn.cpue.LW+se.cpue.LW*1.96)
cpue.stats

table.cpue.stats <- cpue.stats %>% 
  select(Reach, Site, Year=year, Nets=nets, `Mean Soak Time (hrs)`=mn.soak.time,
         `Mean CPUE KO`=mn.cpue.KO, `StDev CPUE KO`=sd.cpue.KO,
         `Mean CPUE LW`=mn.cpue.LW,`StDev CPUE LW`=sd.cpue.LW)
table.cpue.stats
#remove Factor Ross site for trend analysis
catch.per.effort <-catch.per.effort[which(catch.per.effort$Site != "Factor Ross"),]

plot.cpue <- ggplot(catch.per.effort)+
  geom_point(aes(x=year-0.25,y=cpue.KO), col="purple", size=2,alpha=0.5) +
  geom_point(aes(x=year+0.25,y=cpue.LW), col="blue", size=2,alpha=0.5) +
  geom_smooth(aes(x=year,y=cpue.KO),method = "lm", se=F, alpha=0.25,col="purple")+
  geom_smooth(aes(x=year,y=cpue.LW),method = "lm", se=F, col="blue",alpha=0.25)+
  facet_wrap(~reach.site)+
  labs(y="CPUE (KO in purple, LW in blue)", x="Year")
plot.cpue




#simple LM regression for year KO:
lm.KOyr <- lm(data=catch.per.effort,formula= cpue.KO~year)
summary(lm.KOyr)
par(mfrow=c(2,2)) #look at residuals
plot(lm.KOyr)


plot.cpue.KO <- ggplot(catch.per.effort)+
  geom_point(aes(x=year,y=cpue.KO, col=Reach), size=2,alpha=0.5) +
  geom_smooth(aes(x=year,y=cpue.KO),method = "lm", se=T, alpha=0.25,col="purple")+
  geom_text(aes(x = min(year)+4,y = max(cpue.KO)), label = paste0("R-squ = ",round(summary(lm.KOyr)$r.squared,2)))+
  geom_text(aes(x = min(year)+4,y = max(cpue.KO)-.1), 
            label = paste0("KOcpue ~ year(",round(summary(lm.KOyr)$coeff[2,1],3),")"))+
  geom_text(aes(x = min(year)+4,y = max(cpue.KO)-.2), label = paste0("p = ",round(summary(lm.KOyr)$coeff[2,4],3)))+
  labs(y="Kokanee CPUE (fish/net hour)", x="")
plot.cpue.KO

# linear regression by site for KO:
lm.KOyrsite <- lm(data=catch.per.effort,formula= cpue.KO~year*Site)
summary(lm.KOyrsite)
par(mfrow=c(2,2)) #look at residuals
plot(lm.KOyrsite)

plot.cpue.KObysite <- ggplot(catch.per.effort[which(catch.per.effort$Site != "Factor Ross"),])+
  geom_point(aes(x=year,y=cpue.KO), col="purple", size=2,alpha=0.5) +
  geom_smooth(aes(x=year,y=cpue.KO),method = "lm", se=F, alpha=0.25,col="purple")+
  facet_wrap(~reach.site)+
  labs(y="Kokanee CPUE (fish/net hour)", x="Year")
plot.cpue.KObysite

#linear regression by reach for KO:
lm.KOyrreach <- lm(data=catch.per.effort,formula= cpue.KO~year*Reach)
summary(lm.KOyrreach)
par(mfrow=c(2,2)) #look at residuals
plot(lm.KOyrreach)

plot.cpue.KObyreach <- ggplot(catch.per.effort[which(catch.per.effort$Site != "Factor Ross"),])+
  geom_point(aes(x=year,y=cpue.KO, col=Site), size=2,alpha=0.5) +
  geom_smooth(aes(x=year,y=cpue.KO),method = "lm", se=F, alpha=0.25,col="purple")+
  facet_wrap(~Reach)+
  labs(y="Kokanee CPUE (fish/net hour)", x="Year")
plot.cpue.KObyreach

#which model is best?
library(AICcmodavg); citation("AICcmodavg")
models <- list(lm.KOyr, lm.KOyrsite, lm.KOyrreach)
model.namesKO <- c('lm.KOyr', 'lm.KOyrsite', 'lm.KOyrreach')

table.KOAIC <- aictab(cand.set = models, modnames = model.namesKO) 
table.KOAIC %>% 
  as_tibble() %>% 
  mutate(model = c("lm(KOcpue ~ year)",
                   "lm(KOcpue ~ year*Reach)",
                    "lm(KOcpue ~ year*Station)"))


#simple linear regression by year for LW
lm.LWyr <- lm(data=catch.per.effort,formula= cpue.LW~year)
summary(lm.LWyr)
par(mfrow=c(2,2))
plot(lm.LWyr)

plot.cpue.LW <- ggplot(catch.per.effort)+
  geom_point(aes(x=year,y=cpue.LW, col=Reach),  size=2,alpha=0.5) +
  geom_smooth(aes(x=year,y=cpue.LW),method = "lm", se=T, col="blue")+
  geom_text(aes(x = max(year)-4,y = max(cpue.LW)), label = paste0("R-squ = ",round(summary(lm.LWyr)$r.squared,2)))+
  geom_text(aes(x = max(year)-4,y = max(cpue.LW)-.1), 
            label = paste0("LWcpue ~ year(",round(summary(lm.LWyr)$coeff[2,1],3),")"))+
  geom_text(aes(x = max(year)-4,y = max(cpue.LW)-.2), label = paste0("p = ",round(summary(lm.LWyr)$coeff[2,4],3)))+
  labs(y="Lake Whitefish CPUE (fish/net hour)", x="Year")
plot.cpue.LW

library(gridExtra)
plot.cpue.KOLW <- arrangeGrob(plot.cpue.KO,plot.cpue.LW)
plot(plot.cpue.KOLW)

# by site*yr for LW:
lm.LWyrsite <- lm(data=catch.per.effort,formula= cpue.LW~year*Site)
summary(lm.LWyrsite)
par(mfrow=c(2,2))
plot(lm.LWyrsite)

plot.cpue.LWbysite <- ggplot(catch.per.effort[which(catch.per.effort$Site != "Factor Ross"),])+
  geom_point(aes(x=year,y=cpue.LW), col="blue", size=2,alpha=0.5) +
  geom_smooth(aes(x=year,y=cpue.LW),method = "lm", se=F, alpha=0.25,col="blue")+
  facet_wrap(~reach.site)+
  labs(y="Lake Whitefish CPUE (fish/net hour)", x="Year")
plot.cpue.LWbysite


# by reach for LW:
lm.LWyrreach <- lm(data=catch.per.effort,formula= cpue.LW~year*Reach)
summary(lm.LWyrreach)
par(mfrow=c(2,2))
plot(lm.LWyrreach)

plot.cpue.LWbyreach <- ggplot(catch.per.effort[which(catch.per.effort$Site != "Factor Ross"),])+
  geom_point(aes(x=year,y=cpue.LW, col=Site), size=2,alpha=0.5) +
  geom_smooth(aes(x=year,y=cpue.LW),method = "lm", se=F, alpha=0.25,col="blue")+
  facet_wrap(~Reach)+
  labs(y="Lake Whitefish CPUE (fish/net hour)", x="Year")
plot.cpue.LWbyreach


#which model is best?
modelsLW <- list(lm.LWyr, lm.LWyrsite, lm.LWyrreach)
model.namesLW <- c('lm.LWyr', 'lm.LWyrsite', 'lm.LWyrreach')

table.LWAIC <- aictab(cand.set = models, modnames = model.namesLW) 
table.LWAIC %>% 
  as_tibble() %>% 
  mutate(model = c("lm(LWcpue ~ year)",
                   "lm(LWcpue ~ year*Reach)",
                   "lm(LWcpue ~ year*Station)"))






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

print(TR.map)
# fix points for clearwater trawls - off for both 2000 and 2021-done, had to guess 


# fix up transects from 2021
transect.loc <- read_excel("Coordinates_2021WillistonLocations.xlsx", sheet="cleaned") %>% 
  mutate(lat.deg = as.numeric(sub("\\s+\\S+$", '', easting))) %>% 
  mutate(lat.minutes = floor(as.numeric(sub("^\\S+\\s+", '', easting)))) %>%
  mutate(lat.seconds = (as.numeric(sub("^\\S+\\s+", '', easting))-lat.minutes)*60) %>% 
  mutate(long.deg= as.numeric(sub("\\s+\\S+$", '',northing))*-1) %>% 
  mutate(long.minutes= floor(as.numeric(sub("^\\S+\\s+", '',northing)))) %>% 
  mutate(long.seconds = (as.numeric(sub("^\\S+\\s+", '', northing))-long.minutes)*60) %>% 
  mutate(Latitude = lat.deg+(lat.minutes/60)+(lat.seconds/3600)) %>% 
  mutate(Longitude = long.deg-(long.minutes/60)-(long.seconds/3600)) %>% 
  select(-c(lat.deg,lat.minutes,lat.seconds,long.deg,long.minutes,long.seconds)) %>% 
  filter(!is.na(Latitude))
str(transect.loc)

transect.lines <- st_as_sf(transect.loc,coords = c("Longitude", "Latitude"), crs=4326) %>%
  filter(type %in% "transect") %>% 
  group_by(label1) %>% 
  dplyr::summarize() %>%
  st_cast("LINESTRING")
  
mapview(transect.lines,
        legend = F,
        col.lines= c("black"))

TR.lines<- TR.pts %>%
  group_by(yearF, Station_ID, Trawl_num) %>%
  dplyr::summarize() %>%
  st_cast("LINESTRING")

TR.map <- mapview(list(TR.pts,TR.lines), 
                  zcol="yearF", 
                  col.lines = c("snow", "grey"),
                  layer.name = "year",
                  legend = list(T, F))



#### GNTR map ####
GNTR.map <- mapview(list(GN.pts,TR.lines), 
                    zcol="yearF", 
                    col.lines = c("snow", "red"),
                    layer.name = "GNyear",
                    legend = list(T, F),
                    lwd=2) 
print(GNTR.map)



manmade.wbdy <- st_read(dsn="./FWA_MANMADE_WATERBODIES_POLY/FWMNMDWTRB_polygon.shp")
watershed50 <- st_read(dsn="./WDIC_WATERBODY_POLY_SVW/WSA_WB_PLY_polygon.shp")
st_cast(watershed50, "POLYGON")

watershed50 <- st_transform(watershed50, crs=st_crs(TR.lines)) 
willi.bbox <- st_bbox(watershed50)

length(watershed50)
williston <- watershed50[which(watershed50$WTRBD_ID %in% "00001PARA"),]

library(leaflet)
willi.base <- leaflet() %>% 
  addProviderTiles("OpenStreetMap", group="OpenStreetMap") %>% 
  addPolygons(data=williston, color="blue")
  
willi.GNTR <- willi.base %>% 
  addMarkers(data=GN.pts) %>% 
  addPolylines(data=TR.lines)
willi.GNTR




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


### KinRev GN and TR caught fish #####
# check briefly if there is a difference between GN and trawl-caught fish in Kin/rev data:
kinrev <- read_excel("KinRev trawl and gillnet database.xlsx", sheet="Database ") %>% 
  mutate(logwt = log10(Weight), logFL = log10(Length)) %>% 
  filter(Yr %in% c(2013,2014)) %>%  #exclude 2013 & 2014 b/ccaught abunch of wood armfish in trawl
  filter(Sp %in% "KO") %>% 
  select(Lake, Yr, Stname, Method, Sp, Mat, Age, FL=Length, wt = Weight, logFL,logwt)

str(kinrev)
range(kinrev$FL, na.rm = T)
table(kinrev$Stname)

kinrev.lgfish <- kinrev %>% 
  filter(Lake %in% "Kinbasket", FL >100)#Yr >= 2010, 
kinrev.recentyrs <- kinrev %>% 
  filter(Lake %in% "Kinbasket")


plot.kinrev1 <- ggplot(kinrev.recentyrs)+
  geom_point(aes(x=logFL, y=logwt, col=Method), alpha =0.1)+
  geom_smooth(aes(x=logFL, y=logwt, col=Method), method="lm", se=T)+
  facet_wrap(~Lake)+
  labs(x="log10(Fork Length)", y="log10(Weight)", title = "KO in Kinbasket 2013 and 2014")
plot.kinrev1
#ggsave(plot.kinrev1, filename = "plot.kinrev1.png", device = "png", width = 6, height=5)

fitflwt.kinrev1 <- lm(logwt~logFL+Method, data=kinrev.recentyrs) #this is the fit of the 2021 length-weight from trawl
summary(fitflwt.kinrev1)
par(mfrow = c(2, 2))
plot(fitflwt.kinrev1) #check for length-wt outliers in residual plot

#only 100mm+ fish:
plot.kinrev2 <- ggplot(kinrev.lgfish)+
  geom_point(aes(x=logFL, y=logwt, col=Method), alpha =0.1)+
  geom_smooth(aes(x=logFL, y=logwt, col=Method), method="lm", se=T)+
  facet_wrap(~Lake)+
  labs(x="log10(Fork Length)", y="log10(Weight)", title = "KO in Kinbasket >100mm, 2013 2014")
plot.kinrev2
#ggsave(plot.kinrev2, filename = "plot.kinrev2.png", device = "png", width = 6, height=5)

fitflwt.kinrev2 <- lm(logwt~logFL+Method, data=kinrev.lgfish) #this is the fit of the 2021 length-weight from trawl
summary(fitflwt.kinrev2)
par(mfrow = c(2,2))
plot(fitflwt.kinrev2) #check for length-wt outliers or leverage in residual plot

#Well... kinda looks like the trawl-caught fish are fatter at large FLs compared to GN-caught fish
# so I shouldn't assume the trawl and gill net have the same relationship. 
# Discarded that component of report but see below for calc of ave wts for BIOMASS calc



## FLs caught by method and year. Point out the FL range that is added by 7th panel

str(GNTR)
unique(GNTR$sp)

RIC7.range <- data.frame(lower= 130, upper=160, method="GN")

GNTR[GNTR$sp %in% c("KO","LW","PCC"),]
plot.FLspmethod <- ggplot(data=GNTR[GNTR$sp %in% c("KO","LW","PCC"),])+
  geom_histogram(aes(x=FL, fill=sp), position="dodge")+
  geom_vline(data=RIC7.range, aes(xintercept=c(lower)))+
  geom_vline(data=RIC7.range, aes(xintercept=c(upper)))+
  facet_grid(year~method)+
  scale_x_continuous(breaks = seq(0,350,50))+
  labs(x="Fork Length (mm)", fill="species")
plot.FLspmethod

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
  labs(y="Log10(Fork Length)", x="Log10(Weight)"))#title="KO all length-weights in 2021 by collection method"))

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


#this is now defunct but will leave:


fitflwt.ko21 <- lm(logwt~logFL, data=GNTR.measuredwts[GNTR.measuredwts$FL>100,]) #
summary(fitflwt.ko21)
par(mfrow = c(2, 2))
plot(fitflwt.ko21) #check for length-wt outliers in residual plot

pred.logwt <- predict(fitflwt.ko21, GNTR.predwts, interval ="prediction") #this is predicting individual fish weights, not average
cf <- logbtcf(fitflwt.ko21, 10)
back.trans <- cf*10^pred.logwt

#totally separate for BIOMASS estimate by TW - need ave weight per FL
#add in the collection method term from the Kinbasket reln:
summary(fitflwt.kinrev2)
fitflwt.kinrev2$coefficients[3] #about -0.0226 slope for GN weights compared with TR weights

fitflwt.koKIN<- lm(logwt~logFL, data=GNTR.measuredwts[GNTR.measuredwts$FL>100,])
summary(fitflwt.koKIN)

#throw out all variability, just looking to calc ave weight at given length using formula
cf <- logbtcf(fitflwt.koKIN, 10)

GNTR.prewts.KINave <- GNTR.predwts %>% 
  mutate(logwt = fitflwt.koKIN$coeff[1]+
           logFL*(fitflwt.koKIN$coeff[2]-as.numeric(fitflwt.kinrev2$coefficients[3]))) %>% 
  mutate(wt = cf*10^logwt) %>% 
  full_join(GNTR.measuredwts)


plotpred.weightKO.2021KIN <- ggplot(GNTR.prewts.KINave)+
    geom_point(aes(x=logFL, y=logwt, col=wt.pred))+
    geom_smooth(aes(x=logFL, y=logwt, col=wt.pred), method="lm")+
    labs(title="KO measured and predicted weights in 2021",
         y="log weight (+/- 95% CI)")
plotpred.weightKO.2021KIN

ggsave(plot = plotpred.weightKO.2021KIN, filename = "plotpred.weightKO.2021KINave.png",
       device="png", width=6, height=5)
write_csv(GNTR.prewts.KINave, "GNTR.predictedwts.KINave.csv")

# check difference between LW and KO FL-wt relationship for BIOMASS
GNTR.LWKO <- GNTR %>% 
  mutate(wt = ifelse(year %in% 2021 & method %in% "GN", NA, wt), logwt = log10(wt),
         k=100000*(wt/FL^3)) %>% 
  filter(sp %in% c("LW","KO"))
GNTR.LWKOlrg <- GNTR.LWKO %>% 
  filter(FL > 100, year %in% c(2000,2008))

plot.LWKOflwtcompare <- ggplot(GNTR.LWKOlrg)+
  geom_point(aes(x=logFL, y=logwt, col=sp))+
  geom_smooth(aes(x=logFL, y=logwt, col=sp), method="lm")+
  facet_grid(~year+method)
plot.LWKOflwtcompare
ggsave(plot.LWKOflwtcompare, filename = "plot.LWKOflwtcompare.png", device = "png",
       width=6, height=5)


lmKOLWlrg <- lm(logwt~logFL*sp, data=GNTR.LWKOlrg)
summary(lmKOLWlrg)

summary(lm(logwt~logFL, data=GNTR.LWKOlrg[GNTR.LWKOlrg$sp %in% "LW",]))
summary(lm(logwt~logFL, data=GNTR.LWKOlrg[GNTR.LWKOlrg$sp %in% "KO",]))


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

# only measured weights - this will be used from now on:
plot.FLwtbyyear <- ggplot(GNTRwts.allyrsKO[which(GNTRwts.allyrsKO$wt.pred %in% "measured"),])+
  #geom_errorbar(aes(x=logFL, ymin=log10(pred.wt.lwr), 
  #                  ymax=log10(pred.wt.upr)), col="light blue")+
  geom_point(aes( x=FL,y=wt, col=yearF))+
  #geom_smooth(aes(x=logFL, y=logwt, col=yearF), linewidth=0.75,method="lm", se=T)+
  labs(y="Weight (g)", x="Fork Length (mm)", col="Year") #title="Compare weight and fork length by year for Kokanee"))
plot.FLwtbyyear

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
# plot.FLwtbyyearbigpred <- ggplot(GNTRwts.allyrsbigKO)+
#     geom_errorbar(aes(x=logFL, ymin=log10(pred.wt.lwr), 
#                       ymax=log10(pred.wt.upr)), col="light blue")+
#     geom_point(aes(x=logFL, y=logwt, col=yearF))+
#     geom_smooth(aes(x=logFL, y=logwt, col=yearF), linewidth=0.75,method="lm", se=T)+
#   labs(title="Compare weight and fork length by year for Kokanee >100mm- with Predicted",
#        y="logwt (+/- 95CI for predicted points")
# plot.FLwtbyyearbigpred

#fit2 uses predicted weights ## shouldn't we generate a range of points within the 95% CI to capture variability in test?
# fit2.ko <- lm(logwt~logFL*yearF, data=GNTRwts.allyrsbigKO)
# car::Anova(fit2.ko)


# only bigger KO (>100mm) - NOT using predicted weights
plot.FLwtbyyearbignopred <- ggplot(GNTRwts.allyrsbigKO[which(GNTRwts.allyrsbigKO$wt.pred %in% "measured"),])+
    geom_point(aes(x=logFL, y=logwt, col=yearF))+
    geom_smooth(aes(x=logFL, y=logwt, col=yearF), linewidth=0.75,method="lm", se=T)+
  labs(x="log10(Fork Length)", y="Log10(Weight)", col="Year") #title="Compare weight and fork length by year for Kokanee >100mm")
plot.FLwtbyyearbignopred

#fit3 does not use predicted weights and fish >100mm
fit3.ko <- lm(logwt~logFL*yearF, data=GNTRwts.allyrsbigKO[which(GNTRwts.allyrsbigKO$wt.pred %in% "measured"),])
car::Anova(fit3.ko)

 levels(GNTRwts.allyrsbigKO$yearF)
 
# Compare 2000 and 2021 Trawl-caught fish only:
 
 str(GNTR)
TR.KO<- TR %>% #measured KO only
  filter(sp %in% "KO") %>%  
  mutate(logFL = log10(FL),logwt = log10(wt), yearF = as.factor(year))
 
plot.FLwtKOinTR <- ggplot(TR.KO)+
   geom_point(aes(x=logFL, y=logwt, col=yearF))+
   geom_smooth(aes(x=logFL, y=logwt, col=yearF), linewidth=0.75,method="lm", se=T)+
   labs(x="log10(Fork Length)", y="Log10(Weight)", col="Year")
plot.FLwtKOinTR 

fit4.ko <- lm(logwt~logFL*yearF, data=TR.KO)
car::Anova(fit4.ko)

 # only bigger KO (>100mm) - NOT using predicted weights
 plot.FLwtbyyearbignopred <- ggplot(GNTRwts.allyrsbigKO[which(GNTRwts.allyrsbigKO$wt.pred %in% "measured"),])+
   geom_point(aes(x=logFL, y=logwt, col=yearF))+
   geom_smooth(aes(x=logFL, y=logwt, col=yearF), linewidth=0.75,method="lm", se=T)+
   labs(x="log10(Fork Length)", y="Log10(Weight)", col="Year") #title="Compare weight and fork length by year for Kokanee >100mm")
 plot.FLwtbyyearbignopred
 
 #fit3 does not use predicted weights and fish >100mm
 fit3.ko <- lm(logwt~logFL*yearF, data=GNTRwts.allyrsbigKO[which(GNTRwts.allyrsbigKO$wt.pred %in% "measured"),])
 car::Anova(fit3.ko)
 
# I am somewhat suspicious of the 2000 L-w relationship here... maybe the gn-caught fish really were RB...



 
  
 #### Compare Fulton's K ####

plot.Kbyagehistogram <-ggplot(GNTRwts.allyrsKO[GNTRwts.allyrsKO$wt.pred %in% "measured",])+
  geom_histogram(aes(x=k, fill=as.factor(age)), col="black", binwidth=0.02)+
  facet_grid(yearF~method)
plot.Kbyagehistogram
 
GNTR.KOmeasured <- GNTRwts.allyrsKO %>% 
  filter(wt.pred %in% "measured", age >0) #exclude age 0s

GNTR.KOmeasured %>% 
  group_by(reach, year) %>% 
  summarize(mn.K = mean(k, na.rm=T), sd = sd(k,na.rm=T))

lmKOcondition.yrreach <- lm(data=GNTR.KOmeasured, k~year*reach)
summary(lmKOcondition.yrreach)

ggplot(GNTR.KOmeasured)+
    geom_histogram(aes(x=k, fill=as.factor(age)), col="black", binwidth=0.02)+
    facet_wrap(~reach+year, nrow=3)

plot.KO.codition <- ggplot(GNTR.KOmeasured)+
  geom_jitter(aes(x=reach, y=k, col=reach), width = .1)+
  geom_boxplot(aes(x=reach, y=k),alpha = 0,varwidth = length(is.na(GNTR.KOmeasured$k)))+
  facet_wrap(~year)+
  labs(title = "KO non-fry condition factor", x="Reach", y="Kokanee (non-fry) condition factor")+
  theme(legend.position = "none")
plot.KO.codition

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

# Issues with this at the moment! ages are estimated still for 2021, 2000 ages are whack

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

anova(mod1KO,mod2KO) #signif difference among years, but 2000 data are whacky

#KO ages from trawl and gillnet combined:
ggplot(GNTR[which(GNTR$sp %in%"KO"),]) +
  geom_histogram(aes(x=FL, fill=as.factor(age)), binwidth=5,position = "stack",
                 col="black")+
  scale_x_continuous(breaks = seq(0,300,20))+
  facet_wrap(~year, nrow=3)+
  labs(title="KO count by age (note 2021 is prelim)", x= "Fork Length (mm)",fill="Age")
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
  scale_x_continuous(breaks=seq(0,13,2))+
  geom_line(data=LW.ages.vonb, aes(x=x.prev, y=pTyp.prev, col=yearF), linewidth=1)+
  labs(x="Age",y="Fork Length (mm)",col="")
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
  geom_line(data=KO.ages.vonb, aes(x=x.prev, y=pTyp.prev,col=yearF), linewidth=1)+
  labs(x="Age",y="Fork Length (mm)",col="")
KOages.plot



#### Length comparison - dbpop and fish catch ####

#conversion formula 
x=11.2
log10(x)*23.909-68.216

y=-32
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
