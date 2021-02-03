
#This script is to create visualizations of KO escapement counts around Williston

library(tidyverse)
library(readxl)



KO.abundance <- read_excel("Table B3+B4 Abundance 2002-2020 (for FWCP)_KP.xlsx", 
                           sheet="Summary Sheet",n_max = 32)



index.streams <- KO.abundance %>% 
  filter(index %in% "y") 

long.abun <- gather(KO.abundance, "year", "count", 4:30) %>% 
  filter(year %in% c(2002:2020)) %>% 
  mutate(nyear = as.numeric(year)) %>% 
  mutate(prepost = ifelse(nyear <= 2010, "mon",
                          ifelse(nyear >=2018, "mon", NA)))

long.abun.index <- gather(index.streams, "year", "count", 4:30) %>% 
  filter(year %in% c(2002:2020)) %>% 
  mutate(nyear = as.numeric(year)) %>% 
  mutate(prepost = ifelse(nyear <= 2010, "mon",
                          ifelse(nyear >=2018, "mon", NA))) %>% 
  mutate(dominant = ifelse(nyear %in% c(2006,2010,2014,2018), "d",NA))




ggplot(long.abun)+
  geom_jitter(aes(x=nyear, y=count, colour = Watershed, shape=index),height=0,
              width=.2, size=3)+
  theme_bw()+
  scale_x_continuous(breaks=seq(min(long.abun$year),max(long.abun$year),1))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        panel.grid.minor = element_blank())+
  scale_y_continuous(breaks=seq(0,max(long.abun$count, na.rm=T), 50000))+
  labs(title="All Streams", x="Year", y="Count", colour = "Watershed \nGroup", 
       shape="Index \nStream")



ggplot(long.abun)+
  geom_line(aes(x=nyear, y=count, colour = NAME),size=2)+
  theme_bw()+
  scale_x_continuous(breaks=seq(min(long.abun$year),max(long.abun$year),1))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        panel.grid.minor = element_blank())+
  scale_y_continuous(breaks=seq(0,max(long.abun$count, na.rm=T), 50000))+
  labs(title="All Streams", x="Year", y="Count", colour = "Watershed \nGroup", 
       shape="Index \nStream")

df <- data.frame(nyear = c(2002:2020))
df$dom <- ifelse(df$nyear %in% c(2002,2006,2010,2014,2018), "d",NA)


ggplot(long.abun.index)+
  geom_line(aes(x=nyear, y=count, colour=NAME),
            size=1, na.rm=F)+
  geom_point(aes(x=nyear, y=count, colour=NAME), size = 2)+
  theme_bw()+
  scale_x_continuous(breaks =seq(min(long.abun$year),max(long.abun$year),1))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        panel.grid.minor = element_blank())+
  scale_y_continuous(breaks=seq(0,max(long.abun$count, na.rm=T), 50000))+
  #geom_label(data = df, aes(x = nyear, y=350000, label=dom), size=3)+
  labs(title = "Index Streams",x="Year", y="Count", colour = "Index \nStream")


ggplot(long.abun)+
  geom_line(aes(x=nyear, y=count, colour=NAME),
            size=1, na.rm=F)+
  geom_point(aes(x=nyear, y=count, colour=NAME), size = 2)+
  theme_bw()+
  scale_x_continuous(limits = c(2018,2020),
                     breaks =seq(min(long.abun$year),max(long.abun$year),1))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        panel.grid.minor = element_blank())+
  scale_y_continuous(limits = c(0,50000,10000),
                     breaks=seq(0,max(long.abun$count, na.rm=T), 10000))+
  #geom_label(data = df, aes(x = nyear, y=350000, label=dom), size=3)+
  labs(title = "Index Streams",x="Year", y="Count", colour = "Stream")


#bar chart:

ggplot(long.abun.index, aes(x=year, y=count, fill=NAME))+
  geom_bar(position="stack", stat="identity")


(bar.watershed <- ggplot(long.abun.index, aes(x=year, y=count/1000, fill=Watershed))+
  geom_bar(position="stack", stat="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size=12), 
        panel.grid.minor = element_blank())+
  scale_y_continuous(breaks=seq(0,600, 100))+
  labs(title = "Index Streams Only",x="Year", y="Count (X1000)", 
       fill = "Major\nWatershed"))

ggsave(plot = bar.watershed, filename = "bar.watershed.png", width = 7, height = 4)









#length - weight

KO.flwt <- read_excel("fish size data 2020 (for FWCP)_KP.xlsx") %>% 
  mutate(k = wt/(FL^3)*100) %>% 
  mutate(logFL = log10(FL*10)) %>% 
  mutate(logwt = log10(wt))

str(KO.flwt)

ggplot(KO.flwt)+
  geom_point(aes(x=logFL, y=logwt, colour=sex)) +
  geom_smooth(aes(x=logFL, y=logwt), method="lm")

fitflwt <- lm(logwt~logFL, data=KO.flwt)
summary(fitflwt)

ggplot(KO.flwt)+
  geom_point(aes(x=FL, y=k, colour=sex))

ggplot(KO.flwt)+
  geom_point(aes(x=FL, y=k, colour=Stream))+
  geom_smooth(aes(x=FL, y=k, colour=Stream), method="lm")


# Map

library(sf)
library(mapview)
library(bcmaps)



KO.uppermost <- read_excel("Uppermost KO Observations + barriers 2002-2020 (for FWCP).xlsx", 
                          skip=1, na = c("No Survey","No Fish", "No Data", "No data","Not Surveyed"))
KO.uppermost

long.uppermost <-gather(KO.uppermost, "year", "UTMs", 2:9)

barriers <- long.uppermost %>% 
  filter(year %in% c("barrier location","barrier type"))

UTM9 <- long.uppermost %>%
  mutate(zone = substring(UTMs,1,2)) %>% 
  dplyr::filter(zone %in% "9U") %>% 
  mutate(zone = 9, UTME = substring(UTMs,4,9), UTMN = substring(UTMs, 11,17))
  
UTM10 <- long.uppermost %>%
  mutate(zone = substring(UTMs,1,2)) %>% 
  dplyr::filter(zone %in% "10") %>% 
  mutate(zone = 10, UTME = substring(UTMs,5,10), UTMN = substring(UTMs, 12,18))


locations9 <- UTM9 %>% 
  st_as_sf(coords = c("UTME", "UTMN"), 
           crs = 3156) #for zone 9
  
locations10 <- UTM10 %>% 
  st_as_sf(coords = c("UTME", "UTMN"), 
           crs = 3157) #for zone 10
unique(locations10$stream)

Pelly <- locations10 %>% 
  filter(stream %in% c("Pelly C","Pelly Lk outlet (Zygadene Creek)"))



mapview(Pelly, cex=4, lwd=1, legend=F) %>% 
  leafem::addStaticLabels(label = Pelly$year,
                          noHide = F,
                          direction = 'top',
                          textOnly = TRUE,
                          textsize = "20px")

Manson <- locations10 %>% 
  filter(stream %in% c("Lower Manson R (below lakes)",
                       "Upper Manson R (above lakes)"))

mapview(Manson, cex=4, lwd=1, legend=F) %>% 
  leafem::addStaticLabels(label = Manson$year,
                          noHide = F,
                          direction = 'top',
                          textOnly = TRUE,
                          textsize = "20px")

# ha ha... the data just has so many wrong points, I can't do much here...
