
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



ggplot(long.abun.index)+
  geom_line(aes(x=nyear, y=count, colour=NAME),
            size=2, na.rm=F)+
  geom_point(aes(x=nyear, y=count, colour=NAME), size = 5, shape="x")+
  theme_bw()+
  scale_x_continuous(breaks =seq(min(long.abun$year),max(long.abun$year),1))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        panel.grid.minor = element_blank())+
  scale_y_continuous(breaks=seq(0,max(long.abun$count, na.rm=T), 50000))+
  #geom_label(data = df, aes(x = nyear, y=350000, label=dom), size=3)+
  labs(title = "Index Streams",x="Year", y="Count", colour = "Index \nStream")


ggplot(long.abun.index, aes(x=year, y=count, fill=NAME))+
  geom_bar(position="stack", stat="identity")

ggplot(long.abun.index, aes(x=year, y=count, fill=Watershed))+
  geom_bar(position="stack", stat="identity")+
  theme_bw()+
  #scale_x_continuous(breaks =seq(min(long.abun$year),2022,1))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        panel.grid.minor = element_blank())+
  scale_y_continuous(breaks=seq(0,500000, na.rm=T), 50000)+
  #geom_label(data = df, aes(x = nyear, y=350000, label=dom), size=3)+
  labs(title = "Index Streams",x="Year", y="Count", fill = "Major \nWatershed")




#bar chart:






#length - weight

KO.flwt <- read_excel("fish size data 2020 (for FWCP).xlsx") %>% 
  mutate(k = wt/(FL^3)*100) %>% 
  mutate(logFL = log10(FL*10)) %>% 
  mutate(logwt = log10(wt))

str(KO.flwt)

ggplot(KO.flwt)+
  geom_point(aes(x=logFL, y=logwt)) +
  geom_smooth(aes(x=logFL, y=logwt), method="lm")

fitflwt <- lm(logwt~logFL, data=KO.flwt)
summary(fitflwt)

ggplot(KO.flwt)+
  geom_point(aes(x=FL, y=k))



