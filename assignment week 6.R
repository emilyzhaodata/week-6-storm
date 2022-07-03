
library(tidyr)
library(dplyr)
library(readxl)
library(readxlsb)
library(DT)
library(writexl)
library(stringr)
library(ggplot2)

#1
Stormevents <- read.csv("C:\\Users\\emily.zhao\\Documents\\Stormdata_1996.csv")
#2
Stormevents <-Stormevents%>%select(BEGIN_DAY,END_DAY,EPISODE_ID,EVENT_ID,STATE,STATE_FIPS,CZ_NAME,CZ_TYPE,CZ_FIPS,EVENT_TYPE,SOURCE,BEGIN_LAT,BEGIN_LON,END_LAT,END_LON)
#3
Stormevents <-Stormevents[order(Stormevents$STATE),]
#4
Stormevents$STATE <- str_to_title(Stormevents$STATE)
#5
Stormevents$CZ_NAME <- str_to_title(Stormevents$CZ_NAME)
Stormevents<-Stormevents%>%filter(CZ_TYPE== "C")
Stormevents<-Stormevents%>%subset(select = -c(CZ_TYPE) )
#6
Stormevents$STATE_FIPS1 <- paste0("0", Stormevents$STATE_FIPS)
Stormevents$CZ_FIPS1 <- paste0("0", Stormevents$CZ_FIPS)
Stormevents$FIP<- str_c(Stormevents$STATE_FIPS1,Stormevents$CZ_FIPS1)
#7
Stormevents<-Stormevents %>%
  rename_with(str_to_title)
#8
state<-data("state")
dxy <- data.frame(state.name, state.area,state.region)%>%rename(region=state.region)
#9
Stormevents_count<-Stormevents%>%group_by(State)%>% summarise(events = n())
join<-left_join(dxy,Stormevents_count,by = c("state.name"="State"))
#10s
plot<-ggplot(join, aes(x = state.area, y = events)) +
  geom_point(aes(color = region))

plot +
  xlab("Land area(square miles)") +
  ylab("# of storm events in 1996")
