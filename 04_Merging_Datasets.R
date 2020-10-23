# Created by Adrien Hernandez, Audrey-Anne Guindon and Mohamad Elmasri

library(dplyr)
library(tidyr)
library(data.table)
#install.packages("devtools")
#devtools::install_github("melmasri/traveltimeHMM")
library(traveltimeHMM)
library(ggplot2)

edges <- read.csv("Quebec_data_2/edges.csv")
vehrouteData <- read.csv("Quebec_data_2/vehroutedata_clean.csv")

## Format edges and keep edge ID and edge length
edges_file <- function(edges){
  edges %>%
  mutate(
    edge = gsub(":", "", edge_id),
  ) %>%
  select(edge, length)%>%
  arrange(edge, length) %>%
  group_by(edge)%>%
  summarise(length = first(length))
}

edges <- edges_file(edges)

# Cleaning \t in vehroudata

## Join edges to trip data to obtain edge length and rename columns
join_edges2trip <-function(data){
  data$edge <- gsub("\t", "", data$edge)
  vehrouteData %>%
  left_join(edges, by ="edge") %>%
  arrange(id, time) %>%
  mutate(
    traveltime = ifelse(tt==0, 0.5, tt),
    tripID = id,
    linkID = gsub("\t", "", edge),
    logspeed = log(length/traveltime),
    timeBin = 0,
    speedkmh = (length/traveltime)*3.6,
    time = gsub("\t", "", time)
  ) %>%
  select(tripID, linkID, timeBin, logspeed, speedkmh, traveltime, length, time)
}  

Sumo_data <- join_edges2trip(vehrouteData)

## Add time bins
rules = list(
  list(start='7:00', end= '10:00', days = 0:6, tag='MR'),
  list(start='12:00', end='15:00', days = 0:6, tag='NR'),
  list(start='17:00', end= '20:00', days = 0:6, tag='ER')
  #list(start='6:30', end= '9:00', days = 0:6, tag='MR'),
  #list(start='15:00', end= '18:00', days = 0:6, tag='ER')
)
time_bins <- rules2timebins(rules)
Sumo_data$timeBin = time_bins(Sumo_data$time)

# Creating column speed
Sumo_data$speed <- exp(Sumo_data$logspeed)

# We remove trips whose one of the link speed is > 150
Sumo_data <- as.data.table(Sumo_data)
Sumo_data[, whour :=as.POSIXlt(time)$wday*24 + as.POSIXlt(time)$hour]
Sumo_data[, week :=week(time)]
r = Sumo_data[3.6*exp(logspeed)>150][, tripID[1],tripID][, tripID]
Sumo_data= Sumo_data[!tripID %in% r][order(tripID, time)]

# We remove the last link from each trip because the cars never end up their trip at the same time
#Sumo_data = Sumo_data[,.SD[-.N] , tripID]
#Sumo_data = Sumo_data[,.SD[-1] , tripID]
Sumo_data = Sumo_data[order(tripID, time)]

# We rename the columns to get the correct names for our method
Sumo_data <- as.data.frame(Sumo_data)
Sumo_data <- rename(Sumo_data, c("trip"="tripID", "timeBins"="timeBin", "linkId"="linkID", "tt"="traveltime"))

write.csv(Sumo_data, "Quebec_data_2/Sumo_data.csv")
