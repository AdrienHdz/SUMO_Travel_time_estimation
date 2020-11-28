# Created by Adrien Hernandez

library(dplyr)
library(tidyr)
library(data.table)

Sumo_data <- read.csv("Quebec_data/Sumo_data.csv")

Sumo_data <- as.data.frame(Sumo_data)
Sumo_trips <- Sumo_data %>% group_by(trip)%>%tally()
Sumo_trips_AM <- Sumo_data[Sumo_data$timeBin=="MR",] %>% group_by(trip)%>%tally()
Sumo_trips_PM <- Sumo_data[Sumo_data$timeBin=="ER",] %>% group_by(trip)%>%tally()
Sumo_trips_NR <- Sumo_data[Sumo_data$timeBin=="NR",] %>% group_by(trip)%>%tally()

Sumo_trips_Other <- Sumo_data[Sumo_data$timeBin=="Other",] %>% group_by(trip)%>%tally()
Sumo_links <- Sumo_data %>% group_by(linkId)%>%tally()

message("There are ", nrow(Sumo_trips), " trips in total")
message(nrow(Sumo_trips_AM), " trips in the morning rush")
message(nrow(Sumo_trips_PM), " trips in the evening rush")
message(nrow(Sumo_trips_NR), " trips in the noon rush")
message(nrow(Sumo_trips_Other), " trips otherwise")

summary(Sumo_trips$n)
message("The shortest trip has ", min(Sumo_trips$n), " edges")
message("The longest trip has ", max(Sumo_trips$n), " edges")

Sumo_data_MR <- Sumo_data[Sumo_data$timeBins=="MR",]
Sumo_data_ER <- Sumo_data[Sumo_data$timeBins=="ER",]
Sumo_data_other <- Sumo_data[Sumo_data$timeBins=="Other",]

speed_mean <- aggregate(Sumo_data$speedkmh, list(Sumo_data$trip), mean)
summary(speed_mean$x)
speed_mean_MR <- aggregate(Sumo_data_MR$speedkmh, list(Sumo_data_MR$trip), mean)
summary(speed_mean_MR$x)
speed_mean_ER <- aggregate(Sumo_data_ER$speedkmh, list(Sumo_data_ER$trip), mean)
summary(speed_mean_ER$x)
speed_mean_other <- aggregate(Sumo_data_other$speedkmh, list(Sumo_data_other$trip), mean)
summary(speed_mean_other$x)


speed_sd <- aggregate(Sumo_data$speedkmh, list(Sumo_data$trip), sd)
summary(speed_sd$x)
speed_sd_AM <- aggregate(Sumo_data_AM$speedkmh, list(Sumo_data_AM$trip), sd)
summary(speed_sd_AM$x)
speed_sd_ER <- aggregate(Sumo_data_ER$speedkmh, list(Sumo_data_ER$trip), sd)
summary(speed_sd_ER$x)
speed_sd_other <- aggregate(Sumo_data_other$speedkmh, list(Sumo_data_other$trip), sd)
summary(speed_sd_other$x)

tt_trajet <- aggregate(Sumo_data$tt, list(Sumo_data$trip), sum)
summary(tt_trajet$x)/60
tt_trajet_MR <- aggregate(Sumo_data_MR$tt, list(Sumo_data_MR$trip), sum)
summary(tt_trajet_MR$x)/60
tt_trajet_ER <- aggregate(Sumo_data_ER$tt, list(Sumo_data_ER$trip), sum)
summary(tt_trajet_ER$x)/60
tt_trajet_other <- aggregate(Sumo_data_other$tt, list(Sumo_data_other$trip), sum)
summary(tt_trajet_other$x)/60

len_trajet <- aggregate(Sumo_data$length, list(Sumo_data$trip), sum)
summary(len_trajet$x)/1000
len_trajet_MR <- aggregate(Sumo_data_MR$length, list(Sumo_data_MR$trip), sum)
summary(len_trajet_MR$x)/1000
len_trajet_ER <- aggregate(Sumo_data_ER$length, list(Sumo_data_ER$trip), sum)
summary(len_trajet_ER$x)/1000
len_trajet_other <- aggregate(Sumo_data_other$length, list(Sumo_data_other$trip), sum)
summary(len_trajet_other$x)/1000


Sumo_data <- as.data.frame(Sumo_data)
Sumo_links <- Sumo_data %>% group_by(linkId)%>%tally()
Sumo_links_AM <- Sumo_data[Sumo_data$timeBin=="MR",] %>% group_by(linkId)%>%tally()
Sumo_links_PM <- Sumo_data[Sumo_data$timeBin=="ER",] %>% group_by(linkId)%>%tally()
Sumo_links_NR <- Sumo_data[Sumo_data$timeBin=="Other",] %>% group_by(linkId)%>%tally()
summary(Sumo_links$n)
