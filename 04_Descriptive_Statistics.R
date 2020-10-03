# Created by Adrien Hernandez

library(dplyr)
library(tidyr)
library(data.table)

Sumo_data <- read.csv("Quebec/Sumo_data.csv")

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

