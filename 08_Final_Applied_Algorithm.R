# Created by Adrien Hernandez

#install.packages("devtools")
#devtools::install_github("melmasri/traveltimeHMM")
devtools::install_github("AdrienHdz/Sumo_Travel_time_estimation/traveltimeCLT")
library(traveltimeHMM)
library(traveltimeCLT)
library(dplyr)
library(tidyr)
library(data.table)


# Loading data
Sumo_data <- read.csv("Quebec_data/Real_data.csv")

Sumo_data$speed <- exp(Sumo_data$logspeed)

# Transforming variables
Sumo_data <- as.data.table(Sumo_data)
Sumo_data$timeBins <- as.character(Sumo_data$timeBins)


# Creating test trips
set.seed(2020)
test.trips.Sumo <- create_test_trips(M = 500, Sumo_data, min.n = 1)

# Splitting data into train and test set
test = Sumo_data[trip %in% test.trips.Sumo]
train = Sumo_data[!trip %in% test.trips.Sumo]

# Counting the the number of trips in each set
print(paste0("Number of trips inside the test set: ", test[, 1, trip][, sum(V1)]))
print(paste0("Number of trips inside the train set: ", train[, 1, trip][, sum(V1)]))
print(paste0("Number of trips in total: ", test[, 1, trip][, sum(V1)] + train[, 1, trip][, sum(V1)]))


# Setting up the rules for our dataset
myrules = list(
  list(start='6:30', end= '9:00', days = 0:6, tag='MorningRush'),
  list(start='15:00', end= '18:00', days = 0:6, tag='EveningRush')
)

mytimebins = c("MorningRush", "EveningRush", "Other")


# We run the travel time estimation method

graph <- graph_traveltimeCLT(data.train = train, L = 2, data.timebins = mytimebins)

ttCLTmodel <- traveltimeCLT(obj.data.train = graph$data.train, obj.graph.stat.full = graph$graph.stat.full, M = 500, bin = "MorningRush", rules = myrules)

ttCLTresults <- predict_traveltimeCLT(obj.traveltime = ttCLTmodel, obj.graph.stat.full = graph$graph.stat.full, data.test = test, bin = "EveningRush", rules = myrules)

