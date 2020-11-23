# Created by Adrien Hernandez

#install.packages("devtools")
#devtools::install_github("melmasri/traveltimeHMM")
devtools::install_github("AdrienHdz/traveltimeCLT")
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
  list(start='6:30', end= '9:00', days = 0:6, tag='MR'),
  list(start='15:00', end= '18:00', days = 0:6, tag='ER')
  #list(start='7:00', end= '10:00', days = 0:6, tag='MR'),
  #list(start='12:00', end='15:00', days = 0:6, tag='NR'),
  #list(start='17:00', end= '20:00', days = 0:6, tag='ER')
)

mytimebins = c("MR", "ER", "Other")


# We run the travel time estimation method

ttCLTmodel <- traveltimeCLT(data.train = train, 
                            M = 1000,
                            L = 2,
                            bin = "MR",
                            rules = myrules,
                            data.timebins = mytimebins)

ttCLTresults <- predict_traveltimeCLT(ttCLTmodel, 
                                      test, 
                                      "MR", 
                                      myrules)

