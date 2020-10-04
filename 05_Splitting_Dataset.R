# Created by Adrien Hernandez and Mohamad Elmasri
install.packages("devtools")
devtools::install_github("melmasri/traveltimeHMM")
library(traveltimeHMM)
library(dplyr)
library(tidyr)
library(data.table)

Sumo_data <- read.csv("Quebec/Sumo_data.csv")

Sumo_data <- as.data.table(Sumo_data)
Sumo_data$timeBins <- as.character(Sumo_data$timeBins)

unique_trip <- Sumo_data %>%
  group_by(trip) %>%
  filter(n() == 1)

Sumo_data <- Sumo_data[!Sumo_data$trip %in% unique_trip$trip,]

# We create a test set of 2000 trips
# The function that allows to create the test set 
create_test_trips<-function(M = 500, db, min.n = 1){
  ## Setting the graph
  db[, linkId.from := linkId, by = trip]
  db[, linkId.to := shift(linkId, type = 'lead'), by = trip]
  db[, N:=.N, by =list(linkId.from, linkId.to, timeBins) ]
  A = db[, .(N = N[1]), by =list(linkId.from, linkId.to, timeBins) ]
  A[, N.left := 0 ] 
  setkey(A, linkId.from, linkId.to, timeBins)
  
  bank = c()
  unique.trips =db[, min(N>1)  , trip][V1 ==1][, trip]
  
  repeat{
    n = length(unique.trips)
    car = unique.trips[sample.int(n,1)]
    k = db[trip==car, .(linkId.from, linkId.to, timeBins)]
    
    if(A[k, all(N -N.left - 1> 0 )]){
      bank = c(bank,car)              ## add the trip
      unique.trips = setdiff(unique.trips, bank) ## removing all smapled trips
      ## updating A
      A[k, N.left:= N.left + 1]
      ## break if number of samples reached
      if(length(bank) >=M)
        break
    }
    
  }
  bank
}

set.seed(2020)
test.trips.Sumo <- create_test_trips(M = 2000, Sumo_data)

test = Sumo_data[trip %in% test.trips.Sumo]
train = Sumo_data[!trip %in% test.trips.Sumo]

test[, 1, trip][, sum(V1)]; train[, 1, trip][, sum(V1)]

test[, 1, trip][, sum(V1)] + train[, 1, trip][, sum(V1)]
