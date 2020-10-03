# Created by Adrien Hernandez

# Loading libraries
library(dplyr)
library(tidyr)
library(data.table)

# Reading data
mydata <- read.csv("vehroutedata.csv")

# Turning data into a correct format
Correct_Shape <- function(data_input){
  data_input$real_id <- data_input$id
  data_input$id <- sample(1:300000, nrow(data_input), replace=FALSE)
  data_input$edge <- as.character(data_input$edge)
  data_input$exitTimes <- as.character(data_input$exitTimes)
  
  s <- strsplit(data_input$edge, split = " ")
  s1 <- data.frame(id = rep(data_input$id, sapply(s, length)), edge = unlist(s))
  s <- strsplit(data_input$exitTimes, split = " ")
  s2 <- data.frame(id = rep(data_input$id, sapply(s, length)), exitTimes = unlist(s))
  
  
  Sumo_s1 <- s1 %>% group_by(id)%>%tally()
  Sumo_s2 <- s2 %>% group_by(id)%>%tally()
  clean <- setdiff(Sumo_s1, Sumo_s2)
  data_input <- data_input[!data_input$id %in% clean$id,]
  
  
  data_edge <- data_input %>% 
    separate_rows(edge, sep=" ") 
  
  data_exitTimes <- data_input %>% 
    separate_rows(exitTimes, sep=" ") 
  
  data_edge <- data_edge[data_edge$edge!="",]
  
  data_edge$exitTimes <- data_exitTimes$exitTimes
  data <- data_edge
  
  data$depart <- as.integer(data$depart)
  data$exitTimes <- as.integer(data$exitTimes)
  data$tt <- data$exitTimes - data$depart
  
  data$time <- "2020-07-08 00:00:00"
  data$time <- as.POSIXct(data$time)
  return(data)
}

data <- Correct_Shape(mydata)

# Setting the graph
Setting_graph <- function(data_input){
  data_input <- as.data.table(data_input)
  data_input[, exitTimes2 := exitTimes, by = id]
  data_input[, exitTimes2 := shift(exitTimes2, type = 'lag'), by = id]
  data_input <- as.data.frame(data_input)
  data_input$depart <- ifelse(!is.na(data_input$exitTimes2), data_input$exitTimes2, data_input$depart)
  data_input$tt <- ifelse(!is.na(data_input$exitTimes2), (data_input$exitTimes-data_input$depart), data_input$tt)
  data_input$time <- data_input$time + data_input$depart
  
  data_input <- data_input[data_input$type!="bus",]
  
  data_input$depart <- NULL
  data_input$exitTimes2 <- NULL
  data_input$exitTimes <- NULL
  data_input$X <- NULL
  return(data_input)
}

vehroutedata_cleaned <- Setting_graph(data)

write.csv(vehroutedata_cleaned, "vehroutedata_cleaned.csv")
