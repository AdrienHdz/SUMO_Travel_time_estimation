# Created by Adrien Hernandez

library(ggplot2)
library(splitstackshape)

Sumo_data <- read.csv("Quebec_data/Sumo_data.csv")

# Visualization of the traffic density over 24h

Sumo_data_time <- data.frame(Data=as.POSIXct(Sumo_data$time, format="%Y-%m-%d %H:%M:%S"))

ggplot(Sumo_data_time, aes(x=Data)) + 
  geom_histogram(binwidth=100, fill="grey", colour="black") + #binwidth in seconds
  ylab("Nombre de voitures") + 
  xlab('heure de la journée')+#xlab should pick up variable name 'Data'
  ggtitle("Nombre de voitures présentes dans le réseau")

# Visualization of the variation of the speed for a sample of trip
Sumo_data <- getanID(data = Sumo_data, id.vars = "trip")
set.seed(2)
sample_plot <- unique(Sumo_data$trip[Sumo_data$timeBins=="MR"])
sample_plot <- sample(sample_plot, 5)
Sumo_data_sample_plot <- Sumo_data[Sumo_data$trip %in% sample_plot]
Sumo_data_sample_plot$trip <- as.factor(Sumo_data_sample_plot$trip)
Sumo_data_sample_plot <- rename(Sumo_data_sample_plot, c("nbr_edge" = ".id"))


ggplot(Sumo_data_sample_plot, aes(x = Sumo_data_sample_plot$nbr_edge, y = Sumo_data_sample_plot$speedkmh, group = Sumo_data_sample_plot$trip, color = Sumo_data_sample_plot$trip)) + geom_line() + geom_point() + scale_colour_discrete(drop=TRUE,
                                                                                                                                                                                                                                        limits = levels(Sumo_data_sample_plot$trip))
