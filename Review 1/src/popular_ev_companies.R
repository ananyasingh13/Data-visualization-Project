library(tidyverse)
library(ggplot2)
evs <- read.csv("/home/malviscape/dataviz/dataset/ElectricCarData_Clean.csv", header=TRUE)
no_of_vehicles_per_brand <- evs %>% group_by(Brand) %>% tally(n="num_vehicles")
no_of_vehicles_per_brand <- no_of_vehicles_per_brand[order(-no_of_vehicles_per_brand$num_vehicles), ]
head(no_of_vehicles_per_brand)
label <- no_of_vehicles_per_brand$Brand
plot <- pie(no_of_vehicles_per_brand$num_vehicles,labels=label,radius = 1.1, main="Popular EV companies", col = rainbow(length(no_of_vehicles_per_brand$num_vehicles)))
