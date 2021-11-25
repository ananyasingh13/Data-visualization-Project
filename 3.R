library(tidyverse) 
library(ggplot2) 
library(plotly)

evs <- read.csv('C:/Users/Administrator/Desktop/ElectricCarData_Clean.csv')

ev_acc<-evs[,1:3][order(evs$AccelSec),] 

plt <- plot_ly(ev_acc, x = ~Model, y = ~AccelSec, type ='bar')
plt <- plt %>%  layout(title = "<b>Quickest 0-100 km/h in an EV</b><br><i>Acceleration per second vs Model</i>",
                       
                       yaxis = list(title = "Acceleration (s)"), 
                       xaxis = list(title = "Model of the vehicle") )
plt
