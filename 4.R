library(tidyverse) 
library(ggplot2) 
library(plotly)

evs <- read.csv('C:/Users/Administrator/Desktop/ElectricCarData_Clean.csv')

ev_efficiency<-evs[,c(1:2,6)][order(evs$Efficiency_WhKm),] 

plt <- plot_ly(ev_efficiency, x = ~Model, y = ~Efficiency_WhKm, type ='bar')
plt <- plt %>%  layout(title = "<b>Efficiencies of EVs</b><br><i>Efficiency vs Model</i>",
                       
                       yaxis = list(title = "Efficiency in Wh-Km"), 
                       xaxis = list(title = "Model of the vehicle") )

plt
