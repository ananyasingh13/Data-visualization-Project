#Q1

library(ggplot2) 
library(plotly) 
evs <- read.csv('C:/Users/Administrator/Desktop/ElectricCarData_Clea n.csv') 
pwrtrn <- evs[, c(4:6, 9)] 
avg_rng <- aggregate(pwrtrn$Range_Km ~ pwrtrn$PowerTrain, FUN = mean) 
plt <- plot_ly(data=evs, x= ~Range_Km, y= ~PowerTrain, color=~PowerTrain ,type="scatter") 
plt <- plt %>% layout(title = "<b>Relationship between Power Train and Range of the EV</b><br><i>Power Train vs Range (in km)</i>", 
                      yaxis = list(title = "Power Train"), xaxis = list(title = "Range (km)")) 

plt