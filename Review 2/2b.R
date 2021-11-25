
library(tidyverse) 
library(ggplot2) 
library(plotly)

evs <- read.csv('C:/Users/Administrator/Desktop/ElectricCarData_Clean.csv')

Rapidcharge_price <- evs[, c(8, 14)] 
Rapidcharge_price <- Rapidcharge_price[order(Rapidcharge_price$PriceEuro), ] 
avg_price <- aggregate(Rapidcharge_price$PriceEuro ~ Rapidcharge_price$RapidCharge, FUN = "mean") 
avg_price <- avg_price %>% rename(RapidCharge = `Rapidcharge_price$RapidCharge`, Avg_Price = `Rapidcharge_price$PriceEuro`) 
less_than_25 <- Rapidcharge_price %>% filter(PriceEuro < 25000) 
yes_percentage <- round((nrow(less_than_25 %>% filter(RapidCharge == "Yes")) /nrow(less_than_25)) * 100, 2) 
no_percentage <- round(100 - yes_percentage, 2) 
percentages_2 <- data.frame(rapidcharge = c("Yes", "No"), percentage = rbind("1" = yes_percentage,"2" = no_percentage)) 

colors <- c('rgb(5, 193, 230)', 'rgb(255, 71, 102)')

fig <- plot_ly(percentages_2, labels = ~rapidcharge, values = ~percentage , type = 'pie',marker = list(colors = colors,
                                                                                                       line = list(color = '#FFFFFF', width = 1)))

fig <- fig %>% layout(title = '<b>Relation between Price and Rapid Charging</b><br><i>Chances of having Rapid charging Below Below ???25,000</i>',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      legend = list(title=list(text='<b> Rapid Charge available </b>')))
fig

