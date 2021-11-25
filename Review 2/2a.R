library(tidyverse) 
library(ggplot2) 
library(plotly)
evs <- read.csv('C:/Users/Administrator/Desktop/ElectricCarData_Clean.csv')

Rapidcharge_price <- evs[, c(8, 14)] 
Rapidcharge_price <- Rapidcharge_price[order(Rapidcharge_price$PriceEuro), ] 
avg_price <- aggregate(Rapidcharge_price$PriceEuro ~ Rapidcharge_price$RapidCharge, FUN = "mean") 
avg_price <- avg_price %>% rename(RapidCharge = `Rapidcharge_price$RapidCharge`, Avg_Price = `Rapidcharge_price$PriceEuro`) 
less_than_avg <- Rapidcharge_price %>% filter(PriceEuro < avg_price$Avg_Price[2]) 
yes_percentage <- round((nrow(less_than_avg %>% filter(RapidCharge == "Yes")) /nrow(less_than_avg)) * 100, 2) 
no_percentage <- round(100 - yes_percentage, 2) 
percentages <- data.frame(rapidcharge = c("Yes", "No"), percentage = rbind("1" = yes_percentage,"2" = no_percentage)) 
head(percentages) 

colors <- c('rgb(5, 193, 230)', 'rgb(255, 48, 124)')

fig <- plot_ly(percentages, labels = ~rapidcharge, values = ~percentage , type = 'pie',marker = list(colors = colors,
                                                                                                     line = list(color = '#FFFFFF', width = 1)))

fig <- fig %>% layout(title = '<b>Relation between Price and Rapid Charging?</b><br><i>Chances of having Rapid charging Below ???57,324.68</i>',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      legend = list(title=list(text='<b> Rapid Charge available </b>')))
fig
