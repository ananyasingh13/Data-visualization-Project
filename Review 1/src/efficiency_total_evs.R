library(ggplot2)
evs <- read.csv('/home/malviscape/dataviz/dataset/ElectricCarData_Clean.csv')
ev_efficiency<-evs[,c(1:2,6)][order(evs$Efficiency_WhKm),]
plt <- ggplot(data = ev_efficiency, aes(x = Model, y = Efficiency_WhKm))
plt <- plt + geom_bar(stat="identity")
plt <- plt + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plt <- plt + xlab("Model of vehicle") + ylab("Efficiency in Wh-Km")
plt <- plt + labs(title = "Efficiencies of EVs", subtitle = "Efficiency vs Model")
plt <- plt + theme(plot.subtitle = element_text(face = "italic"), plot.title = element_text(face = "bold"))
plt
