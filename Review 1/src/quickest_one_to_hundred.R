library(ggplot2)
evs <- read.csv('/home/malviscape/dataviz/dataset/ElectricCarData_Clean.csv')
ev_acc<-evs[,1:3][order(evs$AccelSec),]
plt <- ggplot(ev_acc, aes(x = Model, y = AccelSec))+geom_bar(stat="identity")
plt <- plt + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plt <- plt + xlab("Model of the vehicle") + ylab("Acceleration (s)")
plt <- plt + labs(title = "Quickest 0-100 km/h in an EV", subtitle = "Acceleration per second vs Model")
plt
