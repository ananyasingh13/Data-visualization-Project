library(ggplot2)
evs <- read.csv('/home/malviscape/dataviz/dataset/ElectricCarData_Clean.csv')
pwrtrn <- evs[, c(4:6, 9)]
avg_rng <- aggregate(pwrtrn$Efficiency_WhKm ~ pwrtrn$PowerTrain, FUN = mean)
plt <- ggplot(pwrtrn, aes(x = Efficiency_WhKm, y = PowerTrain))+geom_point(aes(color=PowerTrain))
plt <- plt + labs(title="Relationship between Power Train and Efficiency of the EV", subtitle="Power Train vs Efficiency (in Wh-Km)")
plt <- plt + xlab("Efficiency (Wh-km)") + ylab("Power Train") 
plt <- plt + theme(plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"))
plt
