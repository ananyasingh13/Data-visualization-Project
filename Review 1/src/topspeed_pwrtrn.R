library(ggplot2)
evs <- read.csv('/home/malviscape/dataviz/dataset/ElectricCarData_Clean.csv')
pwrtrn <- evs[, c(4:6, 9)]
avg_rng <- aggregate(pwrtrn$TopSpeed_KmH ~ pwrtrn$PowerTrain, FUN = mean)
plt <- ggplot(pwrtrn, aes(x = TopSpeed_KmH, y = PowerTrain)) + geom_point(aes(color=PowerTrain))
plt <- plt + labs(title="Relationship between Power Train and Top Speed of the EV", subtitle="Power Train vs Top Speed")
plt <- plt + xlab("Top Speed (km/h)") + ylab("Power Train") 
plt <- plt + theme(plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"))
plt
