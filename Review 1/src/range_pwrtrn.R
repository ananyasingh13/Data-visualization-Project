library(ggplot2)
evs <- read.csv('/home/malviscape/dataviz/dataset/ElectricCarData_Clean.csv')
pwrtrn <- evs[, c(4:6, 9)]
avg_rng <- aggregate(pwrtrn$Range_Km ~ pwrtrn$PowerTrain, FUN = mean)
plt <- ggplot(pwrtrn, aes(x = Range_Km, y = PowerTrain))+geom_point(aes(color=PowerTrain))
plt <- plt + labs(title="Relationship between Power Train and Range of the EV", subtitle="Power Train vs Range (in km)")
plt <- plt + xlab("Range (km)") + ylab("Power Train") 
plt <- plt + theme(plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"))
plt
