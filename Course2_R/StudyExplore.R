## Exploratoty graphs

pollution <- read.csv("avgpm25.csv", colClasses=c("numeric", "character", "factor", "numeric", "numeric"))

# one dimention

summary(pollution$pm25)

boxplot(pollution$pm25, col="blue")

hist(pollution$pm25, col="green")
rug(pollution$pm25)

hist(pollution$pm25, col="green", breaks=100)
rug(pollution$pm25)

boxplot(pollution$pm25, col="blue")
abline(h = 12)

hist(pollution$pm25, col="green")
abline(v = 12, lwd= 2)
abline(v = median(pollution$pm25), col = "magenta", lwd= 4)

barplot(table(pollution$region), col="wheat", main ="number of counties in each regions")

# two dimention

boxplot(pm25 ~ region, data = pollution, col = "red")

par(mfrow = c(2,1), mar = c(4,4,2,1))
hist(subset(pollution, region == "east")$pm25, col = "green")
hist(subset(pollution, region == "west")$pm25, col = "blue")

#scatterplot
with(pollution, plot(latitude, pm25))
abline(h = 12, lwd = 2, lty = 2)


with(pollution, plot(latitude, pm25, col = region))
abline(h = 12, lwd = 2, lty = 2)


par(mfrow = c(1,2), mar = c(1,1,1,1))
with(subset(pollution, region == "west"),plot(latitude, pm25, main = "west"))
with(subset(pollution, region == "east"),plot(latitude, pm25, main = "east"))


# lattice

library(lattice)

state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4,1))


#ggplot
install.packages("ggplot2")
library(ggplot2)
qplot(displ, hwy, data = mpg)

## Base ploting system ****************

hist(airquality$Ozone)

par(mfrow = c(1,1), mar = c(4,4,2,1))

with(airquality, plot(Ozone, Wind))


airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality, xlab="Month", ylab="Ozone")


with(airquality, plot(Ozone, Wind))
title(main = "test title")


with(airquality, plot(Ozone, Wind, main = "title 2"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col="blue"))

with(airquality, plot(Wind, Ozone, main = "title 3", type = "n"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col="blue"))
with(subset(airquality, Month != 5), points(Wind, Ozone, col="red"))
legend("topright", pch = 1, col= c("blue", "red"), legend = c("May", "Other months"))

model <- lm(Ozone ~ Wind, airquality)
abline(model, lwd = 2)


par(mfrow=c(1,2), mar=c(4,4,2,1), oma=c(0, 0,2, 0))
with(airquality, {
  plot(Wind, Ozone, main="Wind vs Ozone")
  plot(Solar.R, Ozone, main="Solar.R, Ozone")
  mtext("Multiple plots", outer=TRUE)
})


