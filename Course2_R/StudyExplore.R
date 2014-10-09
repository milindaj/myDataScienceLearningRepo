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

## course project - hopuse hold consumption
hpcDT <- fread("household_power_consumption.txt", sep=";", colClasses = c("charecter", "charecter", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), na.strings=c("?"))



## lattice

library(lattice)
xyplot(Ozone ~ Wind, data = airquality)

airquality <- transform(airquality, Month = factor(Month))
xyplot(Ozone ~ Wind | Month, data = airquality, layout=c(5,1))

set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each = 50)
y <- x + f *x + rnorm(100, sd = 0.5)
f <- factor(f, labels = c("Group 1", "Group2"))
xyplot(y ~ x | f, layout=c(2,1))

xyplot(y ~ x | f, panel = function(x, y, ...) {
  panel.xyplot(x, y, ...)
  panel.abline(h = median(y), lty = 2)
})

xyplot(y ~ x | f, panel = function(x, y, ...) {
  panel.xyplot(x, y, ...)
  panel.lmline(x, y, col = 2)
})


## ggplot2

qplot(displ, hwy, data = mpg)

qplot(displ, hwy, data = mpg, color = drv)

qplot(displ, hwy, data = mpg, geom= c("point", "smooth"))

qplot(hwy, data = mpg, fill = drv)


# facets

qplot(displ, hwy, data = mpg, facets = . ~drv)

qplot(hwy, data = mpg, facets = drv~., binwidth = 2)

qplot(hwy, data = mpg, facets = drv~cyl, binwidth = 2)

qplot(displ, hwy, data = mpg, facets = . ~ drv, geom = c("point", "smooth"), method = "lm" )

# ggplot
g <- ggplot(mpg, aes(displ, hwy))
p <- g + geom_point()

p <- g + geom_point() + geom_smooth()

p <- g + geom_point() + geom_smooth(method = "lm")


p <- g + geom_point() + facet_grid(. ~ drv) + geom_smooth(method = "lm")


g + geom_point(color = "steelblue", size = 4, alpha = 1/2)

g + geom_point(aes(color = drv), size = 4, alpha = 1/2)


g + geom_point(aes(color = drv), size = 4, alpha = 1/2) + labs(title = " car chart") + labs(x = "my way", y = "highway")


g + geom_point(aes(color = drv), size = 4, alpha = 1/2) + geom_smooth (size = 4, linetype = 3, method = "lm", se = FALSE)


g + geom_point(aes(color = drv), size = 4, alpha = 1/2) + labs(title = " car chart") + labs(x = "my way", y = "highway") + theme_bw(base_family = "Times")


# outliers
testdata <- data.frame (x = 1:100, y = rnorm(100))
testdata[50,2] <- 100

plot(testdata$x, testdata$y, type = "1", ylim=c(-3,3))

g <- ggplot(testdata, aes(x = x, y = y))
g + geom_line()
g + geom_line() + coord_cartesian(ylim = c(-3, 3))


g <- ggplot(mpg, aes(displ, hwy))
g <- g + geom_point(alpha = 1/3) 
g <- g + facet_wrap (drv ~ cyl, nrow = 4, ncol = 4) 
g <- g + geom_smooth (method = "lm", se = FALSE, col = "steelblue") 
g <- g + theme_bw(base_size = 12) 
g <- g + labs(x = "x label") 
g <- g + labs(y = "y label") 
g <- g + labs(title = "main title")

