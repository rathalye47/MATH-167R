# Rohan Athalye
# Homework 10: Multiple Graphs and ggplot2

# -- Problem 1a --

# Downloading the dataset cars (in the base package).
data("cars")

# Constructing the frequency histogram for speed.
cars.speed <- cars$speed
hist(cars.speed, axes=FALSE, breaks=seq(3.5,25.5,1), main="Frequency Histogram for cars: speed", xlab="speed", ylab="frequency", xlim=c(0,26), ylim=c(0,6))
axis(1, pos=c(0,0), at=seq(2,26,2))
axis(2, pos=c(2,0), at=seq(0,6,1), las=2)


# -- Problem 1b --

# Allows us to combine multiple plots into 1 overall graph.
par(mfrow=c(2,2), cex=1, mar=c(3,4,3,1))

# Creating the frequency histogram for speed.
hist(cars.speed, axes=FALSE, breaks=seq(3.5,25.5,1), main="", xlab="", ylab="", xlim=c(0,26), ylim=c(0,6))
axis(1, pos=c(0,0), at=seq(2,26,2))
axis(2, pos=c(2,0), at=seq(0,6,1), las=2)
mtext("Cars: Speed", side=3, line=1)
mtext("frequency", side=2, line=1, cex=.8)

# Creating the frequency histogram for distance.
cars.distance <- cars$dist
hist(cars.distance, axes=FALSE, breaks=seq(0,120,20), main="", xlab="", ylab="", xlim=c(0,120), ylim=c(0,20))
axis(1, pos=c(0,0), at=seq(0,120,20))
axis(2, pos=c(0,0), at=seq(0,20,5), las=2)
mtext("Cars: Distance", side=3, line=1)
mtext("frequency", side=2, line=2, cex=.8)

# Creating the boxplot for speed.
boxplot(cars.speed, staplewex=.5, horizontal=TRUE, frame=FALSE)
mtext("speed", side=1, line=2, cex=.8)

# Creating the boxplot for distance.
boxplot(cars.distance, staplewex=.5, horizontal=TRUE, frame=FALSE)
mtext("distance", side=1, line=2, cex=.8)


# -- Problem 2 --

# Downloading the dataset iris (in the base package).
data("iris")

# Loading the package ggplot2.
library(ggplot2)

# Creating the scatterplot.
Species <- factor(iris$Species)
ggplot(data=iris, aes(x=Petal.Length, y=Sepal.Length, col=Species)) + geom_point() + 
	labs(title="Scatter Plot: Petal Length vs Sepal Length", subtitle="Dataset Iris", x="petal length", y="sepal length") + 
	theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
