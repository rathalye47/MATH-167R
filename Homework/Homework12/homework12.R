# Rohan Athalye
# Homework 12: ggplot2 + Confidence Intervals

# -- Problem 1 --

# Writing a function that will construct a two-sided confidence interval for the population standard deviation.
construct_confidence_interval <- function(data, conf.level) {
	dof <- length(data)-1
	standard.dev <- sd(data)
	conf.level.sides <- 1-conf.level
	conf.level.int <- conf.level * 100
	left.critical.val <- qchisq(conf.level.sides/2, dof, lower.tail=FALSE)
	right.critical.val <- qchisq(1-conf.level.sides/2, dof, lower.tail=FALSE)
	lower.conf.limit <- round(sqrt((dof*standard.dev*standard.dev)/left.critical.val), 4)
	upper.conf.limit <- round(sqrt((dof*standard.dev*standard.dev)/right.critical.val), 4)
	sprintf("The %i%% interval is (%.4f,%.4f)", conf.level.int, lower.conf.limit, upper.conf.limit)
}


# -- Problem 2 --

# Loading the libraries and downloading the data.
library(ggplot2)
library(readxl)
pressures <- read_excel("pressure.xlsx")

# Constructing the normal plot using package ggplot2.
ggplot(pressures, aes(sample=pressure)) +
	geom_qq(col="coral4") + geom_qq_line(col="blue") +
	labs(title="Normal Plot for Maximum Concrete Pressure", x="Theoretical Quantiles", y="Pressure") +
	theme(plot.title = element_text(hjust = 0.5))

# It is plausible that this sample was selected from a normal population distribution.
# The points fall close to the straight line, suggesting that the assumption of a normal population distribution is plausible.

# Calculating a two-sided confidence interval for the population standard deviation of maximum pressure with confidence level 95%.
construct_confidence_interval(pressures$pressure, .95)
