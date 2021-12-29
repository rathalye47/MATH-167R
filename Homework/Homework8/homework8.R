# Rohan Athalye
# Homework 8: Summary Statistics and Plots

# -- Problem 1 --

# Generating a random sample of size=100 from the normal distribution with mean=10 and standard deviation=2.
(x <- rnorm(n=100, mean=10, sd=2))

# Standardizing the values in the set.
(standardized_x <- scale(x, center=TRUE, scale=TRUE))

# Finding the 1st, 2nd, 3rd ,�, 99th percentiles/quantiles of the standardized set.
(standardized_quantiles <- quantile(standardized_x, seq(0.01, 0.99, 0.01)))

# Finding the corresponding percentiles/quantiles for the standard normal distribution.
(normal_quantiles <- quantile(x, seq(0.01, 0.99, 0.01)))

# Plotting the points (x, y) where x = a quantile of the standard normal distribution and y = the corresponding quantile of the standardized set.
plot(normal_quantiles, standardized_quantiles, main="QQplot for Normal Distribution", xlab="Theoretical Quantiles", ylab="Sample Quantiles")

# Adding a line with slope 1 and y-intercept 0 to the plot.
abline(a=0, b=1, col="red")

# Commenting on the plot.
# The points form a staight line. 
# The points deviate significantly from the line drawn in 1f since I cannot even see that line.


# -- Problem 2 --

# Generating a random sample of size=100 from the exponential distribution with the rate parameter equal to 2.
(y <- rexp(n=100, rate=2))

# Finding the 1st, 2nd, 3rd ,�, 99th percentiles/quantiles for the exponential distribution.
(exponential_quantiles <- quantile(y, seq(0.01, 0.99, 0.01)))

# Corresponding percentiles/quantiles for the standard normal distribution.
normal_quantiles

# Plotting the points (x, y) where x = a quantile of the standard normal distribution and y = the quantile of the exponential distribution.
plot(normal_quantiles, exponential_quantiles, main="QQplot for Exponential Distribution", xlab="Theoretical Quantiles", ylab="Sample Quantiles")

# Commenting on the plot.
# The points do not form a straight line. Instead, the points form a curve following that of an exponential graph.


# -- Problem 3 --

# Downloading the data into my R-session.
library("MASS")
data(sleep)
sleep

# Using tapply() to produce summary statistics for groups 1 and 2 of variable "extra".
group_statistics <- tapply(sleep$extra, sleep$group, summary)

# Checking the structure of the resulting object.
str(group_statistics)
# The object that I got as a result of using tapply() was a List of 2.

# Printing the resulting summaries for groups 1 and 2.
group_statistics


# -- Problem 4 --

# Creating the dataframe.
library("readxl")
(gravity <- read_excel("gravity.xlsx"))

# Constructing a stem-and-leaf display using repeated stems.
stem(gravity$spec_gravity)

# Commenting on any interesting features of the display.
# I noticed that the stem-and-leaf display automatically used repeated stems, even though the dataset was quite small that we wouldn't need repeated stems.
# I found it interesting that a stem for "7" was displayed, even though it has no leaves.
