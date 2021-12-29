# Rohan Athalye
# Homework 7: Sorting Dataframes, Simulating Experiments, and Distributions

# -- Problem 1 --

# Creating the dataframe.
library("readxl")
(Crime.rate <- read_excel("crime_rate.xlsx"))

# Using function sort to display the three highest violent crime rates in the dataframe.
Crime.rate.sorted <- rev(sort(Crime.rate$`Violent Crime`))
Crime.rate.sorted[1:3]

# Arranging the rows of the dataframe so that �Violent Crime� is in descending order.
(Crime.rate.2 <- Crime.rate[match(Crime.rate.sorted, Crime.rate$`Violent Crime`), ])

# Displaying the top three states along with their highest violent crime rates.
(Crime.rate.2[c(1:3),])

# Writing Crime.rate and Crime.rate.2 into the file crime_rate_updated.xlsx.
library("writexl")
write_xlsx(list(Original=Crime.rate, Sorted=Crime.rate.2), path="crime_rate_updated.xlsx")


# -- Problem 2 --

# Rolling two fair dice and calculating X, which is the total number of dots shown by the dice.
dice_experiment <- function(){
  roll <- sample(1:6, size=2, replace=TRUE)
  X <- sum(roll)
  return(X)
}

# Repeating the experiment 50 times and finding the average value of X.
mean(replicate(50, dice_experiment()))


# -- Problem 3 --

# Tossing a coin 10 times and calculating X, which is the number of heads the coin shows.
coins_experiment <- function(){
  toss <- sample(c("H", "T"), 10, replace=TRUE, prob=c(.6, .4))
  X <- 0
  for (i in 1:10) {
	if (toss[i] == "H") X <- X + 1
	}
  return(X)
}

# Repeating the experiment 100 times and finding the average value of X.
mean(replicate(100, coins_experiment()))

# The average value of X seems close to the expected value of X.
# The expected value of X is 6.


# -- Problem 4 --

# Using base package to produce the provided plot.

# Generating a sequence of x values from -3 to 3 with increment .1.
x <- seq(-3, 3, .1)

# Using the dnorm() function to evaluate the pdf of N(0,1) at x's.
# Using the plot function to plot points and connect them using option type="l", l=line.
plot(x, dnorm(x), axes=FALSE, type="l", main="Standard Normal and t-distribution \nComparing Densities", ylab="density", col="red")

# Adding axes.
axis(1, pos=c(0,0))
axis(2, pos=c(-3,0), las=2)

# Overlaying graphs with the legend.
lines(x, dt(x, 5), col="blue")
lines(x, dt(x, 10), col="green")
legend("topright", c("N(0,1)", "t-dist,df=5", "t-dist,df=10"), fill=c("red", "blue", "green"), bty="n")
