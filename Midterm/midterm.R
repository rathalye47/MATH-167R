# Rohan Athalye
# Midterm

# -- Problem 1 --

# Constructing the comparative bar graph.
urban <- c(ABC=144, CBS=135, NBC=108, Other=63)
suburban <- c(ABC=180, CBS=240, NBC=225, Other=105)
rural <- c(ABC=90, CBS=96, NBC=54, Other=60)
urban.relfreq <- sapply(urban, FUN=function(x) round(((x/1500) * 100), digits=1))
suburban.relfreq <- sapply(suburban, FUN=function(x) round(((x/1500) * 100), digits=1))
rural.relfreq <- sapply(rural, FUN=function(x) round(((x/1500) * 100), digits=1))
network.matrix <- cbind(urban.relfreq, suburban.relfreq, rural.relfreq)
dimnames(network.matrix) <- list(c("ABC", "CBS", "NBC", "Other"), c("Urban", "Suburban", "Rural"))
network.matrix
barplot(network.matrix, beside=TRUE)
barplot(network.matrix, beside=TRUE, ylim=c(0,20), col=3:6, xlab="Residence", ylab="Percentage", main="Distribution of Television Viewers of Network Preference \nbased on Residence: Comparative Bar Graph")
legend("topright", legend=c("ABC", "CBS", "NBC", "Other"), fill=3:6)

# Recommending networks for each location.
# For urban residences, I would recommend ABC.
# For suburban residences, I would recommend CBS.
# For rural residences, I would recommend CBS.


# -- Problem 2 --

# Constructing the pie chart.
majors <- c(1500, 11000, 7000, 2000, 5000)
majors.freq <- sapply(majors, FUN=function(x) round((x / sum(majors)) * 100))
majors.levels <- c("Agriculture", "Arts and Science", "Business Administration", "Education", "Engineering")
labels <- paste(majors.levels, majors.freq, sep=", ")
new_labels <- paste(labels, "%", sep="")
pie(majors.freq, labels=new_labels, col=rainbow(7), main="Distribution of Undergraduate Majors: Pie Chart")

# Constructing the Pareto chart.
pareto.chart <- data.frame(majors.levels, majors.freq)
names(pareto.chart) <- c("College", "Percentages")
pareto.chart
order <- rev(order(pareto.chart$Percentages))
(pareto.chart.updated <- data.frame(College=pareto.chart$College[order], Percentages=pareto.chart$Percentages[order]))
bp <- barplot(pareto.chart.updated$Percentages, names=pareto.chart.updated$College, col=3:7, ylim=c(0,110), xlab="College", ylab="Percentage", main="Distribution of Undergraduate Majors: Pareto Chart")
(cum.freq <- cumsum(pareto.chart.updated$Percentages))
lines(bp, y=cum.freq, type="l", col="red")
points(bp, y=cum.freq, pch=21, col="red")


# -- Problem 3 --

# Drawing side-by-side boxplots to compare base number of seizures in the placebo and the progabide groups.
library("readxl")
(epilepsy <- read_excel("Epilepsy.xlsx"))
boxplot(epilepsy$Base~epilepsy$Trt, staplewex=.1, horizontal=TRUE, col=2:3, xlab="Base Number of Seizures", ylab="Trt", main="Boxplot of base by treatment")

# Commenting on any interesting features I observe.
# I observed that there are 2 outliers in the base number of seizures for the progabide group.
# I observed that there is 1 outlier in the base number of seizures for the placebo group.
# I observed that both boxplots are right-skewed.

# Drawing side-by-side boxplots to compare the ages of the participants assigned to the placebo and the progabide groups.
boxplot(epilepsy$Age~epilepsy$Trt, staplewex=.1, horizontal=TRUE, col=2:3, xlab="Age", ylab="Trt", main="Boxplot of age by treatment")

# Commenting on any interesting features I observe.
# I observed that neither boxplot has outliers.
# I observed that the boxplot for the progabide group has a larger IQR than the boxplot for the placebo group.
# I observed that the boxplot for the placebo group has a higher median than the boxplot for the progabide group.


# -- Problem 4 --

# Stacking the data set. 
library("readxl")
(dose <- read_excel("Dose.xlsx"))
mg <- stack(dose[2:4])
drug.type <- rep(dose$Drug, 3)
dose.stack <- data.frame(drug.type, mg)
names(dose.stack) <- c("Drug", "Response", "Dose")
dose.stack

# Saving the result in the second sheet of Dose.xlsx.
library("writexl")
write_xlsx(list(Original=dose, Stacked=dose.stack), path="Dose.xlsx")

# Computing the mean and standard deviation of each combination of drug and dose.
Group_Means <- aggregate(dose.stack$Response, by=list(dose.stack$Drug, dose.stack$Dose), FUN=mean)
names(Group_Means) <- c("Drug", "Dose", "Mean Response")
Group_Means
Group_Standard_Deviations <- aggregate(dose.stack$Response, by=list(dose.stack$Drug, dose.stack$Dose), FUN=sd)
names(Group_Standard_Deviations) <- c("Drug", "Dose", "Standard Deviation Response")
Group_Standard_Deviations


# -- Problem 5 --

# Generating a random sample of size n=100 from the binomial distribution B(# of trials = 10, p = .3) and computing its sample mean.
binomial_experiment <- function(){
	x <- rbinom(100, 10, .3)
	mean <- mean(x)
	return(mean)
}
binomial_experiment()

# Repeating the process from part a. 200 times.
(means <- replicate(200, binomial_experiment()))

# Constructing a density histogram for the sample means I have obtained.
hist(means, freq=FALSE, breaks=seq(2.5,3.5,0.05), xlim=c(2.5,3.5), ylim=c(0,4), axes=FALSE, xlab="Sample Means", ylab="Density", main="Density Histogram for Sample Means", col="grey")
axis(1, pos=c(0,0), at=seq(2.5,3.5,.1))
axis(2, pos=c(2.5,0), at=seq(0,4,.5), las=2)

# Adding the normal curve (density curve) to the graph.
expected_mean <- 10*.3
standard_deviation <- sqrt(10*.3*.7)/sqrt(10)
x <- seq(min(means), max(means), length=40)
curve(dnorm(x, mean=expected_mean, sd=standard_deviation), col="blue", lwd=2, add=TRUE)

# The shape of the histogram does not resemble the shape of the density curve.


# -- Problem 6 --

# Simulating an experiment in which I flip a coin that favors heads with probability 0.7.
coins_experiment <- function(num_times){
  toss <- sample(c("H", "T"), num_times, replace=TRUE, prob=c(.7, .3))
  num_heads <- 0
  for (i in 1:num_times) {
	if (toss[i] == "H") num_heads <- num_heads + 1
	}
  num_heads.relfreq = round(num_heads/num_times, digits=2)
  return(num_heads.relfreq)
}

# Flipping the coin 50 times and finding f(50), the relative frequency of the number of heads.
(trial_1 <- coins_experiment(50))

# Repeating the process 100, 150, 200, 250, 300, 350, and 400 times and computing the corresponding relative frequencies such as f(100), f(150), and etc.
(trial_2 <- coins_experiment(100))
(trial_3 <- coins_experiment(150))
(trial_4 <- coins_experiment(200))
(trial_5 <- coins_experiment(250))
(trial_6 <- coins_experiment(300))
(trial_7 <- coins_experiment(350))
(trial_8 <- coins_experiment(400))

# Plotting all points (n,f(n)) that I obtain and connecting them with line segments.
x <- c(50, 100, 150, 200, 250, 300, 350, 400)
y <- c(trial_1, trial_2, trial_3, trial_4, trial_5, trial_6, trial_7, trial_8)
plot(x, y, xlim=c(50,400), ylim=c(0,1), axes=FALSE, type="p", main="Relative Frequencies of Number of Heads", xlab="Number of Flips", ylab="Percentage")
lines(x, y, col="red")
axis(1, pos=c(0,0), at=seq(50,400,50))
axis(2, pos=c(50,0), at=seq(0,1,.1), las=2)

# Adding a horizontal line to the plot y = 0.7.
abline(h=0.7, col="blue")

# It seems that the relative frequencies are getting close to the actual probability of 0.7.
