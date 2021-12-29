# Rohan Athalye
# Final Exam

# -- Problem 1 --

# Writing a function that computes the sample size for the given quantities.
compute_sample_size_mean <- function(alpha, beta, st_dev, mu, sample_mean, type) {
	delta <- mu - sample_mean
	z.beta <- qnorm(beta, lower.tail=FALSE)
	if ((type=='>') | (type=='<')) {
		z.alpha <- qnorm(alpha, lower.tail=FALSE)
		n <- round((st_dev*(z.alpha+z.beta))^2/(delta)^2, digits=0)
		paste('Sample size is', n, sep=' ')
	}
	else {
		if (type=='not=') {
			z.alpha <- qnorm(alpha/2, lower.tail=FALSE)
			n <- round((st_dev*(z.alpha+z.beta))^2/(delta)^2, digits=0)
			paste('Sample size is', n, sep=' ')
		}
		else {
			print('Error: the only acceptable types are <, >, or not=')
		}
	}
}


# -- Problem 2 --

# Using the function from Problem 1 to obtain the sample size.
compute_sample_size_mean(0.01, 0.01, 0.3, 5.5, 5.6, 'not=')

# Stating the test hypotheses.
# Null hypothesis: mu = 5.5
# Alternative hypothesis: mu != 5.5

# Stating the output of the function used to obtain the sample size.
# delta = 5.5 - 5.6 = -0.1
# z.alpha = 2.575829
# z.beta = 2.326348


# -- Problem 3 --

# - Part a -

# Reading the data from the text file.
ALD <- scan("ALD.txt", skip=1, what=numeric(0), quiet=T, sep="")

# Getting summary statistics for the data.
summary(ALD)

# Calculating the sample standard deviation.
sd(ALD)

# Calculating the interquartile range.
IQR(ALD)

# Creating the table (dataframe).
# Values are rounded to 2 decimal places.
values <- c(0.75, 0.64, 0.30, 0.52, 1.00, 0.48)
names <- c("Sample Mean", "Sample Median", "Sample Standard Deviation", "Q1", "Q3", "IQR")
ALD.dataframe <- data.frame(names, values)
colnames(ALD.dataframe) <- c("Statistics", "Values")
print(ALD.dataframe, row.names=FALSE)

# - Part b -

# Constructing a boxplot for the data.
boxplot(ALD, staplewex=.1, horizontal=TRUE, col="red", xlab="ALD", main="Boxplot: ALD")

# Commenting on any interesting features of the boxplot.
# The shape of the distribution of the boxplot is right-skewed.
# There are no outliers present.

# - Part c -

# Drawing a histogram using 7 classes.
hist(ALD, breaks=7, main="Histogram: Distribution of ALD", xlab="ALD", ylim=c(0,20), axes=FALSE, col="steelblue")
axis(1, pos=0)
axis(2, pos=0.2, las=2)

# My assumption of the shape of the distribution made in part b is confirmed by the new plot.

# - Part d -

# Loading ggplot2.
library(ggplot2)

# Making the normal probability plot.
ALD <- as.data.frame(ALD)
ggplot(ALD, aes(sample=ALD)) +
	geom_qq(col="coral4") + geom_qq_line(col="blue") +
	labs(title="Normal Probability Plot for ALD", x="Theoretical Quantiles", y="ALD") +
	theme(plot.title = element_text(hjust = 0.5))

# It is not plausible that ALD is at least approximately normally distributed.
# Since the sample size is large, normality does not need to be assumed prior to calculating a CI for true average ALD or testing hypotheses about true average ALD. 

# - Part e -

# Stating the test hypotheses.
# Null hypothesis: mu = 1.0
# Alternative hypothesis: mu < 1.0

# Carrying out an appropriate test of hypotheses.
zTest <- function(mu, sample_mean, st_dev, n, type) {
	z <- (sample_mean - mu) / st_dev * sqrt(n)
	paste('Test statistic is z = ', round(z, digits=2), '. ', sep='')
	if ((type=='>') | (type=='<')) {
		paste('Test statistic is z = ', round(z, digits=2), '. ', 'P-value is ', p.value=round(pnorm(-abs(z)), digits=4), sep='')
	}
	else {
		if (type=='not=') {
			paste('Test statistic is z = ', round(z, digits=2), '. ', 'P-value is ', p.value=round(2*pnorm(-abs(z)), digits=4), sep='')
		}
		else {
			print('Error: type is not acceptable')
		}
	}
}

zTest(1.0, mean(ALD$ALD), sd(ALD$ALD), 49, '<')

# Test statistic (rounded to 2 decimal places): z = -5.79.

# P-value ~ 0.

# Final conclusion.
# Since the P-value is <= 0.01, we reject the null hypothesis.
# The data provides strong evidence for concluding that true average ALD under these circumstances is less than 1.0.

# - Part f -

# Calculating a two-sided confidence interval for true average ALD using a confidence level of 95%.
mean_interval <- function(data, conf.level, st_dev) {
	sample_mean = mean(data)
	cr.value=qnorm((1-conf.level)/2, lower.tail=FALSE)
	error=cr.value*st_dev/sqrt(length(data))
	interval <- c(sample_mean-error, sample_mean+error)
	conf.percentage = conf.level * 100
	paste("The ", conf.percentage, "% interval for true average ALD is (", round(interval[1], digits=4), ",", round(interval[2], digits=4), ")", sep="")
}

mean_interval(ALD$ALD, 0.95, sd(ALD$ALD))

# Interpreting the result.
# We don't know what is the true average ALD.
# However, based on the data given to me, I estimate it to be between 0.6651 and 0.8345 pixel dimensions.
# We can trust this interval I produced since it is produced by a formula that is correct about 95% of the time.


# -- Problem 4 --

# - Part a -

# Constructing a normal probability plot of the data.
minutes <- c(159, 120, 480, 149, 270, 547, 340, 43, 228, 202, 240, 218)
minutes <- as.data.frame(minutes)
ggplot(minutes, aes(sample=minutes)) +
	geom_qq(col="coral4") + geom_qq_line(col="blue") +
	labs(title="Normal Probability Plot for Repair Times", x="Theoretical Quantiles", y="Repair Time (Minutes)") +
	theme(plot.title = element_text(hjust = 0.5))

# It is plausible that the population distribution of repair time is at least approximately normal.

# - Part b -

# Stating the test hypotheses.
# Null hypothesis: mu = 200
# Alternative hypothesis: mu > 200

# Carrying out an appropriate test of hypotheses.
t.test(minutes$minutes, mu=200, alternative="greater")

# Test statistic (rounded to 2 decimal places): t = 1.19.

# P-value = 0.1304.

# Final conclusion.
# Since the P-value is > 0.05, we fail to reject the null hypothesis.
# There is not compelling evidence for concluding that true average repair time exceeds 200 minutes.

# - Part c -

# Calculating the type II error probability of the test used in part b when true average repair time is actually 300 minutes.
z.alpha=qnorm(0.05, lower.tail=FALSE)
arg <- z.alpha + (200-300)/150*sqrt(length(minutes$minutes))
beta <- pnorm(arg)
paste("The type II error probability of the test is ", round(beta, digits=4), sep="")


# -- Problem 5 --

# - Part a -

# p-hat = 14/100 = 0.14.

# Stating the test hypotheses.
# Null hypothesis: p = 0.10
# Alternative hypothesis: p > 0.10

# Carrying out an appropriate test of hypotheses.
top <- 0.14 - 0.10
bottom <- sqrt((0.1*0.9)/100)
z.value <- top/bottom
p.value <- 1-pnorm(z.value)
p.value <- round(p.value, digits=4)
paste('Test statistic is z = ', round(z.value, digits=2), '. ', 'P-value is ', p.value, sep="")

# Test statistic (rounded to 2 decimal places): z = 1.33.

# P-value = 0.0912.

# Final conclusion.
# Since the P-value is > 0.05, we fail to reject the null hypothesis.
# This does not provide compelling evidence for concluding that more than 10% of all plates blister under such circumstances.

# In reaching my conclusion, I might have committed a type II error.

# - Part b -

# Writing a function that performs sample size calculations.
compute_sample_size_proportion <- function(alpha, beta, p0, p.hat, type) {
	p0.calculation <- sqrt(p0*(1-p0))
	p.hat.calculation <- sqrt(p.hat*(1-p.hat))
	delta <- p.hat - p0
	z.beta <- qnorm(beta, lower.tail=FALSE)
	if ((type=='>') | (type=='<')) {
		z.alpha <- qnorm(alpha, lower.tail=FALSE)
		n <- round(((z.alpha*p0.calculation) + (z.beta*p.hat.calculation))^2 / (delta)^2, digits=0)
		paste('Sample size is', n, sep=' ')
	}
	else {
		if (type=='not=') {
			z.alpha <- qnorm(alpha/2, lower.tail=FALSE)
			n <- round(((z.alpha*p0.calculation) + (z.beta*p.hat.calculation))^2 / (delta)^2, digits=0)
			paste('Sample size is', n, sep=' ')
		}
		else {
			print('Error: the only acceptable types are <, >, or not=')
		}
	}
}

# - Part c -

# Using my function from part b to perform the calculations.
compute_sample_size_proportion(0.05, 0.10, 0.10, 0.15, '>')

# 362 plates would have to be tested.
