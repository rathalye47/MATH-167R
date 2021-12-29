# Rohan Athalye
# Homework 9: Dot Plots and Histograms

# -- Problem 1 --

# Downloading the data.
library("readxl")
(diameters <- read_excel("diameters.xlsx"))

# Constructing a dot plot for the data using base package.
stripchart(diameters$C1, method="stack", at=0, xlim=c(2,14), offset=.6, pch=19, frame.plot=FALSE, xlab="Number of Trees", main="Dotplot of Number of Trees \nWith Diameters Exceeding 12 Inches")

# Commenting on the shape of the distribution.
# The distribution is unimodal and slightly left-skewed.

# Using function hist() to build a relative frequency histogram for the data.
diameters <- diameters$C1
rel.freq.h <- hist(diameters, breaks=seq(0, 14, 2))
rel.freq.h$counts <- rel.freq.h$counts/sum(rel.freq.h$counts)
plot(rel.freq.h, xlim=c(0,14), ylim=c(0,.5), axes=FALSE, main="Relative Frequency Distribution of Number of Trees \nWith Diameters Exceeding 12 Inches", xlab="Number of Trees", ylab="Relative Frequency", col="grey")
axis(1, pos=c(0,0))
axis(2, pos=c(0,0), las=2)

# The shape in part d. is not different from the one I observed in the dotplot.
# In fact, they are similar to each other as they are both unimodal and slightly left-skewed.

# Finding the sample mean and standard deviation for the data.
(sample_mean <- mean(diameters))
(sample_standard_deviation <- sd(diameters))

# Using R to count the percentages of squares falling in each of the three intervals.
percentages <- numeric(3)
interval_1a <- sample_mean + sample_standard_deviation
interval_1b <- sample_mean - sample_standard_deviation
interval_2a <- sample_mean + (2 * sample_standard_deviation)
interval_2b <- sample_mean - (2 * sample_standard_deviation)
interval_3a <- sample_mean + (3 * sample_standard_deviation)
interval_3b <- sample_mean - (3 * sample_standard_deviation)
interval_1_count <- 0
interval_2_count <- 0
interval_3_count <- 0
for (i in 1:70) {
	if ((diameters[i] > interval_1b) & (diameters[i] < interval_1a)) interval_1_count <- interval_1_count + 1
	if ((diameters[i] > interval_2b) & (diameters[i] < interval_2a)) interval_2_count <- interval_2_count + 1
	if ((diameters[i] > interval_3b) & (diameters[i] < interval_3a)) interval_3_count <- interval_3_count + 1
	}
interval_1_percentage <- round((interval_1_count / length(diameters)) * 100)
interval_2_percentage <- round((interval_2_count / length(diameters)) * 100)
interval_3_percentage <- round((interval_3_count / length(diameters)) * 100)
percentages[1] <- interval_1_percentage
percentages[2] <- interval_2_percentage
percentages[3] <- interval_3_percentage
percentages

# Creating the table using the results from part g.
x <- c("mean +/- sd", "mean +/- 2sd", "mean +/- 3sd")
(data_table <- data.frame("Intervals"=x, "Percentages"=percentages))

# Stating my conclusion.
# The percentages I obtained in part g. are extremely close to the corresponding percentages given by the Empirical Rule.
# Specifically, the percentages I obtained in part g. are a bit higher than the corresponding percentages given by the Empirical Rule.
# Thus, the percentages I obtained in part g. closely resemble that of a normal distribution.
