# Rohan Athalye
# Homework 11: ggplot2

# -- Problem 1 --

# Constructing a pie chart for the data.
College <- c('Agriculture', 'Arts and Science', 'Business Administration', 'Education', 'Engineering')
Number.of.Majors <- c(1500, 11000, 7000, 2000, 5000)
Frequency <- sapply(Number.of.Majors, FUN=function(x) round((x / sum(Number.of.Majors)) * 100))
majors <- data.frame(College, Frequency)
library(ggplot2)
labels <- paste(majors$Frequency, "%", sep='')
ggplot(data=majors, aes(x='', y=Frequency, fill=College)) +
		geom_bar(stat='identity', col='black', width=1.5) +
		coord_polar('y', direction=1) + ylab('Distribution of Undergraduate Majors') +
		theme_void() +
		geom_text(aes(label=labels), position = position_stack(vjust = 0.5)) +
		ggtitle("Pie Chart: Distribution of Undergraduate Majors") +
		theme(plot.title=element_text(hjust=0.5))

# Constructing a bar graph for the data.
ggplot(data=majors, aes(College, Frequency)) +
	geom_col(color="black", fill="darkgreen", width=.8) +
	labs(title="Bar Graph: Distribution of Undergraduate Majors", y="Percentage") +
	theme(plot.title = element_text(hjust = 0.5)) +
	ylim(0,50)


# -- Problem 2 --

# Downloading the data.
library("readxl")
distances <- read_excel("distance.xlsx")

# Constructing the frequency histogram for the data.
tick_marks = c(0,20,40,60,100,120,140,160,200)
ggplot(data=distances, aes(x=distance)) +
	geom_histogram(col="black", fill="steelblue", breaks=tick_marks) +
	scale_x_continuous(breaks=tick_marks) +
	scale_y_continuous(breaks=seq(0,60,10)) +
	labs(title="Frequency Histogram for the Distance Data", x="Distance Traveled\n      (1,000 miles)", y="frequency")

# Constructing the boxplot for the data.
(distances.boxplot <- ggplot(data=distances, aes(x=distance)) + 
	geom_boxplot(col="blue", fill="skyblue") +
	scale_x_continuous(breaks=seq(0,200,25)) +
	scale_y_discrete() +
	labs(title="Boxplot for the Distance Data", x="Distance Traveled\n      (1,000 miles)"))


# -- Extra --

# Creating the table.
library("ggpubr")
summary(distances$distance)
stats <- c("Min.   :  3.78", "1st Qu.: 69.48", "Median :101.97", "Mean  : 96.42", "3rd Qu.:122.06", "Max.   :196.58")
stats.table <- data.frame(stats)
names(stats.table) <- c("distance")
attr(stats.table, "row.names") <- rep("",6)
table <- ggtexttable(stats.table)

# Combining the table and the boxplot.
ggarrange(distances.boxplot, table)
