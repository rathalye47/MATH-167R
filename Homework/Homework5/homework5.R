# Rohan Athalye
# Homework 5: Factors and Stack/Unstack Data

# -- Problem 1 --

# Constructing a character vector that contains the elements of the 1st column "Degree of Suppression".
(Opinions <- rep(c("None", "Very little", "Moderate", "Severe"), c(8, 8, 10, 14)))

# Creating a factor from the character vector.
(Opinions.levels.attempted <- factor(Opinions))

# Determining the levels of the factor.
# By default, the levels of a factor are determined by the distinct values in the data.
# The levels of the factor are "Moderate", "None", "Severe", and "Very little".

# Adding 1 more attribute "ordered".
Opinions.levels <- c("None", "Very little", "Moderate", "Severe")
(Opinions.levels.attempted <- factor(Opinions, levels=Opinions.levels, ordered=T))

# Checking the structure of the object.
str(Opinions.levels.attempted)


# -- Problem 2 --

# Importing the data into R and creating a dataframe called Music.unstack.
Music.unstack <- read.table("music&typing.txt", header=FALSE, sep=",", skip=1)
names(Music.unstack) <- c("No Music", "Hard Rock", "Classical")
Music.unstack

# Converting the unstacked data to a stacked dataframe named Music.stack.
Music.stack <- stack(Music.unstack)

# Giving the names "Assessment Score" and "Background Music" to the columns.
names(Music.stack) <- c("Assessment Score", "Background Music")
Music.stack

# Checking the structure of the new dataframe.
str(Music.stack)


# -- Problem 3 --

# Importing the data into R and creating a dataframe called Package.
Package <- read.table("package.txt", header=TRUE, sep=",")

# Checking the types of the columns in the dataframe Package.
sapply(Package, class)

# Unstacking Delivery time by Firm.
(Package.byFirm <- unstack(Package, Delivery.time~Firm))

# Checking the structure of the new dataframe.
str(Package.byFirm)

# Unstacking Delivery time by Time of day.
(Package.byTime <- unstack(Package, Delivery.time~Time.of.day))

# Checking the structure of the new dataframe.
str(Package.byTime)

# Finding the mean delivery time for each firm.
Group_Means1 <- aggregate(Package$Delivery.time, by=list(Package$Firm), FUN=mean)
names(Group_Means1) <- c("Firm", "Average Delivery Time")
Group_Means1

# Finding the mean delivery time for each time of day.
Group_Means2 <- aggregate(Package$Delivery.time, by=list(Package$Time.of.day), FUN=mean)
names(Group_Means2) <- c("Time of Day", "Average Delivery Time")
Group_Means2

# Finding the mean delivery time for each combination of firm and time of day.
Group_Means3 <- aggregate(Package$Delivery.time, by=list(Package$Firm, Package$Time.of.day), FUN=mean)
names(Group_Means3) <- c("Firm", "Time of Day", "Average Delivery Time")
Group_Means3
