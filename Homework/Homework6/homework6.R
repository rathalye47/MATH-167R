# Rohan Athalye
# Homework 6: Matrices and Packages

# -- Problem 1 --

# Creating the matrix.
(A <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), nrow=3, ncol=4, byrow=T))

# Removing the rightmost column of the matrix and displaying the content of the matrix.
(B <- A[, -4])


# -- Problem 2 --

# Creating the matrix.
Cans <- matrix(c(31, 68, 17, 21, 13, 19, 47, 30, 19, 10, 33, 26, 16, 14, 11), nrow=3, ncol=5, byrow=T)

# Naming the rows and columns of the matrix.
rownames(Cans) <- rownames(Cans, do.NULL=FALSE, prefix="Production Line ")
nonconformity.names <- c("Blemish", "Crack", "Location", "Missing", "Other")
colnames(Cans) <- nonconformity.names
Cans

# Displaying the content of Production Line 2.
Cans["Production Line 2", , drop=FALSE]

# Finding the total of each row.
row.total <- rowSums(Cans)

# Adding the resulting vector as the rightmost column of matrix Cans.
Cans <- cbind(Cans, row.total)

# Naming the column "Sample Size".
colnames(Cans)[6] <- "Sample Size"
Cans

# Finding the total of each column of the updated matrix.
col.total <- colSums(Cans)

# Adding the resulting vector as the bottom row of the updated matrix Cans.
Cans <- rbind(Cans, col.total)

# Naming the row "Total".
rownames(Cans)[4] <- "Total"
Cans

# Displaying the cumulative sample size for all three production lines.
Cans["Total", "Sample Size", drop=FALSE]


# -- Problem 3 --

# Installing the packages readxl, writexl, ggplot2.
install.packages("readxl")
install.packages("writexl")
install.packages("ggplot2")


# -- Problem 4 --

# Using read_excel function to download the data in crime_rate.xlsx.
library("readxl")
(crime_rates <- read_excel("crime_rate.xlsx"))

# Making sure the column names aren't changed.
colnames(crime_rates)
# The column names are "State" and "Violent Crime".

# Checking the column types.
sapply(crime_rates, class)
# The type of column "State" is "character", and the type of column "Violent Crime" is "numeric".
