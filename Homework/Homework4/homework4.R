# Rohan Athalye
# Homework 4: Dataframes and Patterns

# -- Problem 1 --

# Creating a data set Air.Pollution.DataSet.
Air.Pollution.DataSet <- scan("air_pollution.txt", skip=1, what=list("", "", numeric(0), numeric(0)), quiet=T, sep=",")

# Creating a dataframe with 4 columns.
Air.Pollution <- as.data.frame(Air.Pollution.DataSet)

# Extracting the column names from air_pollution.txt.
Air.Pollution.names <- scan("air_pollution.txt", nlines=1, what="", quiet=T, sep=",")

# Give these names to the columns of the dataframe.
names(Air.Pollution) <- Air.Pollution.names
Air.Pollution

# Checking the types of each column in the dataframe.
# We can see that the columns "County FIPS Code" and "2010 Population" are numeric.
sapply(Air.Pollution, class)


# -- Problem 2 --

# Adding 1 more column to dataframe Air.Pollution.
Air.Pollution <- cbind(Air.Pollution, test=c(65, NA, 92, 97, 104, 66, NA, NA, 124))

# Changing the name of the new column.
names(Air.Pollution)[names(Air.Pollution)=="test"] <- "PM25 (mg/m3)"
Air.Pollution

# Checking the types of each column in the dataframe.
# We can see that the column "PM25 (mg/m3)" is numeric.
sapply(Air.Pollution, class)


# -- Problem 3 --

# Confirming the types of column data.
sapply(Air.Pollution, class)


# -- Problem 4 --

# Adding 1 more row to the dataframe.
(Air.Pollution <- rbind(Air.Pollution, list("California", "Glenn County", 6021, 28122, 34)))


# -- Problem 5 --

# Checking the types of the columns in the updated dataframe.
sapply(Air.Pollution, class)

# The types of "State" and "County" are character, and the types of "County FIPS Code", "2010 Population", and "PM25 (mg/m3)" are numeric.
# The columns "State" and "County" are of character type since they have strings, which are sequences of characters enclosed in quotes, as elements.
# The columns "County FIPS Code", "2010 Population", and "PM25 (mg/m3)" are of numeric type since it is the default class for numbers, even if they don't have a fractional part.
# Even though we have NA values in our dataframe in the column "PM25 (mg/m3)", there is no extra space added, so that is why the type of "PM25 (mg/m3)" is still numeric.
# If there was an extra space added, the type of "PM25 (mg/m3)" would be character.


# -- Problem 6 --

# Saving the dataframe in a new file called air_pollution_updated.txt.
write.table(Air.Pollution, file="air_pollution_updated.txt", quote=FALSE, row.names=FALSE)


# -- Problem 7 --
rep(c("poor", "satisfactory", "good"), c(5, 6, 7))
