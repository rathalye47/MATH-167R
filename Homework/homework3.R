# Rohan Athalye
# Homework 3: Vectors and Lists

# -- Problem 1 --
Opinions <- c("agree", "agree", "disagree", "neutral", "disagree", "neutral", "agree")


# -- Problem 2 --
neutral_count <- 0
for (i in 1:7) {
	if (Opinions[i] == "neutral") neutral_count <- neutral_count + 1
	}
neutral_count
# After printing neutral_count, there are 2 "neutral" responses in the vector Opinions.


# -- Problem 3 --
Opinions.coded = numeric(7)
for (i in 1:7) {
	if (Opinions[i] == "disagree") Opinions.coded[i] <- -1
	else if (Opinions[i] == "neutral") Opinions.coded[i] <- 0
	else if (Opinions[i] == "agree") Opinions.coded[i] <- 1
	}
Opinions.coded


# -- Problem 4 --
Roster <- list(c("Mary", "John", "Lisa"), c(19, 20, 18))
names(Roster) <- c("Student", "Age")
str(Roster)

# Accessing the entire vector Student.
Roster$Student

# Accessing the individual components of the vector Age.
Roster$Age[3]

# Accessing the components of the list Roster using single brackets.
# This returns a list even if only one component is referenced.
Roster[2]

# Accessing the components of the list Roster using double brackets.
# This returns a vector.
Roster[[1]]

# Accessing the individual components of the vector Age using double brackets and single brackets.
Roster[[2]][1]

Roster$Student[3] <- "Ann"
Roster$Standing <- c("senior", "freshman", "sophomore")
str(Roster)
