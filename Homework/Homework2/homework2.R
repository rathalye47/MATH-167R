# Rohan Athalye
# Homework 2: Variables and Functions

# -- Problem 1 --
x=0
y=x+1
x
y
# After printing in R, the value of x is 0, and the value of y is 1.
x=-1
# The value of y has not changed since any subsequent changes to the value of x do not affect the value of y.
# It is the object that contains the value of x + 1, at the time of assignment, that is bound to y.


# -- Problem 2 --
args(round)
# The arguments of the function round() are x and digits. 
# x is a complex vector, and digits is the number of decimal places to which x has to be rounded off.
# To get help on the function, I would write ?round in the command prompt.
round(pi, digits = 3)
# The irrational number pi approximated to 3 decimal places is 3.142.


# -- Problem 3 --
2^(-9 %% 4)
# 2^(-9 %% 4) evaluates to 8.
sqrt(64)
# sqrt(64) evaluates to 8.
log(32, base=2)
# log(32, base=2) evaluates to 5.


# -- Problem 4 --
G <- function(u) {
if (u <= 1) u
else if ((u > 1) & (u <= 2)) u + 1
else if (u > 2) u + 2}
G(0)
# G(0) = 0.
G(2)
# G(2) = 3.
G_3 <- G(3)
# For R to display the value of the new variable, I would need to surround the assignment with parentheses.
(G_3 <- G(3))


# -- Problem 5 --
ls()
# The environment is R_GlobalEnv, the global environment that is the top level environment available to us at the R command prompt.
# The variable "u" from problem 4 is not in my current environment.
# This is because the variable "u" is stored in the new environment created by function G inside the global environment.


# -- Problem 6 --
rm(x, y)
ls()
# The objects stored in the current environment are "G" and "G_3".
