### GENERAL INFO ###

## Main goal ##
#build an array s/t we can visualize and analyze the Fibonacci 
#sequence and make statistical comparisons to the mile:kilometer ratios

## General notes and information ##
#The Fibonacci sequence has a closed-form formula for the nth Fibonacci number as follows:
# F(n) = ((p^n) - (1-p)^n) / sqrt(5)
# where n is the nth number in the sequence
# where p is the golden ratio; p=(1+sqrt(5))/2

#The iterative formula for the Fibonacci sequence is as follows:
# Generally starting with F(0)=1, F(1)=1
# F(n) = F(n-1) + F(n-2)
# for n>1

#The km:mile ratio is as follows:
# km = (0.621371) * mi
# or more interestingly in our case:
# mi = 1.609344 * km

## Additional notes ##
#Johannes Kepler showed that, for F(n) as above, the series F(n) converges (and approach p) s/t:
# lim(n->inf) F(n+1)/F(n) = p
# where phi = (1+sqrt(5)) / 2 = 1.61803


#We therefore should be able to conclude that, as a general approximation:
# mi = phi * km




##Housekeeping
#clear all variables
rm(list = ls())
#Installing ggplot2 for plotting
#install.packages("ggplot2")
library(ggplot2)
library(tidyverse)

### Defining the Fibonacci sequence ###

##Build the Fibonacci function using the closed-form formula
#Build the value of p and store
phi <- (1 + sqrt(5)) / 2
#Build the closed-form function
FibClosedform <- function(n, p) {
  return(round(
    (phi^n - (1 - phi)^n) / sqrt(5))
    )
}

#Build the Fibonacci sequence using iterative process
FibIterative <- function(n) {
  fib <- numeric(n)
  fib[1] <- 1 #start counting at 1 and not 0
  if (n > 1) fib[2] <- 1
  for (i in 3:n) {
    fib[i] <- fib[i-1] + fib[i-2]
  }
  return(fib)
}


#Check for a small value of p that the last entry in the Fib_iterative is the same as the value in FibClosedForm
#We choose to check for n=7
cat("The 7th digt as calculated by the closed-form equation is:", FibClosedform(7, p))
#Look at the 7th value in the iterative array
#Call function to obtain the array, produce the first 10 digits of the sequence
Fibonacci_iterativesequence <- FibIterative(7)
#Define n=7
Fib_it_7 <- Fibonacci_iterativesequence[7]
cat("The 7th digit as calculated by the iterative equation is:", Fib_it_7)

#Create a matrix to store our 3 rows:
# numbers 1:100, the output of the first 100 results of FibClosedform and 100 results of FibIterative
# Create a matrix with 3 rows and 100 columns
matrix_data <- matrix(NA, nrow = 3, ncol = 100)

# Create row
row_names <- c("Integers", "FibIterative", "FibClosedform")


# Fill the matrix with the requested values
matrix_data[1, ] <- 1:100
matrix_data[2, ] <- FibIterative(100)
for (i in 1:100) {
  matrix_data[3, i] <- FibClosedform(i, phi)
}

# Create a data frame
df <- data.frame(matrix_data, row.names = row_names)

## Comparing the values of FibClosedForm and FibIterative
# Compare the values of rows 2 and 3 to see if any are different and calculate the percent error
different_columns <- which(df[2,] != df[3,])
# Since we expect some differences in the larger values, we create a data frame to capture those differences
result_df <- data.frame(Column = numeric(0), Row2_Value = numeric(0), Row3_Value = numeric(0), Percent_Error = numeric(0))

# Populate the result data frame
for (col_idx in different_columns) {
  row2_val <- df[2, col_idx]
  row3_val <- df[3, col_idx]
  
  percent_error <- abs((row2_val - row3_val) / row2_val) * 100 #calculate percent error, where row2 is the "true" value since the iterative value is not an approximation
  
  result_df <- rbind(result_df, c(col_idx, row2_val, row3_val, percent_error))  #tack on percent error
}

# Reset row names for result_df
row.names(result_df) <- NULL

# Add column names
names(result_df) <- c("Column", "FibIterative", "FibClosedForm", "Percent_Error")

# Display the result data frame
print(result_df)

#Create the average of the percent errors
average_percenterror <-mean(result_df[,4])

#Print the average percent error
cat("The average percent error from", result_df[1, 1], "to", result_df[30, 1], "is", average_percenterror)

