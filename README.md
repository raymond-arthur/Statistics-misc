# Fibonacci Sequence and the Mile-to-Km Ratio

## Project Main Goal

In R, build an array such that we can analyze the Fibonacci sequence and make statistical comparisons to the mile:kilometer ratios.

### General Notes and Information

The most common form of the Fibonacci sequence is the iterative formula as follows:

```math
F(0) = F(1) = 1
```
```math
with \\
F(n) = F(n-1) + F(n-2)
```
```math
for \ n > 1
```

We can recognize this as the familiar sequence:

```
1, 1, 2, 3, 5, 8, 13, ...
```

However, the Fibonacci sequence has a closed-form approximation for the nth Fibonacci number as follows:

```math
F(n) = \frac{{\phi^n - (1-\phi)^n}}{{\sqrt{5}}}
```


Where n is the nth integer in the sequence and &phi; is the golden ratio given by 
```math
&phi; = \frac{{1 + \sqrt{5}}}{2}.
```

We note that &phi; approximately equals 1.61803.

Johannes Kepler showed that, for the series F(n) as above, F(n) converges towards &phi;:

```math
\lim_{n \to \infty} \frac{F(n+1)}{F(n)} = \phi
```

We also note that the mile:km ratio is defined as:

```math
mile = 1.609344 * km
```

We therefore should be able to conclude that as a general approximation:

```math
mile = \phi * km
```

In order to do this in R, we first define phi and build the Fibonacci closed-form equation as follows:

```r
phi <- (1 + sqrt(5)) / 2
```

The function `FibClosedform`:

```r
FibClosedform <- function(n, p) {
  return(round(
    (phi^n - (1 - phi)^n) / sqrt(5))
    )
}
```

We then build the iterative sequence `FibIterative`:

```r
FibIterative <- function(n) {
  fib <- numeric(n)
  fib[1] <- 1 #start counting at 1 and not 0
  if (n > 1) fib[2] <- 1
  for (i in 3:n) {
    fib[i] <- fib[i-1] + fib[i-2]
  }
  return(fib)
}
```

We make two important notes here:

- the sequence will start indexed at 0 and thus will calculate F(0)=0 instead of F(0)=1, and this can be fixed easily by defining F(0) and F(1).
- We therefore need to calculate the sequence for $i \in (3,n)$
  

We can easily check that the two functions are indexing $n$ the same way by picking a small number for $n$ and checking the outputs of the two functions as follows:

```r
#Check for a small value of p that the last entry in the Fib_iterative is the same as the value in FibClosedForm
#We choose to check for n=7
cat("The 7th digt as calculated by the closed-form equation is:", FibClosedform(7, p))
#Look at the 7th value in the iterative array
#Call function to obtain the array, produce the first 10 digits of the sequence
Fibonacci_iterativesequence <- FibIterative(7)
#Define n=7
Fib_it_7 <- Fibonacci_iterativesequence[7]
cat("The 7th digit as calculated by the iterative equation is:", Fib_it_7)
```

The console then prints the 7th value for both and we can see that both are 13.

We now look to see if any of the large values of $n$ have differing values for FibIterative and FibClosedform. We can do this by creating a table of the first 100 values of each function and getting the percent error as:

$$
\text{{\% error for a given n}} = \frac{\text{{|FibIterative(n) - FibClosedform(n)|}}}{\text{{FibIterative(n)} }}
$$

```r
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
names(result_df) <- c("n-value", "FibIterative", "FibClosedForm", "Percent_Error")
```

result_df will tell us for which values of n the two methods provide different results while keeping track of the respective n value.

result_df prints as follows:

|     | n-value | FibIterative | FibClosedForm | Percent_Error |
| --- | --- | --- | --- | --- |

| 1   | 71  | 3.080615e+14 | 3.080615e+14 | 3.246105e-13 |
| --- | --- | --- | --- | --- |
| 2   | 72  | 4.984540e+14 | 4.984540e+14 | 2.006203e-13 |
| 3   | 73  | 8.065155e+14 | 8.065155e+14 | 2.479803e-13 |
| 4   | 74  | 1.304970e+15 | 1.304970e+15 | 2.298904e-13 |
| 5   | 75  | 2.111485e+15 | 2.111485e+15 | 2.368002e-13 |
| 6   | 76  | 3.416455e+15 | 3.416455e+15 | 2.634310e-13 |
| 7   | 77  | 5.527940e+15 | 5.527940e+15 | 2.532589e-13 |
| 8   | 78  | 8.944394e+15 | 8.944394e+15 | 2.683245e-13 |
| 9   | 79  | 1.447233e+16 | 1.447233e+16 | 2.763894e-13 |
| 10  | 80  | 2.341673e+16 | 2.341673e+16 | 2.562271e-13 |
| 11  | 81  | 3.788906e+16 | 3.788906e+16 | 2.744855e-13 |
| 12  | 82  | 6.130579e+16 | 6.130579e+16 | 2.740361e-13 |
| 13  | 83  | 9.919485e+16 | 9.919485e+16 | 2.903376e-13 |
| 14  | 84  | 1.605006e+17 | 1.605006e+17 | 2.990642e-13 |
| 15  | 85  | 2.596955e+17 | 2.596955e+17 | 2.957310e-13 |
| 16  | 86  | 4.201961e+17 | 4.201961e+17 | 2.893887e-13 |
| 17  | 87  | 6.798916e+17 | 6.798916e+17 | 3.012245e-13 |
| 18  | 88  | 1.100088e+18 | 1.100088e+18 | 2.908859e-13 |
| 19  | 89  | 1.779979e+18 | 1.779979e+18 | 3.020260e-13 |
| 20  | 90  | 2.880067e+18 | 2.880067e+18 | 3.022152e-13 |
| 21  | 91  | 4.660047e+18 | 4.660047e+18 | 3.076364e-13 |
| 22  | 92  | 7.540114e+18 | 7.540114e+18 | 2.987753e-13 |
| 23  | 93  | 1.220016e+19 | 1.220016e+19 | 3.021600e-13 |
| 24  | 94  | 1.974027e+19 | 1.974027e+19 | 2.904924e-13 |
| 25  | 95  | 3.194043e+19 | 3.194043e+19 | 3.077729e-13 |
| 26  | 96  | 5.168071e+19 | 5.168071e+19 | 3.170235e-13 |
| 27  | 97  | 8.362114e+19 | 8.362114e+19 | 3.134901e-13 |
| 28  | 98  | 1.353019e+20 | 1.353019e+20 | 3.148397e-13 |
| 29  | 99  | 2.189230e+20 | 2.189230e+20 | 3.143242e-13 |
| 30  | 100 | 3.542248e+20 | 3.542248e+20 | 3.145211e-13 |

We can take the arithmetic mean of the percent error by simply using the mean() command for column 4 of results_df and printing it to console:

```r
#Create the average of the percent errors
average_percenterror <-mean(result_df[,4])

#Print the average percent error
cat("The average percent error from", result_df[1, 1], "to", result_df[30, 1], "is", average_percenterror*100,"%")
```

We see that the value for the percent error is: $2.85 *10^{-11}$% or a very negligible amount for the values of n $\in$ (70,100).

We can perform a very similar percent error calculation for the ratio of $\text{{mi}}= 1.609344\text{{ km}}$ and $\phi$ using the following:

```r
mi_km <- 1.609344
phi_ratio <- phi / mi_km
phi_ratio_percenterror <- (phi - mi_km)/phi * 100

cat("The percent error of the conversion from mi to km using the phi value is", phi_ratio_percenterror, "%.")
```

We get a percent error of $0.5370708$%.

## Conclusion
Coincidentally, the golden ratio is a great approximation for the conversion rate of miles to kilometers!
