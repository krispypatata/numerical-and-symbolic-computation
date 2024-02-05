# Gabinete, Keith Ginoel S.
# CMSC 150 - B3L
# Exercise No. 6

sourcePath = getwd()
sourcePath = paste(sourcePath, "/GabineteEx04.R", sep="")
source(sourcePath)

# Part 1. Create an R function named PolynomialRegression which accepts an integer and
# a list as inputs. The integer will specify the order of the polynomial (≥ 1), and the list will
# contain the two vectors for the data points, one for the independent variable and another
# for the dependent variable. It should solve for the nth order polynomial that will model the
# data points.

# The function must return the following variables in a list, with the following labels:
#   augcoeffmatrix: the original augmented coefficient matrix;
#   coefficients: the vector of coefficients of the polynomial;
#   polynomial_string: the string version of the polynomial;
#   polynomial_function: the function version of the polynomial.


PolynomialRegression = function (order, data_points) {
  # degree of the polynomial regression; for code clarity
  n = order
  
  # extract the contents of the data_points list
  # data point for the independent variable
  x = data_points[[1]]
  
  # data point for the dependent variable
  y = data_points[[2]]
  
  # check if the length of the two data points are equal; if not, then don't proceed to the execution of the PolynomialRegression function and return NA
  if (length(x) != length(y)) {
    print("Length of data point vectors are not Equal.")
    print("Can't proceed to Polynomial Regression")
    return (NA)
  }
  
  # for code clarity, represent the length of the two data points as d
  d = length(x)    # remember: we already know at this point that length(x) == length(y)
  
  # ==================================================================================================================
  # create some necessary matrices
  # ==================================================================================================================
  # coefficient matrix (n+1)x(n+1) matrix
  coefficientMatrix = matrix(
    0,
    nrow = n+1,
    ncol = n+1,
    byrow = TRUE
  )
  
  # fill in the coefficientMatrix with the correct values
  for (row in 1:(n+1)) {
    for (col in 1:(n+1)) {
      # compute for the value to be put inside a matrix cell; coefficientMatrix[row,col]
      sum = 0
      for (content in x) {
        sum = sum + content^(row-1 + col-1)
      }
      
      # update the cell's value
      coefficientMatrix[row,col] = sum
    }
  }
  # print(coefficientMatrix)   # for checking
  
  # ==================================================================================================================
  # constant matrix (n+1)x1 matrix
  constantMatrix = matrix (
    0,
    nrow = n+1,
  )
  
  # fill in the coefficientMatrix with the correct values
  for (row in 1:(n+1)) {
    sum = 0
    # compute for the value to be put inside a matrix cell; constantMatrix[row]
    for (i in 1:d) {
      sum = sum + ( x[i]^(row-1) ) * y[i]
    }
    
    # update the cell's value
    constantMatrix[row] = sum
  }
  # print(constantMatrix)     # for checking
  
  # ==================================================================================================================
  # augmented coefficient matrix (returned item #1)
  augcoeffmatrix = cbind(coefficientMatrix, constantMatrix)
  
  # store the created augmented coefficient matrix in a labelled list (wil be needed to use the function for Gaussian or Gauss-Jordan elimination methods)
  # the code below for the variables vector is optional
  variables = c()
  for (count in 1:n) {
    variable = "x"
    variable = paste(variable, count, sep="")
    variables = c(variables, variable)
  }
  
  augcoeffmatrixList = list (variables=variables, augcoeffmatrix=augcoeffmatrix) 
  
  # ==================================================================================================================
  # compute for the coefficients of the polynomial by using either the Gaussian Elimination Method or the Gauss-Jordan Method
  result_GaussianElimination = GaussianMethod(augcoeffmatrixList)
  # print(result_GaussianElimination)  # for checking
  
  # coefficients (returned item #2)
  coefficients = result_GaussianElimination$solution
  
  # for checking
  # print(result_GaussianElimination$solution)
  
  # ==================================================================================================================
  # for the polynomial_string (returned item #3)
  polynomial_string = "function(x)"
  
  # constructing the polynomial with the computed coefficients
  for (index in 1:length(coefficients)) {
    polynomial_string = paste(polynomial_string, coefficients[index])
    
    # add variables beside the coefficients [excluding the first coefficient (as it represents the constant of the polynomila)]
    if (index!=1) {
      polynomial_string = paste(polynomial_string, "* x ^")
      polynomial_string = paste(polynomial_string, index-1)
    }
    
    # add an addition operation "+" between each expression
    if (index!= length(coefficients)) {
      polynomial_string = paste(polynomial_string, " + ")
    }
  }
  
  # for checking
  # print(polynomial_string)
  
  # convert the string version of the polynomial to an expression (by calling the pare() function)
  # get the function version of the polynomial by using the eval() function to the parsed string
  # polynomial_function (returned item #3)
  polynomial_function = eval(parse(text = polynomial_string))     
  
  # ==================================================================================================================
  # create a labelled list to return the results of the Polynomial Regression function (as compliance to the requirements of the program)
  returnedList = list (augcoeffmatrix=augcoeffmatrix, coefficients=coefficients, polynomial_string=polynomial_string, polynomial_function=polynomial_function)
  return (returnedList)
}

# =====================================================================================================================================================
# =====================================================================================================================================================
# sample input
a = c(1,3,6,7)
b = c(10, 20, 19, 33)

sample_data = list (a,b)
samplePR = PolynomialRegression(3, sample_data)
sampleCubicModel = lm(b ~ poly(a, 3, raw=TRUE), data = list_data)

print(samplePR)
print(sampleCubicModel$coefficients)
# ==============================================================================================
# Part 2. 
# In an insurance company, the amount of your return of investment depends on the
# amount that you paid over a period of time. The table below shows the amount of
# investment paid every year, and the estimated return of investment after 10 years.

# i                         1       2       3       4       5       6       7       8       9       10
# Paid (in thousand)        20      20      25      27      30      30      33      35      35      40
# ROI (in ten thousand)     8.75  9.43   12.87   14.24   16.89   18.94   25.48   30.11   36.07   51.27

# Find an appropriate polynomial for the data set as presented, for degrees 1-3. Use the
# code that you have created in finding the solution, and the built in R functions to check
# your answer.
# 
amount_paid = c(20, 20, 25, 27, 30, 30, 33, 35, 35, 40) * 10^3
print(amount_paid)
amount_roi = c(8.75, 9.43, 12.87, 14.24, 16.89, 18.94, 25.48, 30.11, 36.07, 51.27) * 10^4
print(amount_roi)
investment_data = list (amount_paid, amount_roi)

# ==============================================================================================
# Polynomial Regression for degree 1
# using the DIY PolynomialRegression Function
insurancePR_deg1 = PolynomialRegression(1, investment_data)
print(insurancePR_deg1$coefficients)

# using the built-in R function
linearModel = lm(amount_roi ~ amount_paid, data = investment_data)
print(linearModel$coefficients)

# ==============================================================================================
# Polynomial Regression for degree 2
# using the DIY PolynomialRegression Function
insurancePR_deg2 = PolynomialRegression(2, investment_data)
print(insurancePR_deg2$coefficients)

# using the built-in R function
quadraticModel = lm(amount_roi ~ poly(amount_paid, 2, raw=TRUE), data = investment_data)
print(quadraticModel$coefficients)

# ==============================================================================================
# Polynomial Regression for degree 3
# using the DIY PolynomialRegression Function
insurancePR_deg3 = PolynomialRegression(3, investment_data)
print(insurancePR_deg3$coefficients)

# using the built-in R function
cubicModel = lm(amount_roi ~ poly(amount_paid, 3, raw=TRUE), data = investment_data)
print(cubicModel$coefficients)

# plotting
# plot(investment_data[[1]], investment_data[[2]], pch = 20, col = "red", main = "Amount of ROI vs. Amount Paid Over Time", xlab = "Amount Paid Over Time", ylab = "Amount of ROI")
# lines(investment_data[[1]], predict(linearModel), col = "blue")
# lines(investment_data[[1]], predict(quadraticModel), col = "green")
# lines(investment_data[[1]], predict(cubicModel), col = "orange")
