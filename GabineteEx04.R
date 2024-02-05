# Gabinete, Keith Ginoel S.
# CMSC 150 - B3L
# October 05, 2023

# Exercise 04 - Solutions to Systems of Linear Equations

sourcePath = getwd()
sourcePath = paste(sourcePath, "/GabineteEx03.R", sep="")
source(sourcePath)

# Part I. Create two R functions named GaussianMethod and GaussJordanMethod. Both
# functions accept a labelled list containing two items, variables and augcoeffmatrix,
# that is, the return value of your Ex03 function: AugCoeffMatrix(...).

# ========================================================================================================
# ========================================================================================================
# accepts a labelled list containing two items, variables and augcoeffmatrix, as a parameter
GaussianMethod = function (augCoeffMatrix) {
  # extract the augmented coefficient matrix from the passed labelled list
  a = augCoeffMatrix$augcoeffmatrix
  
  # perform forward elimination on the extracted augmented coefficient matrix
  a = ForwardElimination(a)
  
  # check if there's no solution found
  if (all(is.na(a))) {
    print("There's no solution found!")
    return (NA)
  }
  
  # perform backward substitution on the resulting matrix from the previous forward elimination function to find the solution for the given system of linear equations
  solutionVector = BackwardSubstitution(a)
  
  # return the results as labelled list
  returnedList = list (variables=augCoeffMatrix$variables, augcoeffmatrix=a, solution=solutionVector)
  return(returnedList)
}

# ********************************************************************************************************
# function for forward elimination
ForwardElimination = function (augCoeffMatrix) {
  # for code simplicity/clarity
  a = augCoeffMatrix          
  n = nrow(a)                 
  row = 0
  for ( i in 1:(n-1) ) {
    # find pivot row
    pivotRow = 0
    pivotRow = findPivotRow(i, a)
    
    # if there's no valid pivot row found
    if (pivotRow == 0 || a[pivotRow, i] == 0) {
      # no unique solution exists, STOP
      return (NA)
    }
    
    # do partial pivoting
    if (i != pivotRow) {
      a = swapRows(i, pivotRow, a)
    }
    
    
    for (j in (i+1):n) {
      # find the pivot element
      pivotElement = a[i, i]
      
      # compute for the multiplier
      multiplier = (a[j, i])/pivotElement
      
      # find the normalized window
      normalizedRow = multiplier * a[i,]
      
      # modify the values of the jth row by subtracting the normalizedRow from it
      a[j,] = a[j,] - normalizedRow
    }
  }
  
  # return the transformed matrix
  return (a)
}

# ********************************************************************************************************
# function for backward substitution
BackwardSubstitution = function (augCoeffMatrix) {
  # for code simplicity/clarity
  a = augCoeffMatrix         
  n = nrow(a)                 
  
  x = c(1:n)                  # initialize the solution vector
  b = as.vector(a[,n+1])      # get the constants vector
  
  # compute for the solution of the nth row equation
  x[n] = a[n, n+1]/a[n,n]
  
  # compute for the solutions of the succeeding equations (upward direction)
  for (i in (n-1):1) {
    # compute for the sum of known terms/expressions in the given equation
    sumOfKnowns = 0
    for (j in (i+1):n) {
      sumOfKnowns = sumOfKnowns + (a[i, j] * x[j])
    }
    
    # compute for the value (solution) of the unknown term/variable
    x[i] = (b[i] - sumOfKnowns) / a[i,i]
  }
  
  # return the solution vector
  return(x)
}


# ********************************************************************************************************
# function to find the pivot row in a given matrix
findPivotRow = function (col, augCoeffMatrix) {
  # for code simplicity/clarity
  a = augCoeffMatrix
  j = col
  n = nrow(a)
  
  maxValue = 0 # will hold the max value among all the other values in a specific column of the given matrix
  pivotRow = 0 # will hold the found pivot row
  
  # find the row of the element with the highest absolute value among all elements in the specified column of the given matrix
  for (i in j:n) {
    if (abs(a[i, j]) > abs(maxValue)) {
      pivotRow = i
      maxValue = a[i, j]
    }
  }
  
  # return the pivot row
  return(pivotRow)
}

# ********************************************************************************************************
# a function that swaps two specified rows in a given matrix
swapRows = function (i, pivotRow, a) {
  # for code simplicity/clarity
  n = nrow(a)
  temp = a[i,]    # will temporarily hold the vector content of the ith row
  
  # here's where swapping takes place
  for (index in 1:n) {
    if (index == i) {
      a[i,] = a[pivotRow,]
      a[pivotRow,] = temp
    }
  }

  # return the modified matrix
  return(a)
}

# ========================================================================================================
# ========================================================================================================
# accepts a labelled list containing two items, variables and augcoeffmatrix, as a parameter
GaussJordanMethod = function (augCoeffMatrix) {
  # for code simplicity/clarity
  a = augCoeffMatrix$augcoeffmatrix
  n = nrow(a)
  
  # traverse through the elements of the matrix
  for (i in 1:n) {
    # find the pivot row
    pivotRow = 0
    pivotRow = findPivotRow(i, a)
    
    # if there's no valid pivot row found
    if (pivotRow == 0 || a[pivotRow, i] == 0) {
      # no unique solution exists, STOP
      print("There's no solution found!")
      return (NA)
    }
    
    # do partial pivoting
    if (i != pivotRow) {
      a = swapRows(i, pivotRow, a)
    }
    
    a[i,] = a[i,]/a[i,i]
    
    for (j in 1:n) {
      if (i==j) {
        next
      }
      normalizedRow = a[j,i]*a[i,]
      a[j,] = a[j,] - normalizedRow
      
    }
  }
  
  solutionVector = as.vector(a[,n+1])
  
  # return the results as labelled list
  returnedList = list (variables=augCoeffMatrix$variables, augcoeffmatrix=a, solution=solutionVector)
  return(returnedList)
}

# ========================================================================================================
# ========================================================================================================

# Part 2:
# Problem #1
EA <- function (x1, x2, x3, x4, x5, x6, x7, x8) 8000  * x1 + 4500 * x2 + 4000 * x3 + 3000 * x4 + 2000 * x5 + 1000 * x6 + 900  * x7 + 250 * x8 + -143145000;
EB <- function (x1, x2, x3, x4, x5, x6, x7, x8) 7800  * x1 + 6500 * x2 + 5800 * x3 + 0    * x4 + 3100 * x5 + 1600 * x6 + 1000 * x7 + 300 * x8 + -158870000;
EC <- function (x1, x2, x3, x4, x5, x6, x7, x8) 10000 * x1 + 0    * x2 + 3100 * x3 + 0    * x4 + 2600 * x5 + 1300 * x6 + 850  * x7 + 150 * x8 + -108440000;
ED <- function (x1, x2, x3, x4, x5, x6, x7, x8) 5200  * x1 + 3700 * x2 + 3100 * x3 + 2700 * x4 + 2400 * x5 + 1800 * x6 + 1200 * x7 + 450 * x8 + -143805000;
EE <- function (x1, x2, x3, x4, x5, x6, x7, x8) 7700  * x1 + 7100 * x2 + 0    * x3 + 5700 * x4 + 5100 * x5 + 1300 * x6 + 950  * x7 + 95  * x8 + -181390500;
EF <- function (x1, x2, x3, x4, x5, x6, x7, x8) 9300  * x1 + 8700 * x2 + 6100 * x3 + 5100 * x4 + 4000 * x5 + 1000 * x6 + 700  * x7 + 70  * x8 + -209273000;
EG <- function (x1, x2, x3, x4, x5, x6, x7, x8) 6000  * x1 + 0    * x2 + 5000 * x3 + 4300 * x4 + 3000 * x5 + 1900 * x6 + 1400 * x7 + 920 * x8 + -174388000;
EH <- function (x1, x2, x3, x4, x5, x6, x7, x8) 8500  * x1 + 3700 * x2 + 4200 * x3 + 3900 * x4 + 3500 * x5 + 2400 * x6 + 1000 * x7 + 250 * x8 + -183065000;

ticketSystem = list(EA, EB, EC, ED, EE, EF, EG, EH)
tsMatrix = AugCoeffMatrix(ticketSystem)


print(tsMatrix)
tsGauss = GaussianMethod(tsMatrix)
tsGaussJordan = GaussJordanMethod(tsMatrix)

# Part 2:
# Problem #2
E1 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9)  4 * x1 + -1 * x2 +  0 * x3 + -1 * x4 +  0 * x5 +  0 * x6 +  0 * x7 +  0 * x8 +  0 * x9 + -80;
E2 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9) -1 * x1 +  4 * x2 + -1 * x3 +  0 * x4 + -1 * x5 +  0 * x6 +  0 * x7 +  0 * x8 +  0 * x9 + -30;
E3 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9)  0 * x1 + -1 * x2 +  4 * x3 +  0 * x4 +  0 * x5 + -1 * x6 +  0 * x7 +  0 * x8 +  0 * x9 + -80;
E4 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9) -1 * x1 +  0 * x2 +  0 * x3 +  4 * x4 + -1 * x5 +  0 * x6 + -1 * x7 +  0 * x8 +  0 * x9 + -50;
E5 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9)  0 * x1 + -1 * x2 +  0 * x3 + -1 * x4 +  4 * x5 + -1 * x6 +  0 * x7 + -1 * x8 +  0 * x9 + 0;
E6 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9)  0 * x1 +  0 * x2 + -1 * x3 +  0 * x4 + -1 * x5 +  4 * x6 +  0 * x7 +  0 * x8 + -1 * x9 + -50;
E7 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9)  0 * x1 +  0 * x2 +  0 * x3 + -1 * x4 +  0 * x5 +  0 * x6 +  4 * x7 + -1 * x8 +  0 * x9 + -120;
E8 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9)  0 * x1 +  0 * x2 +  0 * x3 +  0 * x4 + -1 * x5 +  0 * x6 + -1 * x7 +  4 * x8 + -1 * x9 + -70;
E9 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9)  0 * x1 +  0 * x2 +  0 * x3 +  0 * x4 +  0 * x5 + -1 * x6 +  0 * x7 + -1 * x8 +  4 * x9 + -120;

temperatureSystem = list(E1, E2, E3, E4, E5, E6, E7, E8, E9)
tempMatrix = AugCoeffMatrix(temperatureSystem)

print(tempMatrix)
tempGauss = GaussianMethod(tempMatrix)
tempGaussJordan = GaussJordanMethod(tempMatrix)


# If there's no solution found sample:
# EA <- function (x1, x2, x3, x4, x5, x6, x7, x8) 0  * x1 + 4500 * x2 + 4000 * x3 + 3000 * x4 + 2000 * x5 + 1000 * x6 + 900  * x7 + 250 * x8 + -143145000;
# EB <- function (x1, x2, x3, x4, x5, x6, x7, x8) 0  * x1 + 6500 * x2 + 5800 * x3 + 0    * x4 + 3100 * x5 + 1600 * x6 + 1000 * x7 + 300 * x8 + -158870000;
# EC <- function (x1, x2, x3, x4, x5, x6, x7, x8) 0 * x1 + 0    * x2 + 3100 * x3 + 0    * x4 + 2600 * x5 + 1300 * x6 + 850  * x7 + 150 * x8 + -108440000;
# ED <- function (x1, x2, x3, x4, x5, x6, x7, x8) 0  * x1 + 3700 * x2 + 3100 * x3 + 2700 * x4 + 2400 * x5 + 1800 * x6 + 1200 * x7 + 450 * x8 + -143805000;
# EE <- function (x1, x2, x3, x4, x5, x6, x7, x8) 0  * x1 + 7100 * x2 + 0    * x3 + 5700 * x4 + 5100 * x5 + 1300 * x6 + 950  * x7 + 95  * x8 + -181390500;
# EF <- function (x1, x2, x3, x4, x5, x6, x7, x8) 0  * x1 + 8700 * x2 + 6100 * x3 + 5100 * x4 + 4000 * x5 + 1000 * x6 + 700  * x7 + 70  * x8 + -209273000;
# EG <- function (x1, x2, x3, x4, x5, x6, x7, x8) 0  * x1 + 0    * x2 + 5000 * x3 + 4300 * x4 + 3000 * x5 + 1900 * x6 + 1400 * x7 + 920 * x8 + -174388000;
# EH <- function (x1, x2, x3, x4, x5, x6, x7, x8) 0  * x1 + 3700 * x2 + 4200 * x3 + 3900 * x4 + 3500 * x5 + 2400 * x6 + 1000 * x7 + 250 * x8 + -183065000;
# 
# ticketSystem = list(EA, EB, EC, ED, EE, EF, EG, EH)
# tsMatrix = AugCoeffMatrix(ticketSystem)
# 
# print(tsMatrix)
# tsGauss = GaussianMethod(tsMatrix)
# tsGaussJordan = GaussJordanMethod(tsMatrix)
