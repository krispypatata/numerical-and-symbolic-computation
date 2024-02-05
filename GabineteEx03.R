# Gabinete, Keith Ginoel S.
# CMSC 150 - B3L
# September 20, 2023
# Exercise No. 3: AugCoeffMatrix

# ================================================================================================================
# Create an R function named AugCoeffMatrix which takes a list which contains mathematical
# functions as input. It should create the augmented coefficient matrix for the functions and return it
# in a single list variable.
AugCoeffMatrix = function (mathList) {
  # deparse each function in the mathList
  mathList = lapply(mathList, deparse)

  # get the number of equations present in the given functions list
  n = length(mathList)
  # according to the lab handout the number of equations in a given system of linear equations
  # is also equal to the number of unknown variables

  # will contain the variables involved in the system of linear equations
  variables = as.list(1:n)   # must be of size n
  # print(length(variables)) # for checking

  # will contain the constants from each function in the list (will then be used later to create a constant matrix)
  constants = list()

  # will contain the coefficients from each function in the list (will then be used to creat a coefficient matrix)
  coefficients = list()

  # access each function in mathList
  for (listIndex in 1:length(mathList)) {
    funct = mathList[[listIndex]]
    # print(funct)  # for checking

    # extract the contents of the function currently selected
    variablesUnformatted = funct[[1]]
    coefficientsUnformatted = funct[[2]]

    # fixes the issue when a function has 6 or more unknown variables
    # will check if the deparse function returns only 2 vectors or not
    # if the deparse function returns 3 or more vectors, then, the function's coefficients component still has
    # some missing char/strings that can be found in the excess vectors
    if (length(funct)>=3) {
      for (i in 3:length(funct)) {
        coefficientsUnformatted = paste(coefficientsUnformatted, funct[[i]] )
      }
    }
    # print(coefficientsUnformatted) # for checking
    
    # perform some string manipulation to the extracted components of the function to fix their states
    functVariables = formatVariables(variablesUnformatted)
    functCoefficients = formatCoefficients(coefficientsUnformatted, functVariables)

    # get the value of b(constant) from the formatted coefficients
    functB = functCoefficients[[length(functCoefficients)]]

    # remove b from the list of formatted coefficients
    functCoefficients = functCoefficients[-length(functCoefficients)]

    # append the extracted actual coefficients of the function to the list of coefficients of the mathList
    coefficients = c(coefficients, functCoefficients)

    # append the extracted actual value of b from the function to the list of constants of the mathList
    constants = c(constants, functB)


    # check if the number of the unknown variables of
    # the mathematical functions are equal to each other
    if (length(variables) == length(functVariables)) {
      variables = c(functVariables)
    } else {
      print("Failed to get the Augmented Coefficient Matrix") # for checking
      return (NA)
    }

  } # end of for loop

  # for checking
  # print("Variables")
  # print(variables)
  # print("Coefficients")
  # print(coefficients)
  # print("Constants")
  # print(constants)

  # create the necessary matrices
  # coefficientMatrix
  coefficientMatrix = matrix(
    as.numeric(unlist(coefficients)), # converts the list to numeric vector (if needed)
    # coefficients,
    nrow = length(variables),
    ncol = length(variables),
    byrow = TRUE,
    dimname = list(1:length(variables),variables)
  )
  # for checking
  # print(coefficientMatrix)

  # constant matrix
  constantMatrix = matrix (
    as.numeric(unlist(constants))*(-1), # converts the list to numeric vector (if needed)
    # lapply(constants, "*", -1),
    nrow = length(constants),
    dimname = list(1:length(constants), "RHS")
  )
  # for checking
  # print(constantMatrix)

  augcoeffmatrix = cbind(coefficientMatrix, constantMatrix)
  # for checking
  # print(augcoeffmatrix)

  # will contain the results
  returnedList = list (variables=unlist(variables), augcoeffmatrix=augcoeffmatrix)

  # return the results
  print("SUCCESS!") # for checking
  return (returnedList)
}

# ================================================================================================================
# returns a list that contains the actual values of coefficients extracted from a given function
# will take the extracted (unformatted) coefficients of a function, and the variables (formatted) extracted from the same function as parameters
formatCoefficients = function (extractedCoefficients, variables) {
  coefficients = extractedCoefficients

  # remove all the spaces
  coefficients = gsub(" ", "", coefficients)

  # split the string with plus sign (+) as the delimiter (to separate each expression present in the given equation)
  coefficients = strsplit(coefficients, "+", fixed=TRUE)   # will return a list of length 1 containing c vector of n number of elements
  coefficients = unlist(coefficients)                      # to point the coefficients variable to the c vector with n elements

  # uncomment for checking
  # print("UNLISTED")
  # print(coefficients)
  # print(length(coefficients))

  # get the value of b from the extracted coefficients
  # b is always the last element of the character vector
  b = coefficients[length(coefficients)]

  # temporarily remove b from the coefficients character vector
  coefficients = coefficients[-length(coefficients)]

  coefficientsInOrder = c()
  # rearrange the order of coefficients in order
  for (varIndex in 1:length(variables)) {
    
    # prevents repetition of getting a coefficient for a certain variable
    repetition = 0
    for (coefIndex in 1:length(coefficients)) {
      # grepl function returns true if a given (sub)string is present in another string
      if (grepl(variables[varIndex], coefficients[coefIndex])) {
        if (repetition==0) {
          coefficientsInOrder = c(coefficientsInOrder, coefficients[coefIndex])
          repetition = repetition + 1
        }
      }
    }
  }

  # update the coefficients variable with its arranged version
  # also convert its class from a character vector into a list
  coefficients = as.list(coefficientsInOrder)
  # print(coefficients)
  # remove the variables in each element of the coefficients list
  for (i in 1:length(coefficients) ) {
    # print(substring(coefficients[[i]], nchar(coefficients[[i]])-1, nchar(coefficients[[i]])-1 ))  # for checking
    # print(coefficients[i]) # for checking
    if (substring(coefficients[[i]], nchar(coefficients[[i]])-1, nchar(coefficients[[i]])-1 ) == "x") {
      coefficients[[i]] = as.numeric( substring( coefficients[[i]], 1, nchar(coefficients[[i]])-3 ) )
    } else if ( substring(coefficients[[i]], nchar(coefficients[[i]])-2, nchar(coefficients[[i]])-2 ) == "x" ) {
      coefficients[[i]] = as.numeric(substring(coefficients[[i]], 1, nchar(coefficients[[i]])-4))
    }
    
    # print(coefficients[i])
  }
  # print("*************************************************")

  # earlier we removed b in the coefficients variable
  # now we'll just append b back to the newly arranged coefficients variable
  coefficients = c(coefficients, as.numeric(b))

  # return the coefficients

  return (coefficients)
}

# ================================================================================================================
# a function that will return a list of valid variables defined in a given math equation
# takes an extracted component (variables) as a parameter
formatVariables = function (extractedVariables) {
  variables = extractedVariables

  # remove the "function" word
  variables = gsub("function", "", variables)

  # remove all the spaces in the variable named variables
  variables = gsub(" ", "", variables)

  # remove parentheses
  variables = gsub("(", "", variables, fixed=TRUE)
  variables = gsub(")", "", variables, fixed=TRUE)

  # split the string variables with comma(,) as the delimiter
  variables = strsplit(variables, ",")

  # currently, variables is a list of length 1 whose content is a vector
  # containing the extracted variables' elements that were split by a comma
  # however, what we want to achieve is a list of length n (number of actual variables) that of course holds the actual variables
  # to fix this issue, just perform unlisting to the given list 'variables' to convert its state into just a vector
  variables = unlist(variables)

  # now, perform proper conversion technique into our vector 'variables' to achieve our main goal
  variables = as.list(variables)

  # return the resulting (formatted) list 'variables'
  return (variables)
}


# ================================================================================================================
# for checking
# systemDeparsed = lapply(system, deparse)
#
# sampleVar = c("x1","x2","x3")
# class(sampleVar)
#
# sampleVar = as.list(sampleVar)
# length(sampleVar)
#
# sampleVarStr = "(x1, x2, x3)"
# sampleVarFormatted = formatVariables(sampleVarStr)
# print(sampleVarFormatted)
# length(sampleVarFormatted)
#
# sampleCoef = c("3 * x1 + -0.2 * x3 + -0.1 * x2 + -7.85")
# sampleCoefFormatted = formatCoefficients(sampleCoef, sampleVarFormatted)
# class(sampleCoefFormatted)
# print(sampleCoefFormatted)
# nchar("HELLO")
#
# # get the value of b(constant) from the formatted coefficients
# functB = sampleCoefFormatted[[length(sampleCoefFormatted)]]
# print(functB)
# # remove b from the list of formatted coefficients
# sampleCoefFormatted = sampleCoefFormatted[-length(sampleCoefFormatted)]
# print(sampleCoefFormatted)
# ================================================================================================================
# actual testing
E1 <- function (x1, x2, x3) 0.3 * x1 + -0.2 * x2 + 10 * x3 + -71.4;
E2 <- function (x1, x2, x3) 3 * x1 + -0.2 * x3 + -0.1 * x2 + -7.85;
E3 <- function (x1, x2, x3) 0.1 * x1 + 7 * x2 + -0.3 * x3 + 19.3;
system = list(E1, E2, E3)
# print(system)

result1 = AugCoeffMatrix(system)

# for checking
# excess equation
E1 <- function (x1, x2, x3) 0.3 * x1 + -0.2 * x2 + 10 * x3 + -71.4;
E2 <- function (x1, x2, x3) 3 * x1 + -0.2 * x3 + -0.1 * x2 + -7.85;
E3 <- function (x1, x2, x3) 0.1 * x1 + 7 * x2 + -0.3 * x3 + 19.3;
E4 <- function (x1, x2, x3) 0.1 * x1 + 7 * x2 + -0.3 * x3 + 19.3;
system2 = list(E1, E2, E3, E4)
# print(system2)

result2 = AugCoeffMatrix(system2)

# for checking
# unequal number of unknown variables
E1 <- function (x1, x2, x3) 0.3 * x1 + -0.2 * x2 + 10 * x3 + -71.4;
E2 <- function (x1, x2, x3) 3 * x1 + -0.2 * x3 + -0.1 * x2 + -7.85;
E3 <- function (x1, x2) 0.1 * x1 + 7 * x2 + -0.3 * x3 + 19.3;
system3 = list(E1, E2, E3)
# print(system3)

result3 = AugCoeffMatrix(system3)

# for checking
# 5 equations
E1 <- function (x1, x2, x3, x4, x5) 0.3 * x1 + -0.2 * x2 + 10 * x3 + 8 * x4 + 8 * x5 + -71.4;
E2 <- function (x1, x2, x3, x4, x5) 3 * x1 + -0.2 * x3 + -0.1 * x2 + 8 * x4 + 8 * x5 + -7.85;
E3 <- function (x1, x2, x3, x4, x5) 0.1 * x1 + 7 * x2 + -0.3 * x3 + 8 * x4 + 8 * x5 + 19.3;
E4 <- function (x1, x2, x3, x4, x5) 0.3 * x1 + -0.2 * x2 + 10 * x3 + 8 * x4 + 8 * x5 + -71.4;
E5 <- function (x1, x2, x3, x4, x5) 3 * x1 + -0.2 * x3 + -0.1 * x2 + 8 * x4 + 8 * x5 + -7.85;

system4 = list(E1, E2, E3, E4, E5)
# print(system4)

result4 = AugCoeffMatrix(system4)

# for checking 7 equations
E1 <- function (x1, x2, x3, x4, x5, x6, x7) 0.3 * x1 + -0.2 * x2 + 10 * x3 + 8 * x4 + 8 * x5 + 8 * x6 + 8 * x7 + -71.4;
E2 <- function (x1, x2, x3, x4, x5, x6, x7) 3 * x1 + -0.2 * x3 + -0.1 * x2 + 8 * x4 + 8 * x5 + 8 * x6 + 8 * x7 + -7.85;
E3 <- function (x1, x2, x3, x4, x5, x6, x7) 0.3 * x1 + -0.2 * x2 + 10 * x3 + 8 * x4 + 8 * x5 + 8 * x6 + 8 * x7 + -71.4;
E4 <- function (x1, x2, x3, x4, x5, x6, x7) 3 * x1 + -0.2 * x3 + -0.1 * x2 + 8 * x4 + 8 * x5 + 8 * x6 + 8 * x7 + -7.85;
E5 <- function (x1, x2, x3, x4, x5, x6, x7) 0.3 * x1 + -0.2 * x2 + 10 * x3 + 8 * x4 + 8 * x5 + 8 * x6 + 8 * x7 + -71.4;
E6 <- function (x1, x2, x3, x4, x5, x6, x7) 3 * x1 + -0.2 * x3 + -0.1 * x2 + 8 * x4 + 8 * x5 + 8 * x6 + 8 * x7 + -7.85;
E7 <- function (x1, x2, x3, x4, x5, x6, x7) 0.3 * x1 + -0.2 * x2 + 10 * x3 + 8 * x4 + 8 * x5 + 8 * x6 + 8 * x7 + -71.4;


system5 = list(E1, E2, E3, E4, E5, E6, E7)
# print(system4)

result5 = AugCoeffMatrix(system5)


# for checking 8 equations
E1 <- function (x1, x2, x3, x4, x5, x6, x7, x8) 0.3 * x1 + -0.2 * x2 + 10 * x3 + 8 * x4 + 8 * x5 + 8 * x6 + 8 * x7 + 8 * x8 + -71.4;
E2 <- function (x1, x2, x3, x4, x5, x6, x7, x8) 3 * x1 + -0.2 * x3 + -0.1 * x2 + 8 * x4 + 8 * x5 + 8 * x6 + 8 * x7 + 8 * x8 + -7.85;
E3 <- function (x1, x2, x3, x4, x5, x6, x7, x8) 0.3 * x1 + -0.2 * x2 + 10 * x3 + 8 * x4 + 8 * x5 + 8 * x6 + 8 * x7 + 8 * x8 + -71.4;
E4 <- function (x1, x2, x3, x4, x5, x6, x7, x8) 3 * x1 + -0.2 * x3 + -0.1 * x2 + 8 * x4 + 8 * x5 + 8 * x6 + 8 * x7 + 8 * x8 + -7.85;
E5 <- function (x1, x2, x3, x4, x5, x6, x7, x8) 0.3 * x1 + -0.2 * x2 + 10 * x3 + 8 * x4 + 8 * x5 + 8 * x6 + 8 * x7 + 8 * x8 + -71.4;
E6 <- function (x1, x2, x3, x4, x5, x6, x7, x8) 3 * x1 + -0.2 * x3 + -0.1 * x2 + 8 * x4 + 8 * x5 + 8 * x6 + 8 * x7 + 8 * x8 + -7.85;
E7 <- function (x1, x2, x3, x4, x5, x6, x7, x8) 0.3 * x1 + -0.2 * x2 + 10 * x3 + 8 * x4 + 8 * x5 + 8 * x6 + 8 * x7 + 8 * x8 + -71.4;
E8 <- function (x1, x2, x3, x4, x5, x6, x7, x8) 3 * x1 + -0.2 * x3 + -0.1 * x2 + 8 * x4 + 8 * x5 + 8 * x6 + 8 * x7 + 8 * x8 + -7.85;


system6 = list(E1, E2, E3, E4, E5, E6, E7, E8)
# print(system4)

result6 = AugCoeffMatrix(system6)

# for checking 10 equations
E1 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) 0.3 * x1 + -0.2 * x2 + 10 * x3 + 8 * x4 + 8 * x5 + 8 * x6 + 8 * x7 + 8 * x8 + 8 * x9 + 8 * x10 + -71.4;
E2 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) 3 * x1 + -0.2 * x3 + -0.1 * x2 + 8 * x4 + 8 * x5 + 8 * x6 + 8 * x7 + 8 * x8 + 8 * x9 + 8 * x10 + -7.85;
E3 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) 0.3 * x1 + -0.2 * x2 + 10 * x3 + 8 * x4 + 8 * x5 + 8 * x6 + 8 * x7 + 8 * x8 + 8 * x9 + 8 * x10 + -71.4;
E4 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) 3 * x1 + -0.2 * x3 + -0.1 * x2 + 8 * x4 + 8 * x5 + 8 * x6 + 8 * x7 + 8 * x8 + 8 * x9 + 8 * x10 + -7.85;
E5 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) 0.3 * x1 + -0.2 * x2 + 10 * x3 + 8 * x4 + 8 * x5 + 8 * x6 + 8 * x7 + 8 * x8 + 8 * x9 + 8 * x10 + -71.4;
E6 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) 3 * x1 + -0.2 * x3 + -0.1 * x2 + 8 * x4 + 8 * x5 + 8 * x6 + 8 * x7 + 8 * x8 + 8 * x9 + 8 * x10 + -7.85;
E7 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) 0.3 * x1 + -0.2 * x2 + 10 * x3 + 8 * x4 + 8 * x5 + 8 * x6 + 8 * x7 + 8 * x8 + 8 * x9 + 8 * x10 + -71.4;
E8 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) 3 * x1 + -0.2 * x3 + -0.1 * x2 + 8 * x4 + 8 * x5 + 8 * x6 + 8 * x7 + 8 * x8 + 8 * x9 + 8 * x10 + -7.85;
E9 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) 0.3 * x1 + -0.2 * x2 + 10 * x3 + 8 * x4 + 8 * x5 + 8 * x6 + 8 * x7 + 8 * x8 + 8 * x9 + 8 * x10 + -71.4;
E10 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) 3 * x1 + -0.2 * x3 + -0.1 * x2 + 8 * x4 + 8 * x5 + 8 * x6 + 8 * x7 + 8 * x8 + 8 * x9 + 8 * x10 + -7.85;


system7 = list(E1, E2, E3, E4, E5, E6, E7, E8, E9, E10)
# print(system4)

result7 = AugCoeffMatrix(system7)