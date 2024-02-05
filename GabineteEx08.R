# Gabinete, Keith Ginoel S.
# CMSC 150 - B3L
# Exercise No. 8
#
# Part 1: Programming Exercise
# Required Competencies: False Position Method, Secant Method, functions, loops, labeled lists

# ==============================================================================================
# parameters: 
#     f = mathematical function 
#     a = lower limit of an interval
#     b = upper limit of an interval
#     macheps = machine epsilon (used for computing the approximate error)
#     max = maximum number of iterations
#     verbose = optional Boolean value that decides whether to print the whole algorithm process or not. 
#               Default value is TRUE
FalsePositionMethod = function (f, a, b, macheps, max, verbose=TRUE) { # verbose parameter can be left empty when this function is called
  # four return values
  given_a = a
  given_b = b
  
  # check if the given values for a & b does bound the root of the equation
  if(f(a) * f(b) < 0) { # interval does bound the root
    # do nothing
  } else {
    return (NA)
  }
  
  # initializing some variables for computation
  c = 0
  ea = 100
  num_iterations = 0

  # --------------------------------------------------------------------------------------------
  # find the root/s of the equation using the false position method
  
  # for displaying the results of the algorithm process/computation for every iteration
  result_matrix <- matrix(
    ,
    nrow = 0,
    ncol = 7,
    byrow = TRUE,
    dimnames = list(NULL, c("a", "b", "f(a)", "f(b)", "c", "f(c)", "Error(%)"))
  )
  
  # run until the approximate error of the computed value for c is within the defined tolerable error
  while (ea >= macheps && num_iterations != max) {
    # for displaying the results every iteration (optional)
    new_row = c(a, b, f(a), f(b))
    
    # c_old = storage variable for the previous/backup converged value 'c'
    c_old = c
    
    # compute for a new converged value (root approximate)
    c = (b * f(a) - a * f(b)) / (f(a) - f(b))
    
    # if f(c) is zero, then c is already the exact root of the function
    # if this happens, then there's no point continuing the process in finding the root approximate
    if ( f(c) == 0 ) {
      return (c)
    }
    
    # update the value of either a or b
    # check which of the two exhibits an opposite sign to c and update the value of the other variable that wasn'st compared
    if ( f(c) * f(a) < 0 ) {
      b = c
    } 
    else {
      a = c
    }
    
    # compute for the approximate error
    if (num_iterations != 0) {
      ea =abs( (c - c_old) / c ) * 100
    }
  
    # update the iteration counter
    num_iterations = num_iterations + 1
    
    # for displaying the results every iteration (optional)
    new_row = c(new_row, c, f(c), ea)
    result_matrix = rbind(result_matrix, new_row)

  }
  
  # (optional) - only if verbose is TRUE
  # displaying the results of the computation for every iteration
  if (verbose == TRUE) {
    rownames(result_matrix) = 1:nrow(result_matrix)
    print(result_matrix)
  }
  
  # list that contains the return values
  results = list ( f=f, given_a=given_a, given_b=given_b, c=c, iterations=num_iterations, ea=ea )
  
  # return the results
  return (results)
}


# ==============================================================================================
# parameters: 
#     f = mathematical function 
#     x0 = first root approximate
#     x1 = second root approximate
#     macheps = machine epsilon (used for computing the approximate error)
#     max = maximum number of iterations
#     verbose = optional Boolean value that decides whether to print the whole algorithm process or not. 
#               Default value is TRUE
SecantMethod = function (f, x0, x1, macheps, max, verbose=TRUE) { # verbose parameter can be left empty when this function is called
  # four return values
  given_x0 = x0
  given_x1 = x1
  
  # compute for the f(x) (y-values) of the first and second root approximates
  y0 = f(x0)
  y1 = f(x1)
  
  # initializing some variables for computation
  x = 0
  ea = 100
  num_iterations = 0
  
  # --------------------------------------------------------------------------------------------
  # find the root/s of the equation using the secant method
  
  # for displaying the results of the algorithm process/computation for every iteration
  result_matrix <- matrix(
    ,
    nrow = 0,
    ncol = 7,
    byrow = TRUE,
    dimnames = list(NULL, c("x0", "x1", "f(x0)", "f(x1)", "x", "f(x)", "Error(%)"))
  )
  
  # run until the approximate error of the computed value for c is within the defined tolerable error
  while (ea >= macheps && num_iterations != max) {
    # for displaying the results every iteration (optional)
    new_row = c(x0, x1, f(x0), f(x1))
    
    # x = storage variable for the previous/backup root approximate 'x'
    x_old = x
    
    # compute for a new root approximate
    x = x1 - (x1 - x0) * y1 / (y1 - y0)
    y = f(x)
    
    # if x is zero, then x is already the exact root of the function
    # if this happens, then there's no point continuing the process in finding the root approximate
    if ( f(x) == 0 ) {
      return (x)
    }
    
    # compute for the approximate error
    if (num_iterations != 0) {
      ea = abs( (x - x_old) / x ) * 100
    }
    
    # update the values of the variables for the root approximates
    x0 = x1
    y0 = y1
    x1 = x
    y1 = y
    
    # update the iteration counter
    num_iterations = num_iterations + 1
    
    # for displaying the results every iteration (optional)
    new_row = c(new_row, x, f(x), ea)
    result_matrix = rbind(result_matrix, new_row)
    
  }

  # (optional) - only if verbose is TRUE
  # displaying the results of the computation for every iteration
  if (verbose == TRUE) {
    rownames(result_matrix) = 1:nrow(result_matrix)
    print(result_matrix)
  }
  
  # list that contains the return values
  results = list ( f=f, given_x0=given_x0, given_x1=given_x1, x=x, iterations=num_iterations, ea=ea )
  
  # return the results
  return (results)

}
# ==============================================================================================
# Sample Run
# options(digits=4)
# fxn = function (x) -0.6*(x^2) + 2.4*x + 5.5
# false_position_result = FalsePositionMethod(fxn, 5, 10, 1*(10^(-3)), 1000, TRUE)
# secant_result = SecantMethod(fxn, 5, 10, 1*(10^(-3)), 1000, FALSE)

# ----------------------------------------------------------------------------------------------
# part 2: World Problem
macheps = 1*(10^(-2))
max_no_iterations = 1000
options(digits=4)

# -----------------------------------------------------------------------------------
# problem 1
# Given:
V = 40
R = 4
# V = pi*(h^2) * ( (3*R -h)/3 )
# 0 = pi*(h^2) * ( (3*R -h)/3 ) - V
problem1_function = function (h) pi*(h^2) * ( (3*R -h)/3 ) - V
# Employ initial guesses of 0 and R.
a = 0
b = R
problem1 = FalsePositionMethod(problem1_function, 0, 4, macheps, max_no_iterations)
print(problem1)
# -----------------------------------------------------------------------------------
# problem 2
problem2_function = function (x) -26 + 85*x -91*(x^2) + 44*(x^3) + 8*(x^4) + (x^5)
x0 = 0.5
x1 = 1.0
problem2 = SecantMethod(problem2_function, x0, x1, macheps, max_no_iterations, TRUE)
print(problem2)