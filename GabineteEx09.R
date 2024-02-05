# Gabinete, Keith Ginoel S.
# CMSC 150 - B3L
# Exercise No. 9

# ==============================================================================================
# Part 1: Programming Exercise
# Required Competencies: Müller’s Method, functions, loops, labeled lists
# ==============================================================================================
# parameters:
#     f = the mathematical function to root-find
#     x0 = first approximate
#     x1 = second approximate
#     x2 = third approximate
#     macheps = machine epsilon (used for computing the approximate error)
#     max = maximum number of iterations
#     verbose = optional Boolean value that decides whether to print the whole algorithm process or not.
#               Default value is TRUE
MullerMethod = function (f, x0, x1, x2, macheps, max, verbose = TRUE) {
  # for return values
  given_x0 = x0
  given_x1 = x1
  given_x2 = x2

  # initializing some variables for computation
  x3 = 0
  ea = 100
  num_iterations = 0

  # --------------------------------------------------------------------------------------------
  # find the root of the mathematical function using the muller's method

  # --------------------------------------------------------------------------------------------
  # for displaying the results of the algorithm process/computation for every iteration
  result_matrix <- matrix(
    ,
    nrow = 0,
    ncol = 12,
    byrow = TRUE,
    dimnames = list( NULL, c("x0", "x1", "x2", "f(x0)", "f(x1)", "f(x2)", "A", "B", "C", "x3", "f(x3)", "Error(%)") )
    )
  # --------------------------------------------------------------------------------------------

  # run until the approximate error of the computed value for x3 is within the defined tolerable error
  while (ea >= macheps && num_iterations != max) {
    # compute for the f(x)/(y-values) of the first, second and third root approximates
    y0 = f(x0)
    y1 = f(x1)
    y2 = f(x2)

    # compute h0, d0, h1, d1
    h0 = x1 - x0
    d0 = (y1 - y0) / h0
    h1 = x2 - x1
    d1 = (y2 - y1) / h1

    # compute A, B, C
    A = (d1 - d0) / (h1 + h0)
    B = A * h1 + d1
    C = y2
    # --------------------------------------------------------------------------------------------
    # solve for x3
    # x3 = x2 - (2c / b+- sqrt(b^2 - 4ac))

    # solve for the denominator part
    # <https://www.r-tutor.com/r-introduction/basic-data-types/complex>
    # negative = abs(B - sqrt(as.complex(B ^ 2 - 4 * A * C))) # produces an error if the discriminant B^2 - 4AC is negative
    # positive = abs(B + sqrt(as.complex(B ^ 2 - 4 * A * C))) # produces an error if the discriminant B^2 - 4AC is negative
    discriminant = B ^ 2 - 4 * A * C

    # no real roots
    # represent discriminant with imaginary number instead
    # <https://stat.ethz.ch/R-manual/R-devel/library/base/html/complex.html>
    # <https://www.johnmyleswhite.com/notebook/2009/12/18/using-complex-numbers-in-r/>
    # use Re for fixing the error: invalid comparison with complex values (Re extracts the real component in the complex number)
    if (Re(discriminant) < 0) {
      discriminant = as.complex(discriminant)
    }

    negative = B - sqrt(discriminant)
    positive = B + sqrt(discriminant)
    
    # comparison of the magnitude of the computed possible denominator values
    denominator = 0
    if (abs(negative) < abs(positive)) {
      denominator = positive
    } else {
      denominator = negative
    }

    # solve for x3
    x3 = x2 - (2 * C / denominator)
    # --------------------------------------------------------------------------------------------
    # compute for the approximate error
    ea = abs((x3 - x2) / x3) * 100

    # --------------------------------------------------------------------------------------------
    # for displaying the results every iteration (optional)
    new_row = c(x0, x1, x2, y0, y1, y2, A, B, C, x3, f(x3), ea)
    result_matrix = rbind(result_matrix, new_row)
    # --------------------------------------------------------------------------------------------

    # update the values of the variables for the root approximates
    x0 = x1
    x1 = x2
    x2 = x3

    # update the iteration counter
    num_iterations = num_iterations + 1
  }

  # --------------------------------------------------------------------------------------------
  # (optional) - only if verbose is TRUE
  # displaying the results of the computation for every iteration
  if (verbose == TRUE) {
    rownames(result_matrix) = 1:nrow(result_matrix)
    print(result_matrix)
  }
  # --------------------------------------------------------------------------------------------

  # list that contains the return values
  results = list ( f=f, given_x0=given_x0, given_x1=given_x1, given_x2=given_x2, x3=x3, iterations=num_iterations, ea=ea )

  # return the results
  return (results)
}

# ==============================================================================================
# Sample Run from the lab handout
# options(digits = 3)
# fxn = function (x) cos(x)
# x0 = 0
# x1 = 2
# x2 = 4
# macheps = 1 * (10 ^ (-5))
# max_default = 1000
# muller_result = MullerMethod(fxn, x0, x1, x2, macheps, max_default, TRUE)

# ----------------------------------------------------------------------------------------------
# Sample Runs from the exercise handout
options(digits = 4)
# Sample Run # 1
fxn = function (x) x^3 - 13*x + -12
sample_result1 = MullerMethod(fxn, 4.5, 5.5, 5, 1*(10^(-9)), 1000, TRUE)

# Sample Run # 2
fxn = function (x) x^3 - x^2 + 2*x - 2
sample_result2 = MullerMethod(fxn, 0, 3, 5, 1*(10^(-9)), 1000, TRUE)

# ----------------------------------------------------------------------------------------------
# Part 2: Word Problem
# prerequisites
macheps = 1 * (10 ^ (-9))
max = 1000
options(digits = 4)

# given functions
csn = function(s) s^3 + 12.5*s^2 + 50.5*s + 66
nsn = function(s) s^4 + 19*s^3 + 122*s^2 + 296*s + 192

# given initial root estimates
x0 = -5 
x1 = -4.7
x2 = -4.4

# solve for the root of C(s)
csn_root = MullerMethod(csn, x0, x1, x2, macheps, max, TRUE)

# solve for the root of N(s)
nsn_root = MullerMethod(nsn, x0, x1, x2, macheps, max, TRUE)

