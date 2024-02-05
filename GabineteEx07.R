# Gabinete, Keith Ginoel S.
# CMSC 150 - B3L
# Exercise No. 7
#
# Create an R function named Neville which accepts an integer x and a list as inputs.
# The list will contain the two vectors for the data points x and y. 
# It should use Neville’s Interpolating Polynomial to predict the value of the function at x using the
# given data points.
#
#       The function must return the following variables in a list, with the following
#       labels:
#           1. table: the matrix used in computation;
#           2. y: the predicted value of the function at x.
#
#
# ==============================================================================================
Neville = function (x, data_list) {
    
  # extract the list contents
  data_x = data_list[[1]]
  data_y = data_list[[2]]
  
  # --------------------------------------------------------------------------------------------
  # check if the two data points are equal in length
  # if it isn't, then exit the function and return NA
  if ( length(data_x)!= length(data_y) ) {
    cat("\n=======================================================\n")
    cat("ERROR: The x and y data points must be equal in length.\n")
    cat("=======================================================\n\n")
    cat("")
    return (NA)
  }
  
  # --------------------------------------------------------------------------------------------
  # sort the given data points based on the their closeness to x
  distances = c()
  
  # a. compute for the distances between x and each element in data_x
  for (value in data_x) {
    distances = c(distances, abs(x-value))
  }
  
  # b. sort the given data points
  data_x = data_x[order(distances)]
  data_y = data_y[order(distances)]
  
  # for checking
  # print(distances)
  # print(data_x)
  # print(data_y)
  
  # --------------------------------------------------------------------------------------------
  # construct a matrix of size (n+1)x(n+1) 
  # (where n+1 is equal to the number of items in the data points)
  matrix_size = length(data_x)
  n = matrix_size - 1   # also the degree of the polynomial interpolation we're looking for
  
  # for giving some names to the columns of the matrix
  colNames = c()
  for (i in 0:(matrix_size-1)) {
    pString = "Pi,i"
    if (i>0) {
      pString = paste(pString, i, sep="+")
    }
    colNames = c(colNames, pString)
  }
  
  # print(colNames) # for checking
  
  # constructing the matrix
  neville_matrix = matrix (
    data = 0,
    nrow = matrix_size,
    ncol = matrix_size,
    dimnames = list(NULL,colNames)
  )
  
  # populate the first column of the matrix with f(x) or the values in data_y
  neville_matrix[,1] = data_y
  
  # print(neville_matrix) # for checking
  # --------------------------------------------------------------------------------------------
  # solve for the polynomials of degree n
  for (k in 1:n) {
    # will hold the computed values for polynomials of degree n
    new_p_column = c()
    
    # get the latest filled-in column (or simply the current ith column in the matrix)
    p_vector = neville_matrix[,k]
    
    # apply the neville's algorithm formula to compute for a polynomial of degree n
    for (i in 1:( length(p_vector) - k ) ) {
      # P_(i,k) = [  (x - x_i) * P_(i+1, k-1) + (x_(i+k) - x) * P_(i, k-1)  ]   /  (x_(i+k) - x_i)
      new_p_value = ( (x - data_x[i]) * p_vector[i+1] + (data_x[i+k] - x) * p_vector[i] ) / ( data_x[i+k] - data_x[i] )
      
      # update the storage variable
      new_p_column = c(new_p_column, new_p_value)
    }
    
    # --------------------------------------------------------------------------------------------
    # put the computed values into the matrix
    
    # a.
    # first add some additional 0's to the storage variable until it reaches some required length 
    # length of the storage variable should be equal to the number of rows in the matrix
    while ( length(new_p_column) < (n+1) ) {
      new_p_column = c(new_p_column, 0)
    }
    
    # b.
    # insert the computed values as a new column to the constructed matrix
    neville_matrix[,k+1] = new_p_column
  }
  
  # print(neville_matrix) # for checking
  
  # --------------------------------------------------------------------------------------------
  # construct a labelled list for the return values of the Neville function
  # return values:
  # a. table: 
  #     the matrix used in computation - (final) neville_matrix
  table = neville_matrix
  # b. y: 
  #     the predicted value of the function at x
  #     or f(x)^n --> is simply the first value stored in the last column of the final matrix
  y = neville_matrix[1,matrix_size]
  
  # return list
  results = list (table = table, y = y)
  
  return (results)
}

# ==============================================================================================
# Sample Input

# x = c(8,9,11,12)
# y = c(0.9031, 0.9542, 1.0414, 1.0792)
# 
# sample = Neville(10, list(x,y))
# 
# a = c(3,2,1,4)
# b = c(18,08,7,30,40)
# 
# 
# error = Neville(10, list(a,b))

# Part 2.
# data points for x
end_of_march = 31
end_of_april = 30 + end_of_march
end_of_may = 31 + end_of_april
end_of_june = 30 + end_of_may
end_of_july = 31 + end_of_june
days = c(end_of_march, end_of_april, end_of_may, end_of_june, end_of_july)
# data points for f(x) or y
cases = c(182, 536, 245, 2027, 2304)


# 15 days after the celebration = May 8th + 15 days = May 23rd
x = end_of_april+8+15
y = Neville(x, list(days, cases))

# Print the results
print(days)
print(cases)
print(x)
print(y)


