# Gabinete, Keith Ginoel S.
# CMSC 150 - B3L
# Given a vector of n intgers, get the frequencty
# of the occurences of each number. The function must return the tabulated
# format of the unique numbers and its frequence in a matrix format.

# sample input
numInput = c(1,9,7,6,1,2,9,7,3,1)

# create a function that
getFrequency = function(numInput) {
  copy = numInput
  # will store the numbers in the numInput
  vector1 = c(numInput[1])
  
  # loop through the input vector and check if the selected number is already in vector 1
  for (num1 in numInput) {
    if (num1%in%vector1) {
    }
    else {
      vector1 = c(vector1, num1)
    }
  }
  
  # will contain the frequency of each value stored in vector1
  vector2 = c()
  
  # loop through the elements of the input vector
  for (number1 in vector1) {
    # will store the number of appearances of the number in the input vector
    frequency = 0
    
    # loop through the elements of the input vector again
    # needed to compare the readed number from the vector to each number in the input vector
    for (number2 in numInput) {
      # check if num1 is equal to num2
      if (number1 == number2) {
        # update the frequency counter
        frequency = frequency + 1
      }
    }
    
    # concatenate the current value of the frequency variable to vector2
    vector2 = c(vector2, frequency)
  }
  
  # create a matrix to return the result in a table
  result = matrix(
    c(vector1,vector2),
    nrow = length(vector1),
    ncol = 2,
    byrow = FALSE,
    dimnames = list(1:length(vector1), c("Unique Value", "Frequency"))
  )
}

# call the getFrequencty function and print the result
result = getFrequency(numInput)
print(result)
