# Gabinete, Keith Ginoel S.
# CMSC 150 - B3L
# Exercise No. 2
# September 13, 2023

# define a function that returns TRUE if the matrix mat passed as parameter is a square matrix, FALSE otherwise
SquareMatrix = function (mat) {
  nRow = length(mat[1,])
  nCol = length(mat[,1])
  
  if (nRow == nCol) {
    return (TRUE)
  } else {
    return (FALSE)
  }
}

# define a function that accepts three parameters mat, i, and j, and returns the minor of mat with
# respect to i and j
MatrixMinor = function (mat, i, j) {
  minor_ij = det(mat[-i,-j])
  return (minor_ij)
}

# define a function that accepts three parameters mat, i, and j, and returns the cofactor of mat with
# respect to i and j
MatrixCofactor = function (mat, i, j) {
  cofactor = (-1)^(i+j)*MatrixMinor(mat, i, j)
  return (cofactor)
}

# define a funcion that returns the adjoint of the matrix mat if mat is a square matrix;
# returns NA if mat is not a square matrix
MatrixAdjoint = function (mat) {
  if (SquareMatrix(mat)) {
    
    # get the dimensions of the given square matrix
    nRow = length(mat[1,])
    nCol = length(mat[,1])
    
    # will contain all the cofactors of the given matrix mat
    cofactorsOfMat = matrix (
      0, 
      nrow = nRow, 
      ncol = nCol,
      byrow = TRUE
    )
    
    # get the cofactor of each element in the given matrix
    for (i in 1:nRow) {
      for (j in 1:nCol) {
        cofactorsOfMat[i,j] = MatrixCofactor(mat, i, j)
      }
    }
    
    # get the transpose of the cofactors of the given matrix to get its adjoint
    adjointOfMat = t(cofactorsOfMat)
    
    # return the adjoint of the given matrix
    return (adjointOfMat)
      
      
  } else {
    return (NA)
  }
}

# define a function that returns the inverse of the matrix mat if mat is a square matrix;
# returns NA if mat is not a square matrix
MatrixInverse = function (mat) {
  if (SquareMatrix(mat)) {
    
    # get the determinant of the given matrix
    determinantOfMat = det(mat)
    
    # check if the given matrix has an inverse or not
    # if it does not have an inverse, return NA
    if (determinantOfMat ==0) {
      return (NA);
    } else {
      # get the adjoint of the given matrix
      adjointOfMat = MatrixAdjoint(mat)
      
      # get the inverse of the given matrix
      # use the formula: A^(-1) = (1/det(A))*adjofA
      inverseOfMat = (1/determinantOfMat)*adjointOfMat
      
      # return the inverse of the given matrix
      return(inverseOfMat)  
    }

  } else {
    return (NA)
  }
}

# TESTING
A = matrix (
  c(2,1,4,-1,-3,-2,0,-1,5),
  3,
  3
)
print(A)
adjA = MatrixAdjoint(A)
print(adjA)
print(MatrixAdjoint(matB))
invA = MatrixInverse(A)
print(invA)


B = matrix (
  c(2,1,4,-1,-3,-2,0,-1,5,8,9,0,-7,-6,-3,3),
  4,
  4
)
invB = MatrixInverse(B)
print(invB)

print(MatrixInverse(matB))
matA = matrix(1:4, 2)
matB = matrix(c(2,3,4,6,7,5), 2, 3)
print(matB)
print(matA)
print(SquareMatrix(matA))
print(SquareMatrix(matB))
print(MatrixInverse(matB))

# noninvertible matrix
nonInvertible = matrix (
  c(1,0,0,0),
  2,
  2
)
print(MatrixInverse(nonInvertible))
