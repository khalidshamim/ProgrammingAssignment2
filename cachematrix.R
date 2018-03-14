## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
    
    -#set the value of the Matrix
    setMatrix <- function(y) {
      x <<- y
      invMatrix <<- NULL
      }
    
    getMatrix <- function() x                              #get the value of the Matrix
    setInverse <- function(inverse) invMatrix <<- inverse  #set the value of the invertible matrix
    getInverse <- function() invMatrix                     #get the value of the invertible matrix
    list(setMatrix = setMatrix, getMatrix = getMatrix,
              setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
        #get the value of the invertible matrix from the makeCacheMatrix function
           
    inv_Matrix <- x$getInverse()
        if(!is.null(inv_Matrix)) {                       #if inverse matrix is not NULL
              message("Getting Cached Invertible Matrix")   #Type message: Getting Cached Invertible Matrix 
              return(inv_Matrix)                             #return the invertible matrix
            }
            
    -#if value of the invertible matrix is NULL then  
            MatrixData <- x$getMatrix()                     #get the original Matrix Data 
            inv_Matrix <- solve(MatrixData, ...)             #use solve function to inverse the matrix
            x$setInverse(inv_Matrix)                         #set the invertible matrix 
            return(inv_Matrix)                               #return the invertible matrix
}
