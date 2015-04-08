## Function 'makeCacheMatrix' use an square-invertible matrix as input and return a list of outputs from some functions
makeCacheMatrix <- function(x = matrix()) {
    
    ## Compute the determinant of matrix x to check if the matrix is square:
    chck <- determinant(x)
    ## and invertible (non-singular)
    if (abs(chck$modulus) == abs(-Inf)){
        stop("Matrix is singular")
    }
    
    ## Define a local variable 'matinv' within the current environment
    matinv <- NULL
    ## Set the values of input matrix x
    set <- function(y){
        x <<- y
        matinv <<- NULL
    }
    ## Get the values of input matrix x
    get <- function() x
    ## Set the inverse of input matrix x 
    setinv <- function(inverse) matinv <<- inverse
    ## Get the inverse of input matrix x
    getinv <- function() matinv
    ## return list of outputs from functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Function 'cacheSolve' computes the inverse of an input matrix x created in makeCacheMatrix. It first
## checks to see if the inverse of matrix x has already been calculated. If so, it gets the inverse from
## the cache and skips the computation. Otherwise, it calculates the inverse matrix of the data and sets
## the value of the mean in the cache via the setinv function.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of matrix 'x'
    matinv <- x$getinv()
    ## Check whether matrix x is NULL or not
    if(!is.null(matinv)) {
        message("getting cached data")
        ## Return the inverse of matrix x
        return(matinv)
    }
    ## Assign the values in matrix x
    data <- x$get()
    ## Compute its inverse
    matinv <- solve(data, ...)
    ## Assign the inverse matrix to the 'setinv' element of the list above 
    x$setinv(matinv)
    ## Return the inverse matrix
    matinv
}

###################################################################################################

## Testing both functions above

## Create an invertible square matrix
# x = matrix(c(2, 4, 3, 1, 5, 7, 1, 2, 3), nrow=3, ncol=3) # Invertible
# x = matrix(rnorm((25), 5, 2), nrow=5) # Invertible
## Create a non-invertible square matrix
# x = matrix(c(1,3,2,6), nrow=2) # Non-Invertible

## Execute makeVector function
# matx <- makeCacheMatrix(x)
## Extract numeric vector values
# matx$get()
## Get the inverse of the input matrix
# cacheSolve(matx)
## Retrieve the inverse from the cache
# cacheSolve(matx)
## Now that the inverse of matrix x is stored we can call it from the list
# matx$getinv()
