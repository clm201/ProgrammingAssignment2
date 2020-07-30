# Clare M. cachematrix.R

### makeCacheMatrix() takes a matrix and returns a special list object 
## containing functions to get and set the values of the matrix and to get 
## and set the values of its inverse.
### cacheSolve() takes an object created by makeCacheMatrix() and returns the 
## inverse of the set matrix. 



## makeCacheMatrix() takes a matrix, but the matrix can be empty. It sets 
## the value of the inverse ('inv' below) to NULL to clear any old contents.
## The functions returned in the list are described inside makeCacheMatrix().
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # setMat() modifies the variables x and inv in the parent environment.
    # It allows a new matrix to be used without having to call the
    # whole makeCacheMatrix() function again.
    setMat <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # getMat() retrieves the value of x from the parent environment.
    getMat <- function() x
    
    # setInv() is a function created to set the value of inv in the parent
    # environment, so that it can be retrieved using getInv()
    setInv <- function(matInv) inv <<- matInv
    getInv <- function() inv
    
    # return the list of functions
    list(setMat = setMat, getMat = getMat, setInv = setInv, getInv = getInv)
}


## cacheSolve() requires a matrix with values to have been set with 
## makeCacheMatrix(), so if makeCacheMatrix() was called previously with only
## the default empty matrix, values must be given to the matrix (which can be
## done using setMat() from makeCacheMatrix()). cacheSolve() retrieves inv
## from the object created by makeCacheMatrix(), using the getInv() function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getInv()   # note that 'inv' here is a different variable than
                            # the 'inv' created in makeCacheMatrix()
    
    ## If the inverse (inv) is not NULL, then the inverse has already been 
    ## calculated and is retrieved
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## If the inverse has not already been calculated
    matrixData <- x$getMat()    # retrieve the matrix stored in x
    inv <- solve(matrixData, ...)   # solve for the inverse
    x$setInv(inv)   # cache the inverse
    
    inv    # return the inverse
}
