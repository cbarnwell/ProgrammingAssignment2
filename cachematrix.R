## This function creates a special "matrix" which is really a list
## containing functions to:

## 1) set the value of the matrx
## 2) Get the value of the matrx
## 3) Set the value of the inverse matrix
## 4) Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }

    get <- function() x
    setInverse <- function(mx) im <<- mx
    getInverse <- function() im
        
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## This function calculates the inverse matrix of the special "matrix" 
## created in the makeCacheMatrix function.  It first checks to see 
## if the inverse matrix has been calculated.  If so, it gets the inverse
## matrix from cache and skips the computation.  Otherwise it calculates
## the inverse matrix and sets the value in the cache via the 
## setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    i <- x$getInverse()
    if (!is.null(i)){
                        
        message("getting cached inverse matrix")
        return(i)
    }
    m <- x$get()
    i <- solve(m)
    x$setInverse(i)
    i
    
}
