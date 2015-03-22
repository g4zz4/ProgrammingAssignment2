## The cacheMatrix functions provide means of caching the inversion  
## of a matrix. Two functions are provided; makeCacheMatrix and cacheSolve.

## makeCacheMatrix is a function to create a special matrix that is capable
## of caching its own inversion and thereby speeding up any process that
## requires the inverse of a matrix to be computated a number of times.
makeCacheMatrix <- function(x = matrix()) {

    ## Initialise the cached inverse (inv) to NULL
    inv <- NULL
    
    ## set function to assign a new matrix to the caching Matrix
    ## The inverse is NULL'd to force a recalculation
    set <- function(a) {      
        x <<- a
        inv <<- NULL
    }
    
    ## get function to return the matrix within the caching Matrix
    get <- function() x
    
    ## setInverse function to store the inverse of the matrix
    setInverse <- function(v) inv <<- v
    
    ## getInverse to retrieve the inverse of the matrix
    getInverse <- function() inv
    
    ## No real idea what this is for!?
    list(set = set, 
         get = get, 
         setinverse = setInverse, 
         getinverse = getInverse )
}



## cacheSolve is a function for solving the inversion of a caching matrix
## If the inversion has been calculated previously then the cached result
## is used otherwise it is calculated using the solve function.
cacheSolve <- function(x, ...) {

    ## Retrieve the inverse from the caching matrix
    inv <- x$getinverse()
    
    ## If the inverse is not NULL then it has already
    ## been computed and the cached copy can be used
    if(!is.null(inv)) {
        message("getting cached data")
        return (inv)
    }
    
    ## If the inverse is NULL, then the matrix is retrieved
    ## and the solve function called to compute the inverse
    data <- x$get()
    inv <- solve(data, ...)
    
    ## The computed inverse is stored back into the caching matrix
    x$setinverse(inv)
    
    ## The result is returned
    inv
}
