## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## functions to store e return the matrix ant its inverse are defined below
    getmat <- function() x
    setinv <- function(s) m <<- s
    getinv <- function() m
    
    ##  a list containing the functions is returned
    list(getmat = getmat,setinv = setinv,getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated then the cachesolve retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    
    ##  If the inverse has already been calculated then it is returned
    i <- x$getinv()
    if(!is.null(i)) return(i)
        
    ##  If the inverse has been calculated yet then it is calculated, stored and returned
    IM <- x$getmat()
    x$setinv(solve(IM))
    i <- x$getinv()
    i
}