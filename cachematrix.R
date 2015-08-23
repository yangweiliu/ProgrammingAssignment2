## Put comments here that give an overall description of what your
## functions do

## The following is to create a matrix object that can cache its inverse

makeCacheMatrix <- function(x=matrix()) {
    ## Initialize
    i <- NULL
    ## set matrix
    set <- function(matrix) {
        m <<- matrix
        i <<- NULL
    }
    
    ## get matrix
    get <- function(){
        m
    }
    
    ## set inverse
    set_Inverse <- function(inverse){
        i <<- inverse
    }
    
    ## get inverse
    get_Inverse <- function() {
        i
    }
    
    ## Return method list
    list(set=set, get=get, set_Inverse=set_Inverse, get_Inverse=get_Inverse)
}


## Compute inverse of the matrix returned by makeCacheMatrix. 
## If inverse is already calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$get_Inverse()
    
    ## If already done, then just return the inverse
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## Get matrix from object
    data <- x$get()
    
    ## Calculate the inverse 
    m <- solve(data) %*% data
    
    ## Set inverse
    x$set_Inverse(m)
    
    ## Return matrix
    m
}
