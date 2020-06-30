## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # store input matrix in x
    set <- function (y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {x} 
    setInverse <- function(inverse) {m <<- inverse} # set the inverse
    getInverse <- function() {m} # get the inverse
    list(set = set, get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getInverse() # gets the value of the inverse of 'x'
    if(is.matrix(m) && !is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get() # 
    m <- solve(data, ...) # calculate the inverse if 'm' was NULL
    x$setInverse(m)       # store calculated inverse value in x 
    m                     # return the inverse of the input matrix
}
