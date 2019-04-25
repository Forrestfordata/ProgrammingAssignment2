## Put comments here that give an overall description of what your
## functions do
#The pair of functions can cache the inverse of a matrix rather than compute it repeatedly

## Write a short comment describing this function
#This function creates a list that can cache the inverse of a matrix putting in the argument.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
#This function computes the inverse of the matrix in the list returned by makeCacheMatrix above. 
#If the inverse has already been calculated, then the cachesolve will retrieve the inverse from the cache.
cacheSolve <- function(x) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m      
}
