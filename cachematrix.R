# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to create a special "matrix" object, 
# calculate and cache the inverse of this object.

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) i <<- inverse
    
    getInverse <- function() i
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


# This function computes the inverse of the special "matrix". 
# If the inverse has already been calculated, 
# then it should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    i <- x$getInverse()
    
    if (!is.null(i)){
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    i
    
}
