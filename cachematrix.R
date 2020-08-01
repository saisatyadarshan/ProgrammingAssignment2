## The makeCacheMatrix is a function to create a special matrix object
## that can cache its inverse.

## The cacheSolve function computes the inverse of the special matrix 
## returned by makeCacheMatrix.


makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        set<-function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## It first checks if inverse has already been calculated; If so it gets the 
## inverse from the cache and skips the computation.
## Otherwise, it calculates inverse of the data and sets the value of the 
## inverse in the cache and skips the computation.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
       
}
