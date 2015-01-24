
## Creates an empty vector to hold list of functions
## Calculates matrix inverse, m, and saves to parent environment.
## Sets a list of functions, with names, to enable access to value.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}
## Tests to see if m contains a value, if so shows message
## "getting cached data" and returns cached value.
## If not then calculates and returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m

}