makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        # "x <<- y" substitutes the vector x with y (the input) in the main function (makeVector)
        # stores new value as x
        # <<- makes change throughout the entire function, not just this function
        # m <<- NULL returns the mean to NULL, so that a new mean can be calculated
        get <- function() x
        # returns function x
        setinverse <- function(inverse) i <<- inverse
        # sets the inverse, stores in i
        getinverse <- function() i
        # returns inverse (i)
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        # storing the functions
}

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        # if i already exists in makeCacheMatrix, then i returned, function stopped, otherwise continue
        data <- x$get()
        i <- solve(data, ...)
        # i calcs the inverse
        x$setinverse(i)
        i
        # stores the inverse
}
