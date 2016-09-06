## Overall Description
## Computing the inverse of a matrix (using caching).


## Caching function with placeholders.
makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve(x) %*% x
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Returns a matrix that is the inverse of 'x'.
cacheSolve <- function(x, ...) {
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


## Procedures for testing the functions

## My super simple matrix test
## mat<-matrix(1:4, 2, 2)
## cacheSolve(makeCacheMatrix(mat))
## Computed matrix inverse by hand to verify functioning

## My 5x5 matrix
## mat<-matrix(rnorm(5^2), 5)
## cacheSolve(makeCacheMatrix(mat))


