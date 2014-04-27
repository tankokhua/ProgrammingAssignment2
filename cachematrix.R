## Matrix inversion is usually a costly computation and their may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly. 
## The coding below is to write a pair of functions that cache the inverse of a matrix.

## Usage Example:
## > test_matrix <- matrix(rnorm(1024*1024), 1024, 1024)
## > m <- makeCacheMatrix(test_matrix)
## > cacheSolve(m)                     # first time takes longer to solve
## > system.time(cacheSolve(m))        # compare "cacheSolve()"
## > system.time(solve(test_matrix))   # against "solve()"

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
        x <<- y
        m <<- NULL
   }
   get <- function() x
   setinverse <- function(soln) m <<- soln   # save matrix solution
   getinverse <- function() m                # retrieve previously solved matrix solution
   list(set = set, get = get, 
        setinverse=setinverse,
        getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
           message("getting cached data")
           return(m)
        }
        # Solve the matrix if not found
        data <- x$get()          # get matrix to solve
        m <- solve(data, ...)    # solve matrix
        x$setinverse(m)          # save result
        m
}
