## Functions that cache the inverse of a matrix:

## This function creates a special "matrix" caching its inverse:

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## from makeCacheMatrix above.If the inverse has already been calculated, 
## then cacheSolve should retrieve the inverse from the cache:
        
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

        
## Testing the above functions:
        
> source("ProgrammingAssignment2/cachematrix.R")
> mymatrix <- makeCacheMatrix(matrix(c(1,3,4,6),2,2))
> mymatrix$get()
     [,1] [,2]
[1,]    1    4
[2,]    3    6
> cacheSolve(mymatrix)
     [,1]       [,2]
[1,] -1.0  0.6666667
[2,]  0.5 -0.1666667
> cacheSolve(mymatrix)
getting cached data
     [,1]       [,2]
[1,] -1.0  0.6666667
[2,]  0.5 -0.1666667
> mymatrix$getinverse()
     [,1]       [,2]
[1,] -1.0  0.6666667
[2,]  0.5 -0.1666667
