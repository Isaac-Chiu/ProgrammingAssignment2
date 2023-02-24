## Function of caching and inverse a matrix and another function to solve the cash matrix
## And the example of these functions

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
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
# Example of the functions

# Generating the example matrix

Ex <- matrix(c(10, -5, 0, -5, 10, -5, 0, -5, 10), nrow = 3, ncol = 3, byrow = TRUE)
inv_Ex <- solve(Ex)

# cash the matrix 

cash_Ex <-makeCacheMatrix(Ex)

# solve the matrix 
solv_Ex <- cacheSolve(cash_ex)

# Test

identical(inv_Ex, solv_Ex)
