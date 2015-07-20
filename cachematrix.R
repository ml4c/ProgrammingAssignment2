## Caching the Inverse of a Matrix
##
## Description:
##
## The functions 'makeCacheMatrix' and 'cacheSolve' can be used to calculate the
## inverse of a matrix, store it in a cache, and retrieve it from the cache, in
## order to avoid unnecessary recalculation of the inverse of a matrix.
##
## Usage/Example:
##
## my_matrix <- matrix(c(1, 0, 2, 1), nrow = 2, ncol = 2)
## my_cacheMatrix <- makeCacheMatrix()
## my_cacheMatrix$set(my_matrix)
## my_inverse <- cacheSolve(my_cacheMatrix)
##
## Verify that the cached value is use, i.e., the message
## 'getting cached data' is displayed:
##
## my_inverse <- cacheSolve(my_cacheMatrix)
##


## makeCacheMatrix
##
## Description:
##
## This function creates a special "matrix" object that can cache its inverse.
##
## Usage/Example:
##
## my_matrix <- matrix(c(1, 0, 2, 1), nrow = 2, ncol = 2)
## my_cacheMatrix <- makeCacheMatrix()
## my_cacheMatrix$set(my_matrix)
##

makeCacheMatrix <- function(x = matrix()) {

        ## Initialize the inverse with NULL
        
        inverse <- NULL
        
        
        ## Function to set the matrix of the cacheMatrix
        ## and initialize the inverse with NULL
        
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        
        ## Function to get the matrix from the cacheMatrix
        
        get <- function() x
        
        
        ## Function to set the matrix inverse of the cacheMatrix
        
        setinverse <- function(solve) inverse <<- solve
        
        
        ## Function to get the matrix inverse from the cacheMatrix
        
        getinverse <- function() inverse
        
        
        ## Return the list of set and get functions, i.e., the
        ## "public interfaces" of makeCacheMatrix
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## cacheSolve
##
## Description:
##
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the matrix inverse has already been calculated, then
## cacheSolve retrieves the inverse from the cache.
##
## Usage/Example:
##
## my_matrix <- matrix(c(1, 0, 2, 1), nrow = 2, ncol = 2)
## my_cacheMatrix <- makeCacheMatrix()
## my_cacheMatrix$set(my_matrix)
## my_inverse <- cacheSolve(my_cacheMatrix)
##

cacheSolve <- function(x, ...) {
        
        ## Get the inverse of 'x' from the cacheMatrix
        
        inverse <- x$getinverse()
        
        
        ## If inverse of 'x' was available in the cacheMatrix, return it
        
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        
        ## If inverse of 'x' was NOT available in the cacheMatrix, get
        ## the matrix from cacheMatrix
        
        data <- x$get()
        
        
        ## Calculate the inverse of the matrix
        
        inverse <- solve(data, ...)
        
        
        ## Store the inverse of the matrix into the cacheMatrix
        
        x$setinverse(inverse)
        
        
        ## Return a matrix that is the inverse of 'x'
        
        inverse
        
}