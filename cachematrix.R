## Put comments here that give an overall description of what your
## functions do


######################## Question one ########################
## This function creates a special "matrix" object that can cache its inverse.

# take the input matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #get the value of the matrix
        get <- function() x
        #set the value of the inverse of the matrix
        setInverse <- function(inverse) inv <<- inverse
        #get the value of the inverse of the matrix
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


######################## Question two ########################
## cashSolve function computes the inverse of the matrix created by 
## makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        # get the value of inverse of the matrix from makeCasheMatrix function 
        inv <- x$getInverse()
        # if the inverse matrix is not empty return the inverse matrix and a message
        if (!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        # if the inverse matrix in empty, get the matrix from previouse function
        mat <- x$get()
        # calculate the inverse of the matrix
        inv <- solve(mat, ...)
        # set the inverse matrix
        x$setInverse(inv)
        # return the matrix inverse! 
        inv
}

