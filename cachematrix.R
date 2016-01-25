## Put comments here that give an overall description of what your
## functions do

## The following functions will compute the inverse of matrices and 
## cache them so that in the future redundant calculations can be avoided

## Write a short comment describing this function

## makeCacheMatrix function inputs a matrix and creates a list allowing us 
## to access the matrix, set a new matrix, get the inverse of the matrix 
## and set its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}




## CacheSolve checks if the inverse of the matrix is stored in the cache to 
## avoid unnecessary calculation. If so it displays the inverse. 
## If not it computes and stores the inverse. The inverse is displayed 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
    }
