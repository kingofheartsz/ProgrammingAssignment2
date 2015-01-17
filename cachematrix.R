## Put comments here that give an overall description of what your
## functions do

## The first function takes a matrix input and caches it. It can also
## store the inverse of the matrix. The second function attempts to solve
## the matrix.

## Write a short comment describing this function
## It takes a matrix input and caches it. It also has the ability to
## store the inverse of the given matrix. The output is a list of function is returned.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        } 
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## it attempts to solve the inverse of a matrix by first searching
## the getfunction function; otherwise, proceeds to solving the matrix
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
