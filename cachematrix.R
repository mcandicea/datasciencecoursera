## This pair of functions help compute the inverse of a matrix,
## and cache the result

## makeCacheMatrix is a function that creates a special "matrix" 
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
             
}


## cacheSolve is a function that computes the inverse of the special
## "matrix" returned by makeCacheMatric above. If the inverse has already
## been calculated (and the matrix has not changed), then the function 
## returns the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
