## Put comments here that give an overall description of what your
## functions do

    # The makeCacheMatrix will create a special matrix that can
    # cache its inverse

## Write a short comment describing this function

    # 1. Set the value of the matrix --> set
    # 2. Get the value of the matrix --> get
    # 3. Set the value of the inverse --> setinverse
    # 4. Get the value of the mean --> getinverse


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
    # This function will compute the inverse of the matrix
    # by first checking to see if the inverse has already
    # been calculated. If it has already been calculated, it
    # will skip the computation and obtain the inverse from 
    # the cache. Otherwise it will compute the inverse and
    # set the value using setinverse()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
