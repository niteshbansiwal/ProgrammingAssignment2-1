#Function to make a get a matrix and store it and compute inverse of it

makeCacheMatrix<- function(x = matrix()) {
    m <- NULL                                          #m is initialised with value NULL
    set <- function(y) {                               # this will assign values to X
        x <<- y
        m <<- NULL
    }
    get <- function() x                                # to retrieve value of variable
    setinverse <- function(solve) m <<- solve          #this will solve inverse of matrix
    getinverse <- function() m                         # this will get inverse of matrix
    list(set = set, get = get,
         setinverse =  setinverse,
         getinverse  =  getinverse)
}

cacheSolve <- function(x, ...) {                       #this function is to get value of inverse from cache memory if already computed
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




