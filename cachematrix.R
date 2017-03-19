# coursera assignment week 3
# script contains two functions makeCacheMatrix and cacheSolve
# Create an object to cache a matrix and calculate the inverse matrix if required


# "makeCacheMatrix" creates object to cache matrix
makeCacheMatrix <- function(x = matrix()) {
        invrM <- NULL
        set <- function(y) {
                x <<- y
                invrM <<- NULL
        }
        get <- function() x
        setinverse <- function(inverseMa) invrM <<- inverseMa
        getinverse <- function() invrM
        # create list object for output containing objects of function environment
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# "cacheSolve" calculates inverse matrix or retrieve inverse matrix from cache
# use object created by previous function as input argument
cacheSolve <- function(x, ...) {
        invrM <- x$getinverse()
        # check if inverse matrix already exist
        if(!is.null(invrM)) {
                message("getting cached data")
                return(invrM)
        }
        mdat <- x$get()
        invrM <- solve(mdata, ...)
        x$setinverse(invrM)
        invrM
}