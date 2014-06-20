## Generate a special matrix; compute and cache the inverse of a matrix


## Generate a special matrix which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, 
	     get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Compute and cache the inverse of a special matrix generated 
## from the makeCacheMatrix function
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse of matrix")
                return(i)
        }
        data <- x$get()
        i <- solve(data, , ...) # omit the 2nd parameter of "solve" intentionally to get the inverse matrix
        x$setinverse(i)
        i
}

