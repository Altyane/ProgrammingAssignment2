I think that my makeCacheMatrix function create an empty variable 'mat' and a list of four functions: set, that allow to replace
the entry matrix 'x' with another matrix 'y'; get, that allow to return the entry matrix 'x'; setinverse, that allow
to compute and cache the inverse matrix; and getinverse, that allow to return the inverse matrix.


makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y) {
                x <<- y
                mat <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) mat <<- solve
        getinverse <- function() mat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


I think that my cacheSolve function verify if the inverse matrix 'x' has already been calculated: if yes, retrieve
the inverse matrix; if not, compute it, retrieve it and set it in the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        inversemat <- x$getinverse()
        if(!is.null(inversemat)) {
                message("getting cached inversemat")
                return(inversemat)
        }
        mat <- x$get()
        inversemat <- solve(mat, ...)
        x$setinverse(inversemat)
        inversemat
}
