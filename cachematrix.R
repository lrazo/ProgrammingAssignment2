## The first function, `makeCacheMatrix` creates a special "Matrix", 
## which is a list containing the following functions
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the matrix inverse
## 4.  get the value of the matrix inverse
##
## The second function, 'cacheSolve' calculates the inverse of the special "Matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the matrix inverse from the
## cache and skips the computation. Otherwise, it calculates the matrix inverse 
## and sets the value of the matrix inverse in the cache via the `setsolve`
## function.
##



## This function creates a special "matrix" object that can cache its inverse. 
## It is a list that defines four methods

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x<<-y
                i<<-NULL
        }
        get <- function() x
        setsolve <- function(inv) i<<-inv
        getsolve <- function() i
        list(set = set, get = get, 
             setsolve = setsolve, 
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
