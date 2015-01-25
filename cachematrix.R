## Pair of functions to compute and cache the inverse of a matrix.


## makeCacheMatrix
#This function creates a special "matrix" object that can cache its inverse.
## Returns a list of functions to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## Stores cached inverse
        inv  <- NULL
        
        ## Set value of Matrix and reset cached inverse
        set  <- function(y){
                x <<- y
                inv <<- NULL 
        }
        
        ## Get value of Matrix
        get  <- function() x
        
        ## Set value of inverse
        setinverse  <- function(inverse) inv  <<- inverse
        
        ##Get value of inverse
        getinverse  <- function() inv
        
        ##Return
        list(set= set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Load with cached inverse
        inv  <- x$getinverse()
        
        ## return value of cached inverse if it exists
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        ## compute inverse value, cache and return
        data  <- x$get()
        inv  <- solve(data, ...)
        x$setinverse(inv)
        inv
}
