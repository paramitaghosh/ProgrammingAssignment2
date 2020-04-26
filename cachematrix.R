## Caching Inverse of a matrix:
## Caching is useful when we need to do a costly computation
## like inversing a ntarix repeatedlystores a matrix and caches it's inverse.
## Below are pair of functions that can be used to create a special object that
## stores matrix, gets its inverse and caches it.   

## This function creates a special matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
        
        invr <- NULL
        set <- function(y){
                x <<- y
                invr <<- NULL
        }
        
        get <- function() x
        setInverse <- function(inverse) invr <<- inverse
        getInverse <- function() invr
        list(set = set, get= get, setInverse = setInverse,getInverse=getInverse)
}


## This function computes the inverse of the special matric created by makeCacheMatrix function.
## If the inverse has already been calculated then it retrieves the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invr <- x$getInverse()
        if(!is.null(invr)){
                message("getting cached data")
                return(invr)
        }
        
        data <- x$get()
        invr <- solve(data,...)
        x$setInverse(invr)
        invr
}
