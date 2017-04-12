makeCacheMatrix <- function(x = matrix()) {
## return: a list containing functions to
##      1. set the matrix
##      2. get the matrix
##      3. set the inverse
##      4. get the inverse
## this list is used as the input to cacheSolve()

        inv <- NULL
        ## initialize empty matrix
    
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
    
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}



cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
 
        inv <- x$getinverse()
        # if the inverse has already been calculated
        
        if(!is.null(inv)) {
                #get from cache, skip calculation
                message("getting cached data")
                return(inv)
        }
        # otherwise, calculate the inverse
        mat <- x$get()
        inv <- solve(mat, ...)
    
        # set the value of the inverse in the cache via the setinv func
        x$setinverse(inv)
        inv
}

