## The functions cache the inverse of a matrix once it has been computed, 
## so the inverse can be retrieved from the memory after the first time.


## Creates the special object that stocks the information of the matrix
## as well as the inverse of that matrix after this has been computed
makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    ## The set function makes sure that once the matrix is modified,
    ## the inverse has to be computed again
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)

}



## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

        ## Sees if the inverse has already been computed
        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        
        data <- x$get()      
        inv <- solve(data)
        x$setinverse(inv)
        
        inv

}
