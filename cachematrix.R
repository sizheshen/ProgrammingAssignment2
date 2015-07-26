## These two functions cache the inverse of a matrix to avoid computing the inverse repeatedly.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the matrix
## get the matrix
## set the matrix inverse
## get matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    ##Assuming that the matrix supplied is always invertible
    ##Set initial value of inverse as null
    inv <- NULL

    ##Assign a matrix to x 
    set <- function(y) {
       x <<- y
       inv <<- NULL
     }
        
    ##Display matrix
    get <- function() x
    
    ##Assign the inverse of the matrix to inv
    setinv <- function(inverse) inv <<- inverse

    ##Display the inverse of the matrix
    getinv <- function() inv
    
    ##Return a list
    list(set = set, get = get,setinv = setinv,getinv = getinv)
}


## The following function, cacheSolve, calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    
    ##Retrieve cached matrix inverse
    inv <- x$getinv()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ##If cached matrix inverse does not exist, then calculate the inverse
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)

    ## Return a matrix that is the inverse of 'x'
    inv
}
