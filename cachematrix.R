## This aims to create the inverse of a matrix, but if the inverse
## was already solved, it stores it in cache and retrieves it if needed later

## This is the function which helps in storing the results of the inverse in cache

makeCacheMatrix <- function(x = matrix()) {
        m <<- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        
        list(set= set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This gets back the inverse of the matrix, first checking if its there in storage, and if not
## it solves it from scratch

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        if (!is.null(m)) {
                message("Getting cached data...")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data)
        
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
