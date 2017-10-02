## makeCacheMatrix - utility function for storing matrix and their inverse
## cacheSolve - returns the inverse of a matrix

## makeCacheMatrix - can be use for saving storing a matrix and caching a copy of it's inverse
makeCacheMatrix <- function(x = matrix()) {

	    m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, 
			 get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}

## cacheSolve - returns the inverse of a matrix
cacheSolve <- function(x, ...) {
        
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}
