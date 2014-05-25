
#The function creates a special matrix objects that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getdata <- function() x
		#The <<- operator assigns the val of an obj from a diff environment		
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, getdata = getdata,
             setinverse = setinverse,
             getinverse = getinverse)
}

#This function computes the inverse of the special matrix
#If the inverse has been calculated cacheSolve retrieves the inverse from cache

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        tmp_data <- x$getdata()
        inv <- solve(tmp_data, ...)
        x$setinverse(inv)
        inv
}
