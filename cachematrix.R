
##Underneath a couple of capacities that are utilized to make an extraordinary article 
##that stores a network and reserves its converse.
## This capacity makes an uncommon "framework" question that can reserve its reverse.

makeCacheMatrix <- function(x = matrix()) {
        invr <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invr <<- inverse
        getInverse <- function() invr
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This capacity figures the opposite of the extraordinary framework made by makeCacheMatrix above 
## If the inverse has already been calculated (and the framework has not changed then it ought to recover the reverse from the store).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invr <- x$getInverse()
        if (!is.null(invr)) {
                message("getting cached data")
                return(invr)
        }
        mat <- x$get()
        invr <- solve(mat, ...)
        x$setInverse(invr)
        invr
}
