## Put comments here that give an overall description of what your
## functions do

## Creating a special "matrix"

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setcache <- function(solve) inver <<- solve
    getcache <- function() inver
    list(set = set, get = get,
         setcache = setcache,
         getcache = getcache)
}


## creating inverse of a matrix and cache it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
            inver <- x$getcache()
        if(!is.null(inver)) {
            message("getting cached data")
            return(inver)
        }
        data <- x$get()
        inver <- solve(data, ...)
        x$setcache(inver)
        inver
    
}
