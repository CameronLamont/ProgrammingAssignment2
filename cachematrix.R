## Coursera R Programming Assignment 2
#
# Square Matrix Inverse Cache
# - new object to capture and store inverse of a square matrix() using solve()
# - matrix to be enclosed within makeCacheMatrix(x) where x is a matrix()
# - cacheSolve to be used in conjunction as an alternative to solve
#   will pass on ... args however cache will ignore these
#
##

# Wrapper for a matrix to cache the inverse value of a matrix; used by cacheSolve
# function acts as an object to enclose a matrix(), x
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    #get/set for underlying matrix
    set <- function(y) {
        x <<- y
        # if the underlying matrix data changes wipe the cache
        m <<- NULL
    }
    # return the underlying matrix
    get <- function() x
    
    #get/set for Inverse Cache
    setCacheMatrix <- function(CacheMatrix) m <<- CacheMatrix
    getCacheMatrix <- function() m
    # enumurate functions within CacheMatrix
    list(set = set, get = get,
         setCacheMatrix = setCacheMatrix,
         getCacheMatrix = getCacheMatrix)
}


# function to wrap standard solve() function
# will utilise cached inverse value of a cached square matrix x (makeCacheMatrix)
# -- further arguments passed onto solve() along with x.get() if cache is empty
#
# NB: cacheSolve will not account for extra parameters passed 
# to Solve() through ... - cacheSolve presumes these remain constant
#
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    # get cached value
    m <- x$getCacheMatrix()
    # if cached value exists (not null) return it without recomputing
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # if cached value does not exist (is null) 
    # get the underlying data 
    data <- x$get()
    #compute the inverse 
    m <- solve(data, ...)
    #store it in the cache
    x$setCacheMatrix(m)
    #return the makeCacheMatrix
    m
}
