## These functions compute and cache the inverse of a matrix.

## makeCacheMatrix is a function which creates a list of length four,
## where each element of the list is a function.
## The first function, set(), overwrites the input matrix x
## and nullifies any existing inverse stored in i.
## The second function, get(), simply retrieves the input matrix x.
## The third function, setinv(), caches the matrix inverse in i. 
## The fourth function, getinv(), retrieves the cached matrix inverse i.

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y) {
      x <<- y
      i <<- NULL
   }
   get <- function() x
   setinv <- function(inv) i <<- inv
   getinv <- function() i
   list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}

## cacheSolve takes as input a list x (as returned from makeCacheMatrix)
## and returns the inverse of the matrix stored in x.
## The function first looks for the cached inverse of the matrix.
## If the inverse has not been cached (is null), the function calculates 
## the inverse of the matrix stored in x$get() and caches this value 
## in x$setinv().

cacheSolve <- function(x, ...) {
   i <- x$getinv()
   if(!is.null(i)) {
      message("getting cached data")
      return(i)
   }
   data <- x$get()
   i <- solve(data, ...)
   x$setinv(i)
   i 
}
