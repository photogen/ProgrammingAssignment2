# Caching the Inverse of a Matrix

# makeCacheMatrix creates a list containing a function to get & set the values
# of a special "matrix" object, and to set & get the values of its inverse 
makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y){
            # assigning values to the "matrix" (x) and its inverse (i) objects in an
            # environment different from the current global environment; the cached
            # values of the inverse can be retrieved as long as the matrix content
            # remained unchanged.
            x <<- y
            i <<- NULL
      }
      # the two function below create a special object that stores the numeric content
      # of the "matrix" and caches its inverse.
      get <- function() x
      setinv <- function(inverse) i <<- inverse
      getinv <- function() i
      list(set= set, get = get,
           setinv = setinv,
           getinv = getinv)
}

# cacheSolve computes the inverse of the special "matrix" returned by the above
# makeCacheMatrix function. An inverse that has been already calculated and stored,
# is retrieved from the cache by the cacheSolve function, provided that the matrix
# content remained unchanged.
cacheSolve <- function(x, ...) {
      i <- x$getinv()
      # if the inverse has already been calculated, it is retrieved from the cache
      if (!is.null(i)){
            message("getting cached data")
            return(i)
      }
      # otherwise, the inverse is calculated, and its values are set in the cache
      # using the setinv function
      data <- x$get()
      i <- solve(data, ...)
      x$setinv(i)
      i
}

