## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function takes a matrix and creates a cache of the
## function we defined

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setCache <- function(invrM) m <<- invrM
  getCache <- function () m
  list(set = set,
       get = get,
       setCache = setCache, 
       getCache = getCache)
}


## this computes the solve (inverse) function of a matrix. It will return the cache value
## if a cache value is present, otherwise it will compute the inverse. 

cacheSolve <- function(x, ...) {
  
  ##get the cached value of the matrix from m
  m <- x$getCache() 
  ## if m is not null, get the cache data and return it.
  if(!is.null(m)) {
    message("getting cached data")
    return(m) 
  }
  
  ## this gets the original matrix value from the makeCacheMatrix function
  data <- x$get()
  
  m <- solve(data,...) ## and then computes the inverse of it. 
  x$setInverse(m) ## this then sets the value of the cache to the inverse. 
}
