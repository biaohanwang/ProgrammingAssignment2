## Create a special object which store the inverse matrix of 'x' in cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return it if the inverse matrix of 'x' exists in cache or make a caculation of solve(x).

cacheSolve <- function(x) {
  m <- x$getinverse()
  
  #Return the inverse matrix if exists.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #Caculate the inverse matrix.
  data <- x$get()
  message("Caculating the inverse matrix")
  m <- solve(data)
  x$setinverse(m)
  m
}
