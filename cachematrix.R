makeCacheMatrix <- function(x = matrix()) {
        k <- NULL
  set <- function(y)            #Setting the value of the input matrix
  {
          x <<- y
          k <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) k <<- inverse         # 'k' is assigned value from a different environment
  getinverse <- function() k                            # Gives inverse of 'k' matrix   
  #We create a list of outputs we want
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


cacheSolve <- function(x, ...) 
{
        k <- x$getinverse()     #Gives the inverse of x
  if (!is.null(k)) 
  {
          message("getting cached data")
          return(k)
  }
  data <- x$get()
  k <- solve(data, ...)
  x$setinverse(k)
  k
}
