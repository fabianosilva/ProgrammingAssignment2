## Functions bellow are designed to calculate the inverse of a given matrix
## In case the matrix has already been calculated a cached value will be sent.

## This funcion creates the "bnase" matrix for which a inverse can be 
## calculated

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                   #Setting the initial value
  set <- function(y) {                        # In case a new value is 
    x <<- y                                   # passed, cache is cleared
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(matrix) m <<- matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This funcion calculates the inverse matrix for the object created
## by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()                         # try to retrive cache
  if(!is.null(m)) {                           #In case cache is valid
    return(m)                                 #return value retrieved
  }
  data <- x$get()                             #In case cache is null, get data
  m <- solve(data)                            #calculates the inverse
  x$setinverse(m)                             #updates object/cache
  m                                           #returns inverse
}
