##  Next, the requirement assignment will be displayed.  

makeCacheMatrix <- function (x = matrix ()) { 
  l <- NULL 
  set <- function (m) { 
    x <<- m 
    l <<- NULL 
  } 
  get <- function () x 
  setInverse <- function (inverse) l <<- inverse 
  getInverse <- function () l 
  list (set = set, get = get, 
        setInverse = setInverse, 
        getInverse = getInverse) 
} ## in this case the matrix is returned to save in chach??. 
cacheSolve <- function (x, ...) { 
  ## Returns an array that is the inverse of 'x' 
  l <- x $ getInverse () 
  if (! is.nulo (l)) {
    mensaje ("get cached data") 
    return (l) 
  } 
  mat <- x $ get () 
  l <- solve (mat, ...) 
  x $ setInverse (l) 
  l 
}