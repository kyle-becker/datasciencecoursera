MakeCasheMatrix <- function(x = matrix()) {
  ## initialize m in the function's env; set to NULL
  I <- NULL
  
  ## create set() closure to:
  ##   - set x (in makeVector()'s env)
  ##   - clear m (also in makeVector() env)
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  
  ##
  get <- function(){ x }
  
  setInverse <- function(Inverse){ I <<- Inverse }
  
  getInverse <- function(){ I }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cachesolve <- function(x, ...) {
  I <- x$getInverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setInverse(I)
  I
}


Matrix <- MakeCasheMatrix(matrix(1:4,2,2))
print(Matrix$getInverse())

cachesolve(Matrix)
