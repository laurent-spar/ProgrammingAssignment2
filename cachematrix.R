## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Cette fonction crée un objet spécial "matrice" qui peut mettre en cache son inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

## Cette fonction calcule l'inverse de la "matrice" spéciale renvoyée par makeCacheMatrix.
## Si l'inverse a déjà été calculé (et que la matrice n'a pas changé), elle récupère l'inverse dans le cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}