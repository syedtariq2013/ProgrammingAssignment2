## The following R functions in the file demonstrate a way to be
## able to cache potentially time-consuming computations. For
## example, taking the inverse of a matrix may take a long time, 
## especially if it has to be computed repeatedly (e.g.
## in a loop). If the contents of a matrix are not changing, it may make
## sense to cache the value of the inverse so that when we need it again, it
## can be looked up in the cache rather than recomputed.

## USAGE:
## > a <- makeCacheMatrix(matrix(1:4,2)) [create a matrix]
## > cacheSolve(a) [calculate inverse]
## > a$set(matrix(5:8,2)) [change the matrix]
## > cacheSolve(a) [calculate a new inverse]
## > cacheSolve(a) [extract inverse from the cache]

## makeCacheMatrix: creates a special "matrix", which is
## really a list containing a function to

## 1.  set: set the value of the matrix. Used to set a new matrix
## 2.  get: get the value of the matrix
## 3.  setInverse: set the value of the inverse of the matrix
## 4.  getInverse: get the value of the inverse of the matrix

## The function returns a list of the four functions described above

makeCacheMatrix <- function(x = matrix()) {
  ix <- NULL ## Note that it is ok to set the inverse matrix to NULL
  			 ## It can be used to determine that this is a new matrix

  set <- function(y) {
    x <<- y
    ix <<- NULL

  }
  get <- function() x
  setInverse <- function(solve) ix <<- solve
  getInverse <- function() ix

  list(set = set, 
       get = get,
       setInverse = setInverse, 
       getInverse = getInverse)	
}


# The following function calculates the inverse of the special "matrix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it gets the inverse (using getInverse())
# from the cache and skips the computation. Otherwise, it calculates the inverse of
# the data (using library function solve() and sets the value of the inverse 
# in the cache using the setInverse() function

cacheSolve <- function(x, ...) {

## x is the list of functions returned from the makeCacheMatrix function

  ix <- x$getInverse()
  if(!is.null(ix)) {
    message("getting cached data")
    return(ix)
  }
  data <- x$get()
  ix <- solve(data)
  x$setInverse(ix)
  ix
}
