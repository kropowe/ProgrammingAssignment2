## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions are used to cache the inverse of a matrix.

## The first function "makeCacheMatrix" creates a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The second function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse and sets the value in the cache
## that can be used in the future.

## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Example:
## Lets' create an inversible 3 by 3 matrix:
## > x = rbind(c(7, 2, 1), c(0, 3, -1), c(-3,4,-2))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1] [,2] [,3]
## [1,]    7    2    1
## [2,]    0    3   -1
## [3,]   -3    4   -2

## We want an inverse of the set matrix so we call a cacheSolve function.
## As there is nothing cached, the inverse is calculated. The result is also
## cached now.
## > cacheSolve(m)
##      [,1] [,2] [,3]
##  [1,] -2    8   -5
##  [2,]  3  -11    7
##  [3,]  9  -34   21

## Retrieving from the cache in the next run will result in getting cached value
## without the need of computing it again.
## > cacheSolve(m)
## getting cached data.
##      [,1] [,2] [,3]
## [1,]   -2    8   -5
## [2,]    3  -11    7
## [3,]    9  -34   21

## Just to test if a cached matrix is an inverse of x (the result of
## multiplying an original input x and its inverse should be an identity matrix):
## > x %*% cacheSolve(m)
## getting cached data.
##        [,1] [,2] [,3]
## [1,]      1    0    0
## [2,]      0    1    0
## [3,]      0    0    1
## Acknowledgements: I have used some partial solutions to the problem from
## from code presented on Stack Overflow as questions. I have amended and fixed
## where necessary and I have a good understanding of how it works now.
## I find this assignment to be a bit too advanced to expect a beginner
## to solve from scratch without any help from online sources.