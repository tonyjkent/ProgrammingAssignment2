## Introduction to R - Programing Assignment 2
## Tony Kent 12/12/2014
##
## These functions will store the inverse of a matrix so it doesn't have to be recomputed 
## each time you need it.
## 
## Usage:
## use the makeCachehMatrix function to hold the value of the orginal matrix
## eg.  amatrix=makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## then use the cacheSolve matrix to retrieve (or calcualte if it hasn't done so already) the inverse matrix
## eg. cacheSolve(amatrix)


## makeCacheMatrix function 
## this function creates holds the value of the original matrix and the inverse matrix
makeCacheMatrix <- function(x = matrix()) {

      m <- NULL  ## m contains the inverse matrix
      set <- function (y) {  ## sets the value of the original matrix
            x <<-y
            m <<-NULL  ## reset's the solution of NULL in the global environment. (so CacheSolve can test for it)
      }
      get <- function() x  ## retrieves the value of the original matrix
      setinverse <- function(solve) m <<- solve    ## solves for the inverse sets the value to m in the global environment
      getinverse <- function() m ## retrieves the value of inverse matrix, if it has been solved.  
      list (set = set, get=get, setinverse = setinverse, getinverse=getinverse)
}

## cachesolve function:
## cachesolve looks to see if the inverse has already been calculated
## if it has been calcualted, return the value that is stored in the cache.
## otherwise call the solve function to calculate the inverse and store that value in the casche and return the value.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m  <- x$getinverse()
      if(!is.null(m)) {  ## tests i the inverse has already been calcualted
            message("getting cached data")
            return(m)
      }
      data <- x$get() ## if the inverse hasn't been calculated, solve it now
      m <- solve(data, ...)
      x$setinverse(m)
      m
}

