## created by Krish Soundar
## using template of R programming week 2 assignment GitHub code
## Feb 17, 2015

## function makeCacheMatrix takes a matrix and returns
## a list of functions to set and get the matrix data and to set and get
## the inverse of the matrix. 
## passing the returned list to the companion function cacheSolve shown below
## outputs the inverse object either from cache or from computation.

makeCacheMatrix <- function(x = matrix()) {
 
  #create the inverse object to be returned 
  inv <- NULL 
 
  ##set function sets the matrix data to variable outside current environment.
  ##and initializes inverse object to null.
  set <- function(y) {
	 x <<- y  
       inv <<- NULL
  }
  
  ## get function returns the matrix data if it already exists in cache.
  get <- function() x

  ## setinv sets the inverse of the matrix x by calling solve
  setinv <- function(solve) inv <<- solve


  ## getinv retrieves the inverse of the matrix x from cache.
  getinv <- function() inv
 
  
  ## return the list of named functions
  list(set  = set, get= get, 
	 setinv = setinv, 
       getinv = getinv)
       
}


## cacheSolve returns the inverse of a matrix passed to it
## If the inverse exists in cache then it skips the computation
## Or else it computes the inverse of the matrix, caches it 
## and returns it.

## Usage cacheSolve(makeCacheMatrix(x)
## x is input matrix for which inverse is needed

cacheSolve <- function(x, ...) {
        inv <- x$getinv()

	  ##check if inverse exists by checking if obj inv is null
        if (!is.null(inv)) {
		message("retrieving the cached inverse")
            return(m) ##return the cached inverse object
        }
        
        ##since inverse object does not exist in cache
        ##get the matrix through get function
        data <- x$get() 
        
        ##compute inverse using solve function 
        inv <- solve(data, ...) 
         
        ##set the computed inverse object in cache
        x$setinv(inv)
 
        ##return the inverse object
        inv      
}
