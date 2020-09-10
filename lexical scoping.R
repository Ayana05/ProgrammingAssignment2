#There are two functions makeCacheMatrix,makeCacheMatrix
#makecachematrix create a special "matrix" object that can cache its inverse
#cachesolve: This function computes the inverse of the special "matrix" returnrd by makeCacheMatrix above.
#if the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.
#makeCacheMatrix consists of set,get,setinv,getinv
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                   #initialising inverse as null
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x                           #function to get matrix x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#this is used to get Cache data

cacheSolve <- function(x, ...) {             #gets cache data
  inv<- x$getinverse()
  if(!is.null(inv)) {                        #checking whether inverse is null
    message("getting inversed matrix")
    return(inv)                              #return inverse value
  }
  data <- x$get()
  inv<- solve(data, ...)                      #calculates the inverse value
  x$setinverse(inv)                              
  inv                                          #returns a matrix that is inverse of 'x'                   
} 
