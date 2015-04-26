
## The Functions below will cache the inverse of a matrix.

## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
  { 
## reset or clear the working variables 
       m <- NULL 
       set <- function(y) 
       { 
           x <<- y 
           m <<- NULL 
       } 
       get <- function() x 
       
## setInverse function stores the inverse result into variable 'm'  
       setInverse <- function(inverse) m <<- inverse 
## getInverse function retrieves the current inverse value (if it exists) stored in 'm' 
       getInverse <- function() m 
## The four functions set, get, setInverse, and getInverse, stored as a list and returned when the function 
## is called. 
       list(set = set, get = get, 
            setInverse = setInverse, 
            getInverse = getInverse) 
  } 


## cacheSolve is a function that computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) 
  { 
## Return a matrix that is the inverse of 'x' 
       m <- x$getInverse() 
## Check the cache - if m contains a result then this result is returned 
       if(!is.null(m))
         { 
           message("getting cached data") 
           return(m) 
         } 
       data <- x$get() 
## Calculate the inverse of the matrix 'x' and caches the result in 'm'  
       m <- solve(data, ...) 
       x$setInverse(m) 
       m 
  } 

