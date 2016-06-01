## Below are two functions that are used to create a special object that stores a Matrix and caches its inverse. 
## The first function, "makeCacheMatrix" creates a special "Matrix" object that can cache its inverse. Its function is to .
## 1 - set the value of the matrix
## 2 - get the value of the matrix
## 3 - set the value of the inverse
## 4 - get the value of the inverse


makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## The following function calculates the inverse of the matrix created with the above function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse (getInverse) from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) 
{
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
