## Below function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.

## reate a function which starts with a null matrix argument

makeCacheMatrix <- function(x = matrix()) {
## initialize the value of the matrix inverse to NULL
       i <- NULL
## delcare a function "set" where the value will be cached
       set <- function(x) {
## use `<<-` to assign a value to an object in an environment 
## different from the current environment. 
## value is cached when the matrix is created for the first time
           x <<- y
 ## change the value of inverse of the matrix in case the matrix was changed.
           i <<- NULL
       }
## gets the value of the inverse
       get <- function() x
#calculates the inverse of non-singular matrix via the solve function
       setinv <- function(solve) i <<- solve
  # gets the inverse   
       getinv <- function() i
## passes the value of the function makeCacheMatrix  
       list(set = set, get = get, setinv = setinv, getinv = getinv)

}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

# used to get the cache of the matrix
cacheSolve <- function(x, ...) {
 ## @x: output of makeCacheMatrix()
 ## return: inverse of the original matrix input to makeCacheMatrix()
        i <- x$getinv()
#if the inverse exists, it gets it.
        if(!is.null(i)) {
 # get it from the cache and skips the computation.
                message("getting cached data")
                return(i)
        }
#if the inverse if not there, first it is calculated and then retrieved.
        data <- x$get()
        i <- solve(data, ...)
# sets the value of the inverse in the cache via the setinv function.
        x$setinv(i)
        return(i)
}
