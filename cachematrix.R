## MakeCacheMatrix fucntion creates a special "matrix" object that can caches
## its inverse
## CacheSolve function computes the inverse of the matrix returned by 
## MakeCacheMatrix
## If the inverse has already been caculated, then CacheSolve
## should retrieve the inverse from the cachen

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y){
                 x <<- y
                 inv <<- NULL
            }
         get <- function() x
         setinv <- function(inverse) inv <<- inverse
         getinv <- function() inv
         list(set = set, get = get, setinv = setinv, getinv = getinv)

}



cacheSolve <- function(x, ...) {
        ## Return a  matrix that is the inverse of 'x'
         inv <- x$getinv()
       ## If the inverse has already been calculated (and the matrix has not changed)   
         if(!is.null(inv)) {
             message("getting cached data")
             return(inv)      
        }
         data <-x$get()
         inv <-solve(data,...)
         x$setinv(inv)
         return(inv)

}

