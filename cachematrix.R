## Caching a the inverse of a matrix
## The functions compute the inverse of a matrix in the cache rather than compute it repeatedly



##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

       # initialize a matrix inv
        inv <- NULL
        
        # define set() function
        set = function(y) {
                x <<- y     
                inv <<- NULL
        }
       
         #define get() function
        get = function() x
        
         # define setinv() function
        setinv = function(inverse) inv <<- inverse 
        
        # define getinv() function
        getinv = function() inv
        
        # return a list to the function call
        
        return(list(set=set, get=get, setinv=setinv, getinv=getinv)   )
       
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        
        # initialize inv
        inv = x$getinv()
        
        # if the inverse inv has been already calculated
        if (!is.null(inv)){
                # return it from the cache and display a message
                message("getting cached data")
                return(inv)
        }
        
        # otherwise calculates it  
        m = x$get()
        inv = solve(m)
        
        # sets inv in the cache using the  function setinv
        x$setinv(inv)
        
        # return the inverse of x
        return(inv)
       
}
