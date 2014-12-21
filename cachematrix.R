makeCacheMatrix <- function(x = matrix()) {      # input x will be a vector
        
        m <- NULL    #  m will be our 'mean' and it's reset to NULL every 
        #    time makeVector is called
        
        #  note these next three functions are defined but not run when makeVector is called.
        #   instead, they will be used by cachemean() to get values for x or for
        #   m (mean) and for setting the mean.  These are usually called object 'methods'
                
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() { x }   # this function returns the value of the original vector
        
        setinverse <- function(solve)  { m <<- solve }
        # this is called by cachemean() during the first cachemean()
        #  access and it will store the value using superassignment
        
        getinverse <- function() { m } # this will return the cached value to cachemean() on
        #  subsequent accesses
        
        list(set = set, get = get,          #  OK, this is accessed each time makeVector() is called,       
             setinverse = setinverse,  #   that is, each time we make a new object.  This is a list of 
             getinverse = getinverse)  #   the internal functions ('methods') so a calling function
        #   knows how to access those methods.                            
}


cacheSolve <- function(x, ...) {   # the input x is an object created by makeVector
        m <- x$getinverse()               # accesses the object 'x' and gets the value of the mean
        if(!is.null(m)) {              # if mean was already cached (not NULL) ...
                
                message("getting cached data")  # ... send this message to the console
                return(m)                       # ... and return the mean ... "return" ends 
                #   the function cachemean(), note
        }
        data <- x$get()        # we reach this code only if x$getmean() returned NULL
        m <- solve(data, ...)   # if m was NULL then we have to calculate the mean
        x$setinverse(m)           # store the calculated mean value in x (see setmean() in makeVector
        m               # return the mean to the code that called this function
}