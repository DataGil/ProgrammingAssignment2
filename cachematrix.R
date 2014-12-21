makeCacheMatrix <- function(x = matrix()) {      # input x will be a matrix
        
        m <- NULL    #  m will be our 'inverse' and it's reset to NULL every 
        #    time makeCacheMatrix is called
        
        #  note these next three functions are defined but not run when makeCacheMatrix is called.
        #   instead, they will be used by cacheSolve() to get values for x or for
        #   m (inverse) and for setting the inverse.  These are usually called object 'methods'
                
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() { x }   # this function returns the value of the original matrix
        
        setinverse <- function(solve)  { m <<- solve }
        # this is called by cacheSolve() during the first cacheSolve()
        #  access and it will store the value using superassignment
        
        getinverse <- function() { m } # this will return the cached value to cacheSolve() on
        #  subsequent accesses
        
        list(set = set, get = get,          #  OK, this is accessed each time makeCacheMatrix() is called,       
             setinverse = setinverse,  #   that is, each time we make a new object.  This is a list of 
             getinverse = getinverse)  #   the internal functions ('methods') so a calling function
        #   knows how to access those methods.                            
}


cacheSolve <- function(x, ...) {   # the input x is an object created by makeCacheMatrix
        m <- x$getinverse()               # accesses the object 'x' and gets the value of the inverse
        if(!is.null(m)) {              # if inverse was already cached (not NULL) ...
                
                message("getting cached data")  # ... send this message to the console
                return(m)                       # ... and return the inverse ... "return" ends 
                #   the function cacheSolve(), note
        }
        data <- x$get()        # we reach this code only if x$getinverse() returned NULL
        m <- solve(data, ...)   # if m was NULL then we have to calculate the inverse
        x$setinverse(m)           # store the calculated inverse value in x (see setinverse() in makeCacheMatrix
        m               # return the inverse to the code that called this function
}