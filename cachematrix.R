## Put comments here that give an overall description of what your
## functions do


## creates a cache matrix of values of x and their inverses
makeCacheMatrix <- function(x = matrix()) 
{
    ## initializes m as an object in my function
    m <- NULL
    set <- function(y)
    {
        ## sets x to the value of y in the parent environment
        x <<- y
        
        ##clear m in case cacheSolve has been called previously
        m <<- NULL
    }
    
    ##retrieves x from the parent environment
    get <- function() x
    
    ## defines the setter for the inverse of m
    setsolve <- function(solve) m <<- solve
    
    ## defines the getter for the inverse of m
    getsolve <- function() m
    
    ##this is the cache of data
    list(set = set, get = get, setsolve = setsolve, getsolve
         = getsolve)
}


## takes a makeCacheMatrix matrix and returns its inverse
cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    
    ## attempts to retrieve an inverse from the input object
    m <- x$getsolve()
    
    ## checks to see if the result has already been cached
    if(!is.null(m)) ##if m is not null it has been cached already
    {
        message("getting cached data")
        return(m)
    }
    
    ## this runs if the inverse had not been cached already
    
    ## gets the input matrix
    data <- x$get()
    
    ##calculates the inverse of the input matrix
    m <- solve(data, ...)
    
    ## sets the inverse in the input matrix
    x$setsolve(m)
    
    ## returns the inverse matrix
    m
}
