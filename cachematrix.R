## functions for inverting a matrix using a caching implementation so 
## repeated calls to invert the same matrix will execute very quickly

## create a cache of the inverse of a matrix
## @param x : a square, invertible matrix
## @return : a list of functions for accessing the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    #cached inverse of the current matrix
    inverted <- NULL 
    
    #set a new matrix
    setMatrix <- function(newMatrix)
    {
        if(!identical(newMatrix, x))
        {
            x <<- newMatrix
            inverted <<- NULL
        }
    }
    
    #get the matrix
    getMatrix <- function()
    {
        return(x)
    }
    
    #return inverse of matrix
    getInverse <- function()
    {
        #note: we assume x is always invertible
        if(!is.null(inverted))
        {
            message("getting cached data")
            return(inverted)
        }
        
        inverted <<- solve(x)
        return(inverted)
    }
    
    return( list(setMatrix=setMatrix, 
                 getMatrix=getMatrix, 
                 getInverse=getInverse) )
}


## inverts a matrix
## @param x : a "special cached matrix" created with makeCacheMatrix
## @return : an inverted copy of x
cacheSolve <- function(x, ...) {
    return( x$getInverse() )
}
