##this function creates a vector composed of three functions:
#getter which returns the value of the matrix 'x'
#setter which sets the the value of the 'inv' object to the inverse of a matrix
#getter which returns the value of the 'inv' object
makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    
    get <- function(){ x }
    setInverse <- function(inverse){ inv <<-inverse } 
    getInverse <- function() { inv }
    
    list(get = get , 
            setInverse = setInverse , 
                getInverse = getInverse)
}


#this function computes the inverse of matrix 'x' using the solve function:
#if the inverse has been previously calculated then the value is sourced from the cache,
#otherwise, the inverse is calculated using the solve function and stored in the cache
#via the setInverse function
cacheSolve <- function(x, ...) {
        
    inv <- x$getInverse()
    
    if(!is.null(inv))
    {
        message("getting the inverse of matrix from cache")
        return(inv)
    }
    
    matrix <- x$get()
    inv <- solve(matrix)
    x$setInverse(inv)
    
    inv
}
