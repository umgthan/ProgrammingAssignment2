
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	# Changes when the user sets the value
    inve <- NULL

    # set function
    # Sets the matrix itself but not the inverse
    set <- function(m) {
            x <<- m
            inve <<- NULL
    }

    # get function
    # Gets the matrix itself but not the inverse
    get <- function() x

    # Manually set the inverse matrix
    setinverse <- function(inverse) inve <<- inverse

    # Get the inverse matrix
    getinverse <- function() inve

    # put them together in the list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # Check to see metrix was already inverse 
    
    inve <- x$getinverse()
	
    # If it has...
    if(!is.null(inve)) {
    	# Just turn the result	
        message("Here is the cache result found")		
        return(inve)
    }

    # Get the matrix itself
    data <- x$get()

    # Find the inverse
    inve <- solve(data, ...)

    # Cache this result in the object
    x$setinverse(inve)

    # Return this new result
	 message("Reverse Matrix")
    inve    
}


## Please use following function call to test twi functions
## x <- matrix(1:4, nrow=2, ncol=2)
## m <- makeCacheMatrix(x)
## s <- cacheSolve(m)
## type s to see content of the s
## s
## s should return:
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
## s2 <- cacheSolve(m)
## This should display a "Getting cached matrix" message
## print(s2)
## s2 should return
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5