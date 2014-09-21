## Below are two functions: makeCacheMatrix() and cacheSolve() which serve the purpose of 
## calculating the inverse of a given matrix and preserving the calculations for future use

## makeCacheMatrix() takes as input a matrix and returns a list of functions representing this 
## matrix and it's inverse


makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL	# erasing inverse while making new cache matrix
##
## variables x and inv are stored in the envirement
## of function makeCacheMatrix and they can be accessed by
## following functions:
##
        set <- function(y)	# function for altering the matrix
	{
                x <<- y		# assigning new matrix
                inv <<- NULL	# erasing the inverse data
        }
        get <- function() x	# function returning the stored matrix
        set.inverse <- function(inverse) # function for storing the inverse
	{
		inv <<- inverse
	}
        get.inverse <- function() inv # function for retriving the inverse
        list(set = set, get = get,
             set.inverse = set.inverse,
             get.inverse = get.inverse) # returned list of functions

}


## cacheSolve() takes as input the list representing the given matrix created by the function
## above and returns the matrix inverse. This function checks whether the inverse has been
## calculated and either retrieves the earlier stored data or calculates the inverse
## and saves the data for future purpose

cacheSolve <- function(x, ...) {
        
  	inv <- x$get.inverse() # Get the stored inverse.
        if(!is.null(inv)) { # If the inverse was calculated previously, ...
                message("getting cached data")
                return(inv) # ... return the stored inverse.
        }
        data <- x$get() # Otherwise, get the stored matrix ...
        inv <- solve(data, ...) # ... inverse it, ...
        x$set.inverse(inv) # ... store the inverse ...
        inv # ... and return it

}
