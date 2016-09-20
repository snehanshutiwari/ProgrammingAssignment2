## Snehanshu Tiwari



## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function( x = matrix() )
   {

        # "m" varaible is set to null in function makeCacheMatrix environment 
		m <- NULL
		
		# set functiont is used to set values of variable "x" and "m" for all environment. i.e., Visible (same) to all functions
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		# get function is used to fetch the value of variable "x" 
        get <- function() x
		# this function, sets the value of variable "m" for all the environment.
        setInverse <- function(inverse) m <<- inverse
		# fetch the inverse value
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

	
   }


## This function computes the inverse of the special matrix returned by makeCacheMatrix" above. If the inverse has already been calculated (and the matrix has not changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x)
 {
 
    # Now this function checks if inverse already exists. As we used "<<-" to set the inverse "m" in makeCacheMatrix fucntion, it is visible in cacheSolve function too.
    # we are using the same variable name "m" here. But this change will not be visisble to other functiont as we used "<-", making the change local to this environment or function
	m <- x$getInverse()
 
 
    # This if block checks if we got not null value. If yes, return the not null value and exit from function using return statement.
    if( !is.null(m) )
    {
            message("getting cached data")
            return(m)
    }
	
	# If we received null value,  proceed by getting the matrix and calulate the inverse.
  
    data <- x$get()
  
    m <- solve(data) %*% data
  
    # now set the inverse.
    x$setInverse(m)
  
    m
}


