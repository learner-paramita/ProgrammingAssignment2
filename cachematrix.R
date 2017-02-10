## The below functions find the inverse of a matrix 
## The function will calculate and cache the inverse of a new matrix the first time 
## Next time onwards the below functions will read the inverse of the same matrix from the cache
## Inverse calculation will happen only when a new matrix is entered.


## This function creates a special matrix that can store the inverse of a matrix and retrieve it

makeCacheMatrix <- function(x = matrix()) {
	matrix_inverse <- NULL
        	set <- function(y) {
                x <<- y
                matrix_inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) matrix_inverse <<- inverse
        getinverse <- function() matrix_inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function calculates the inverse of a matrix when it is a new one. If it is an old one then
## it uses the cached value to return the inverse of the given old matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	 matrix_inverse <- x$getinverse()
        if(!is.null(matrix_inverse)) {
                message("getting cached data")
                return(matrix_inverse)
        }
        my_data <- x$get()
        matrix_inverse <- solve(my_data, ...)
        x$setinverse(matrix_inverse)
        matrix_inverse
}
