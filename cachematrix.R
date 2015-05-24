## The first function, makeCacheMatrix, creates a special "matrix" object that can cache its inverse.


# stores the cached value
makeCacheMatrix <- function(x = matrix()) {     
        
        # sets intial cache value to NULL
        cache <- NULL                           
        
        # creates the matrix in the working environment
        set <- function(y) {                     
                x <<- y
                cache <<- NULL
        }
        
        # gets the value of the matrix
        get <- function() x
        
        # inverts the matrix and stores in the cache
        setmatrix <- function(inverse) cache <<- inverse
        
        # gets the inverted matrix from cache
        getinverse <- function() cache
        
        # returns the functions to the working environment
        list(set=set, get=get, setmatrix=setmatrix, getinverse=getinverse)
}


## The second function, cacheSolve, calcluates the inverse of the matrix created in makeCacheMatrix.
## If the inverted matrix does not already exist in cache,
## it is created in the working environment and it's inverted value is stored in cache.

## attempts to get the inverse of the matrix stored in cache
cacheSolve <- function(x, ...) {
        
        cache <- x$getinverse()
        
        # returns inverted matrix from cache if it exists
        if (!is.null(cache)) {
                message("getting cached data")
                
                # displays matrix
                return(cache)
        }
        
        # creates matrix if it does not exist
        matrix <- x$get()
        
        # sets and returns inverse of matrix
        cache <- solve(matrix, ...)
       
        # sets inverted matrix in the cache
        x$setmatrix(cache)
        
        # displays matrix
        return (cache)
}


## Finally, perform a simple test of the two functions.

mat1 <- matrix(1:4, nrow=2, ncol=2)
mat2 <- makeCacheMatrix(mat1)
cacheSolve(mat2)