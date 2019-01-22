## This pair of functions that cache the inverse of a matrix provided that the matrix is invertable.
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## functions do

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) Inv <<- inverse
        getInverse <- function() Inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the Inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.
## Is also calculates the determinate of the matrix. If the determinate is zero, the message "the matrix is not invertable" will appear."

cacheSolve <- function(x, ...) {
        Inv <- x$getInverse()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        matrix <- x$get()
        z=det(matrix)
        if (z==0) {
                message("the matrix is not invertable")
                }
        else {
        Inv <- solve(matrix, ...)}
        x$setInverse(Inv)
        Inv
}
