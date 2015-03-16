## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix creates a list of 4 functions: 
# 2 to manipulate input matrix (set it and get it)
# and 2 to manipulate cached value of inversed matrix
# (get inversed matrix and set inversed matrix)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

# Function algorithm:
# 1) checking if matrix is squared: standart deviation equals
# 0 if both dimensions are equal; If matrix is equal, proceed
# with next steps. In case of non-square matrix, show message.
# 2) checking if cache variable contains inverse matrix. If 
# contains - write appropriate message and skip step 3.
# 3) calculating inversed matrix and putting it into cache 
# variable.
# 4) Return value of inversed matrix.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (sd(dim(x$get()))=0) {
        if (is.null(inv)) {
            data <- x$get()
            inv <- solve(data)
            x$setinverse(inv)
        } else {
            message("getting cached data")
        }
        inv
    } else {
        message("matrix is not square")
    }
}


