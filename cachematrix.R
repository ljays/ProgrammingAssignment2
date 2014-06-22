## Author: L. Jays 
## Source: Roger D. Peng, https://github.com/rdpeng/ProgrammingAssignment2
## 1) The makeCacheMatrix() function creates a list object consisting of
##    four functions: setting a matrix, getting the matrix, setting the
##    inverse of the matrix, and getting the inverse of the matrix.
## 2) The cacheSolve() function checks the object created using the previous
##    function for a cached inverse for a given matrix. If the inverse has
##    been stored in the list object, it is retrieved instead of re-calcuated.

## Returns a list of four attributes, each a function, for a matrix object

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() { x }
        setinv <- function(solve) { inv <<- solve }
        getinv <- function() { inv }
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Returns the inverse matrix of matrix x - returns a cached solution if the
## inverse matrix has been previously calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("Retrieving cached inverse: ")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        x$setinv(inv)
        inv                
}
