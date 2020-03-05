## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    #m stores the matrix inverse
    m <- NULL
    #set the input matrix
    setMatrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    #get the input matrix
    getMatrix <- function()  x
    
    #Set the inverse of the matrix
    setInv <- function(inv) m <<- inv
    #get the inverse of matrix
    getInv <- function() m
    
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInv = setInv, getInv = getInv)
}


## Returns cached inverted matrix or returns computed inverted matrix 
# (and caches it)

cacheSolve <- function(x, ...) {
    # Gets cached matrix that is the inverse of 'x'
    i <- x$getInv()
    if(!is.null(i)) {
        message("Getting cached inverted matrix")
        return(i)
    }
    
    data <- x$getMatrix()
    i <- solve(x)
    x$setInv(i)
    i
}
