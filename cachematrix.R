## makeCacheMatrix and cache Solve are evolved versions of the functions shown in the example but
## design to work for a matrix object input and to calculate the inverse of a matrix.

## The aim of the first function will be to create and extended object (specifically a list) from an 
##originalmatrix ('x') given to the system. This new object is formed itself by functions, that
##allow the user to extract the value of the matrix and to modify it and at the same time doanalogously
##with the inverse of the matrix ('inv'). The new object is formed by:
##
##      1- 'set': a function that given an external value('y') redefines the value stored of the matrix (x <<- y)
##      and erases the previous stored value of the inverse matrix (inv <<-NULL,logical as is no longer valid).
##      2-"get": a function that returns the last stored value of the matrix in the vector.
##      3-'setinv': a function that given an external value('solve') redefines the value stored of the matrix
##      (inv <<- solve)
##      4-'getinv':a function that returns the last stored value of the inverse matrix in the vector.
##
##It's worth to notice that this functions doesn't calculate on its calling the inverse of the given matrix 'x'
##leaving its value empty.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## cacheSolve is a function that returns the inverse of a given matrix ('inv'), when this is given in the vectorial 
##format created with makeCacheMatrix. First it checks if the inverse matrix it's already stored in the vector
##(by if(!is.null(inv))), if that's the case then it simply returns it, if not,it takes the value of the
##original matrix (matx <- x$get()) and the fuctions iself calculates it (inv <- solve(matx, ...))
## Before returning it to the user, this value is stored in the vector (x$setinv(inv)).


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data of inverse of matrix")
                return(inv)
        }
        matx <- x$get()
        inv <- solve(matx, ...)
        x$setinv(inv)
        inv
}
