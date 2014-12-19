##  Together the following two R functions are used to create a special object 
##  that stores a matrix and caches its inverse (given that the matrix is 
##  invertible).
##
##  The first function, 'makeCacheMatrix', creates a special 'matrix object', 
##  a list containing 4 functions:
##   - set(y): set the values of the matrix
##   - get():  get the values of the matrix
##   - setInverseMatrix(inverse): set the values of the inverse of the matrix
##   - getInverseMatrix():        get the values of the inverse of the matrix
##
##  The second function, 'cacheSolve', returns the inverse of the special 
##  'matrix' created with 'makeCacheMatrix'. It first checks if the inverse has 
##  already been calculated. If so, it gets the inverse from the cache and skips
##  the computation. Otherwise, it computes the inverse of the matrix and sets 
##  the values of the inverse in the cache via the setInverseMatrix function.



## makeCacheMatrix creates a special 'matrix object' that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        imatrix <- NULL 
        set <- function(y){
                x <<- y  # Update data
                imatrix <<- NULL # No inverse calculated so far for the new data
        }
        get <- function() x 
        setInverseMatrix <- function(inverse) imatrix <<- inverse
        getInverseMatrix <- function() imatrix
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


## cacheSolve computes the inverse matrix of the special 'matrix' returned by 
## the makeCacheMatrix function. If the inverse has already been calculated (and 
## the matrix has not changed), cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        imatrix <- x$getInverseMatrix()
        if(!is.null(imatrix)){
                message("Here the cached inverse matrix")
                return(imatrix) # Return cached inverse of matrix
        }
        data <- x$get()
        imatrix <- solve(data, ...) # Compute inverse of matrix
        x$setInverseMatrix(imatrix) # Cache inverse of matrix
        imatrix  # Return inverse of matrix
}
