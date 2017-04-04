## Put comments here that give an overall description of what your
## functions do
###These functions inverse a given matrix and cache it for later calling. 

## Write a short comment describing this function
### makeCacheMatrix function takes a matrix input then creates a list object of functions
### that set/get the value of matrix, set/get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    invs_mtx <- NULL
    set <- function(y=matrix()) { ##<<- operator sets values in GlobEvirm.
        x <<- y
        invs_mtx <<- NULL
    }
    get <- function() {x}
    setinvs <- function (invs_from_solve) {invs_mtx <<- invs_from_solve}
    getinvs <- function() {invs_mtx}
    list(set=set, get=get, setinvs=setinvs, getinvs=getinvs)
}


## Write a short comment describing this function
### cacheSolve function first checks if there is any previously cached result for 
### a particular matrix, if not, inverse the matrix by using the solve() function
### and caches the new result

cacheSolve <- function(matrix_instance, ...) {
    invs_mtx <- matrix_instance$getinvs()
    if(!is.null(invs_mtx)) {
        message("getting cached data")
        return(invs_mtx)
    }
    data <- matrix_instance$get()
    invs_mtx <- solve(data, ...)
    matrix_instance$setinvs(invs_mtx)
    invs_mtx
        ## Return a matrix that is the inverse of 'x'
}
