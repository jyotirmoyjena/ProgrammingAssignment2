## Use the makeCacheMatrix to create an object that can cache an matrix inverse
## Use the cacheSolve function to invert the matrix returned by the object 
## created by the makeCacheMatrix function and cache a copy of the matrix
## in the object returned by the makeCacheMatrix function

## Returns a list that have four functions, set, get, setInv, getInv
## set -> sets the source matrix
## get -> returns the source matrix
## setInv -> sets the inverse of the source matrix
## getInv -> gets the cached copy of the inverse of the source matrix

makeCacheMatrix <- function(x = matrix()) {
	  inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverted) inv <<- inverted
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## Takes an input as the list returned by the makeCacheMatrix function and 
## inverts the input square matrix and caches the inverse in the input object
## return the inverse of the input matrix

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInv(inv)
        inv
}
