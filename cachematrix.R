## This functions are ment to construct a matrix, cache it´s inverse, 
## retrieve the cache invert and compute theinverse if the matrix has been changed


## 1. Creates a matrix
## 2. Sets it´s inverse
## 3. Gets and caches the inverse

makeCacheMatrix <- function(x = matrix()) {
    matx <- NULL
        set <- function(y) {
                x <<- y
                matx <<- NULL
        }
        get <- function() x
        setinv <- function( solve (matx) %*% matx == diag(nrow = nrow(matx), ncol = ncol(matx))) matx <<- inverse
        getinv <- function ()matx
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## 1. Comcupes the inverse of the matrix above.
## 2. If theinverse has already been calculated,it retrieves it from the cache.
## 3. If not, it calculates the inverse matrix of the data.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get(as.matrix())
        inv <- solve(data)
        x$getinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
