## Put comments here that give an overall description of what your
## functions do


# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    InvMat <- NULL
    set <- function(y){
        x <<- y
        InvMat <<- NULL
    }
    get <- function x
    setInv <- function(solve) InvMat <<- solve
    getInv <- function InvMat
    list(set=set, get=get, setInv=setInv,getInv=getInv)
}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already
##been calculated (and the matrix has not changed), then the 
##cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    Inv <- x$getInv()
    if(!is.null(Inv)){
        message('getting cached data')
        return(Inv)
    }
    data <- x$get()
    m <- solve(data)
    x$setInv(Inv)
    Inv    
}
