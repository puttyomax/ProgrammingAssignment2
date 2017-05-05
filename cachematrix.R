## These two functions enable your computer to skip caliculating matrice's inverses when the calculation has been done before 

## This function provides your computer with a list of functions that are components of the process to make cache

makeCacheMatrix <- function(x = matrix()) {
        INV<-NULL
        set <- function(y) {
                x <<- y
                INV <<- NULL
        }
        get <- function() x
        setinv <- function(inv) INV <<- inv
        getinv <- function() INV
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

        
}


## If you have calculated the inverse of x before, INV contains some value and therefore "result" happen to have the value 
## in the first place and second calculation will be skipped by "if(!is.null(result))" loop

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        result <- x$getinv()
        if(!is.null(result)) {
                message("getting cached data")
                return(result)
        }
        data <- x$get()
        result <- solve(data,...)
        x$setinv(result)
        result
}
