## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
       #set the value of the matrix
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        #get the value of the matrix
        get <- function() x
        #set the value of the inverse
        setinverse <- function(inverse) inver <<- inverse
        #get the value of the mean
        getinverse <- function() inver
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
                }
                data <- x$get()
                inver <- solve(data, ...)
                x$setinverse(inver)
                inver
        }
