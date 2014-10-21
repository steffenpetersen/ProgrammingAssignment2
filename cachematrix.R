## This task is assignment 2 of the R programming course offered by Johns Hopkins on coursera.

## The first function will create a matrix object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
            x <<-y
            m<<-NULL
        }
        get <- function() x
        setsolve  <- function(solve) m<<-solve
        getsolve  <- function() m
        list(set=set, get=get,
             setsolve=setsolve,
             getsolve = getsolve)
}


## This function will first check wether the solution for the task to determine the inverse of the square matrix is in the cache
## if yes it will take the result from the cache and thus avoid a time consuming task
## if no it will solve the task but then save the result to the cache to help future functions be potentially more computationally efficient.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
