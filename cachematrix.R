## Put comments here that give an overall description of what your
## functions do

## The first function, makeMatirx creates a special "Matrix", which is really a list containing a function to
##set the value of the Matirx
##get the value of the Matirx
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        Rsolve <- NULL
        set <- function(y){
                x <<- y
                Rsolve <<- NULL
        }
        get <- function() x
        setRsolve <- function(Realsolve) Rsolve <<- Realsolve
        getRsolve <- function() Rsolve
        list(set = set, get = get,
             setRsolve = setRsolve,
             getRsolve = getRsolve)
}


## The following function calculates the inverse of the special "Matrix" created with the above function.
##However, it first checks to see if the inverse has already been calculated.
##If so, it gets the inverse from the cache and skips the computation.
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setRsolve function.

cacheSolve <- function(x) {
        Rsolve <- x$getRsolve()
        if(!is.null(Rsolve)){
                message("getting cached data")
                return(Rsolve)
        }
        data <- x$get()
        Rsolve <- solve(data)
        x$setRsolve(Rsolve)
        Rsolve
}

