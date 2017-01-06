## Matrix inversion generally takes time to compute, it is a good 
## idea to use cached calculations for heavy operations such as this  

## Below functions can be used to cache the inverse of the matrix    

## This makes a list containing the environments to 
## set/get the value of matrix
## set/get the value of inverse matrix (totally 4)

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) 
        {
        x <<- y
        m <<- NULL
        }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## this function chhecks whether the inverse has already been calculated or not, if calculated it prints "getting cached inverse", else it calculates the inverse using the solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached inverse")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

#checking

# x <- matrix(c(4,2,7,6),2,2)
# z = makeCacheMatrix(x)

#getting the matrix
# z$get()
# [,1] [,2]
# [1,]    4    7
# [2,]    2    6

#solving the first time

# cacheSolve(z)
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4

#solving the secindd time

# cacheSolve(z)
# getting cached inverse
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4
