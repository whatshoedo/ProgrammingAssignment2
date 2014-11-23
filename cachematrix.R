
## The makeCacheMatrix function cache's the inverse of a maxtrix being passed to it
## and utilizes the solve function.
## There are several functions created here explained in more detail below

makeCacheMatrix <- function(x = matrix()) {
 
        ## m vector is being intialized to NULL
        m <- NULL
        ## the set function is created here and passed y
        set <- function(y) {
            ## the x matrix is assigned the matrix y being passed to the set function
            ## x is assigned to the global environment with the <<- operator
            x <<- y
            ## m is initalized and cached in the global environment
            m <<- NULL
        }
        ## the get function simply returns the assigned or set matrix
        get <- function() x
        ## the setinverse function uses the solve function to set the inverse
        ## of matrix m when it is passed
        setinverse <- function(solve) m <<- solve
        ## getinverse function simply displays the matrix m being inversed
        getinverse <- function() m
        ## the list function is invoked and the list of arguments are assigned
        ## as elements of the list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cacheSolve function returns the inverse of a matrix and caches it
## If the 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of x
    m <- x$getinverse()
    ## if the m matrix is not null then return a message stating
    ## "getting cached data" and return the cached matrix and exit
    ## the function
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## if the m matrix is null and hasn't been cached
    ## assign data the matrix by calling the makeCacheMatrix get sub-function
    data <- x$get()
    ## assign m the inverse matrix by using the solve function
    m <- solve(data, ...)
    ## call the setinverse sub-function from the makeCacheMatrix function
    ## m is assigned the inversed function in the sub-function
    x$setinverse(m)
    ## m is returned
    m
}

