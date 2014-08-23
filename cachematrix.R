# this function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(m=matrix()) # input m is a matrix
{
    i <- NULL # i is our inverse which is reset to NULL everytime makeCacheMatrix is called
    set <- function(y) # takes an input matrix
    {
        m <<- y # saves the input matrix
        i <<- NULL # resets the inverse to NULL when a new object is generated
    }
    get <- function() {m} # this function returns the value of the original matrix
    setinverse <- function(solve) # this is called by cacheSolve() during the first cacheSolve() access
    {i <<- solve} # and it will store the value using superassignment
    getinverse <- function() {i} # this returns the cached value to cacheSolve() on subsequent accesses
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse) # this list returns with the newly created object
}

# this function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(m,...) # the input is an object created by makeCacheMatrix
{
    i <- m$getinverse() # accesses the object 'm' and gets the value of inverse
    if (!is.null(i)) # if inverse was already cached (not NULL)...
    {
        message("getting cached data") # ...send this message to the console
        return(i) # ...and return the inverse... "return" ends the function cacheSolve()
    }
    data <- m$get() # if m$getinverse() returns NULL
    i <- solve(data,...) # if i was NULL then we have to calculate the inverse
    m$setinverse(i) # store the calculated inverse value in m
    i # return the inverse to the code that calls this function
}

# examples to call these functions
# > m1 <- makeCacheMatrix(matrix(1:4,2,2))
# > cacheSolve(m1)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > m2 <- makeCacheMatrix(matrix(5:8,2,2))
# > cacheSolve(m2)
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5
# > cacheSolve(m2)
# getting cached data
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5