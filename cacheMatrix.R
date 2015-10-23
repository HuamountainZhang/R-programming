## when you need repeatly use a same calculation reslut, the best way to attain
# result from cache, not calculating it again as it is time-comsuming and 
# need more extra memory. 

## Here I am going to compute the inverse of an invertible matrix. What method I 
## use is to check Cache first when you start to compute, if you find the computation
## have done before, you just get that value, otherwise you should calcluate.


## The first function, makeCacheMaster creates a special "Matrix", which is really a list containing a function to
set the value of the Matrix
get the value of the Matrix
set the value of the inverse
get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
m <- NULL                # m to store the inverse of matrix
        set <- function(y) {
                x <<- y    #assign a value to an object in an environment that is different from the current environment.
                m <<- NULL
        }
        get <- function() x                       # get the matrix of x
        setInverse <- function(solve) m <<- solve  # set the inversed matrix
        getInverse <- function() m                 #  get the inversed matrix
        list(set = set, get = get,                 # return a list containing four elements of functions
             setInverse = setInverse,
             getInverse = getInverse)

}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse 
## in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()  # get the inverse of the matrix, at this moment m either is NULL or a real matrix
        if(!is.null(m)) {    
                message("getting cached data")  # check if inverse has already been calculated, 
                return(m)                       # if yes, get inverse value
                
        }
        data <- x$get()                         # if no, get the specific matrix
        m <-solve(data, ...)                    # calculate the inverse of this matrix
        x$setInverse(m)                         # set the calculated inverse of matrix
        m                                       # get the inverse value

}
