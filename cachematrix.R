# Computing the inverse of a matrix can be a computationally demanding task (it takes a long time).
# The function below serves to amerliorate this problem by storing the inverse of a matrix rather than recomputing the inverse of - 
# the matrix everytime.


# creates a list with four functions
makecachematrix <- function(x = matrix()) {
        m <- NULL # set m to null withinin function
        # make set function which stores x and m 
        set <- function(y) {
                x <<- y # sets the arguemnt in this function (y) ot x
                m <<- NULL
        }
        get <- function() x # functions that returns x 
        setinverse <- function(invert) m <<- invert # inverts arguement passed to m
        getinverse <- function() m
        list(set = set, get = get, # list of four functions
             setinverse = setinverse,
             getinverse = getinverse)
}




## Actually does the inverting bit. If the matrix already has been inverted then returns - 
## the message.


cachematrix <- function(x,...) {
        m <- x$getinverse() # 
        if(!is.null(m)) { # see above regarding message
                message("getting cached data")
                return(m)
        }
        data <- x$get() # m = null then get the function get from the list and set to data
        m <- solve(data, ...) # and set to m 
        x$setinverse(m) #set new m
        return(m) # show the values within m
}
