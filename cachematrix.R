## Functions allow save results inverse matrices and load them from cache, instead 
## of recomputing them again each time

## The function is the framework to make a cachematrix. The input is x: given as
## a square, invertable matrix. The function will return a list containing muli-
## ple functions that set & get both the matrix as well as the inverse. 

makeCacheMatrix <- function(x = matrix () ) {
        inv <- NULL
        set <- function(y) {
                
                #use ' <<- ' to assign value to object in different environment
                # ( as in, different from current environment)
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        
        list( set = set, 
              get = get,
              setinv = setinv,
              getinv = getinv
             )
}


## Returns the inverse of the matrix x if the inverse has already been 
## calculated it caches the matrix

cacheSolve <- function(x, ...) {

        #get inverse of original matrix x
        inv <- x$getinv()
        
        #if statement to check if the inverse has already been calculated
        if ( !is.null ( inv )){
                message("getting cached data")
                return(inv)
        }
        
        #if cannot yet be cached, calculate
        matrix_data <- x$get()
        inv <- solve(matrix_data)
        x$setinv(inv)
        
        return (inv)
}
