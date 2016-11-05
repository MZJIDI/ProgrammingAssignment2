## overall description: it cashes the inverse of an invertible matrix by a pair of functions
## by a pair of functions: makeCacheMatrix() and cashSolve() 

##-----------------------------------------------------------------------------------------------------
## makeCacheMatrix creates 4 functions and 
## then returns the functions by a list to the parent environment


makeCacheMatrix <- function(x = matrix()) {
        
        n <- NULL  ## nitializing it as an object within the makeVector() environment 
        ## It will be used by code comming later in the function   
        
        set<- function (z){ 
                
                x <<- z ## Assign the input argument (Z) to the x object in the parent environment
                n <<- NULL ## Assign NULL value to the n object in the parent environment
        }
        
        get <- function() x ## Since the symbol x is not defined within get(), 
        ## R retrieves it from the parent environment of makeCacheMatrix
        
        
        setinverse <- function(solve) n <<- solve ## setter for the inverse n
        ## Since n is defined in the parent environment         
        ## and need to be accessed after setinverse() completes,         
        ## use <<- to assign the input argument to the value of n in the parent environment
        
        getinverse <- function() n   ## getter for the inverse n
        
        
        
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse) ## Create a new object by returning a list()
        
        ## assigns each of the functions as an element within a list()
        ## and returns it to the parent environment
        ## each element in the list is named,allows to use the $ (extract operator) to access the functions by name 
        ## rather than using the [[ to get the contents of the matrix.
        
}

## -----------------------------------------------------------------------------------------------------------
## computes the inverse of the special "matrix" returned by makeCacheMatrix



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        n <- x$getinverse()
        
        if(!is.null(n)) {
                ## If the inverse has already been calculated (and the matrix has not changed), 
                message("getting cached data")
                ## then the cachesolve should retrieve the inverse from the cache.
                return(n)
        }
        ## If R was a more strongly typed language, the function stub might look like:
        ## cacheSolve <- function(makeCacheMatrix x, ...) {
        # return the inverse of x, or calculate & return if cache is empty
        ## }
        ## This type of specification would make it obvious
        ## that cacheSolve() requires as its input the type of object that is output by makeCacheMatrix().       
        
        
        data  <- x$get()
        n <- solve(data, ...)
        x$setinverse(n)
        n
}       


## =========================================================================================================================
## First, the value of x is set as a function argument, as in makeCacheMatrix().
## Then, the first line of code in the function sets m <- NULL, simultaneously allocating memory for m and setting it to NULL.
## When a reference to this object is passed to the parent environment when the function ends, 
## both x and m are available to be accessed by their respective get and set functions.
