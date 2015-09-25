## Put comments here that give an overall description of what your
## functions do

## This code is written as part of the Coursera Programming assignment-2. 
## Requirement for the assignment is to cache the inverse of the matrix, by writing
## 2 functions. First one "makeCacheMatrix will have getter, setter methods to 
## set the get the required matrices. 
##
## Second function "cacheSolve" will get the inverse of the matrix and cache the same.
## First time inverese is calculated and cached. From the next time, cached data will be
## fetched whenever inverse for the same matrix is expected
##
##

## Write a short comment describing this function
## This function will have get, set methods to set and get the matrix values and its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    # IMCache will hold the inverse matrix values. Setting the valye of this to NULL whenever
    # new Matrix values are provided. Note that whenever new matrix is given as input makeCacheMatrix
    # function will be called
    IMcache <- NULL
    
    setMatrix <- function(y = matrix()){
        x <<- y
        
        ## when the setMatrix method is called, reset the cache value to NULL
        IMcache <<- NULL
    }
    
    # get the matrix value which is set
    getMatrix <- function() x
    
    # set the inverse matrix value
    setInvMatrix <- function(invMatrix) IMcache <<- invMatrix
    
    # get the inverse matrix value
    getInvMatrix <- function() IMcache
    
    # construct the list of all the set and get methods
    list(setMatrix = setMatrix,getMatrix = getMatrix, setInvMatrix= setInvMatrix, getInvMatrix=getInvMatrix  )
}



## This function is used to get the inverse of the matrix and set the cache value by calling getter and setter methods of 
## "makeCacheMatrix" function.

cacheSolve <- function(x, ...) {

    ## Call the makeCacheMatrix functions getInvMatrix to get the cached value
    IMcache <- x$getInvMatrix()
    
    if(!is.null(IMcache)){
        message("Getting the cached data")
        ## Return statement will exit the cacheSolve function with the cached IMcache value without executing the program further
        return(IMcache)
        
    }
    
    ## get the value of matrix by calling getter method
    m <- x$getMatrix()
    
    ## get the inverse of the matrix by calling solve function and store it in IMcache matrix value
    IMcache <- solve(m, ...)
    
    ## set the inverse of the matrix to cache using setter method
    x$setInvMatrix(IMcache)

    ## Return a matrix that is the inverse of 'x'    
    IMcache
    
}
