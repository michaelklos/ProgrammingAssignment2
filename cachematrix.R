## R Programming, Programming Assignment 2 - Lexical Scoping
##
## The following two functions are designed to demonstrate lexical scoping 
## in R - that is, the ability to change variables beyond the scope of the 
## originating function. 
##
## Many thanks to the TAs (looking at you, Scott) and my classmates on this 
## thread for helping me to understand what was going on with this assignment. 
## https://class.coursera.org/rprog-011/forum/thread?thread_id=105


## Input: x (type: matrix, default: empty matrix)
## Return: list object of function "wrappers" of the x vector
##         to enable get/set functionality for core value
##         and inverse value

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv, getinv = getinv)
}


## Input: x (type: list, generated from makeCacheMatrix())
## Return: matrix object containing the *inverse* of the value contained within 
##         x$get(). If this value is new, it is calculated and set via
##         x$setinv(). If this value previously existed, it is extracted from 
##         x$getinv() and returned. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)){
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
