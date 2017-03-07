## cachematrix.R contains two functions; makeCacheMatrix and cacheSolve.
## This is the submission for Programming Assignment 2 for Week 3 R Programming
##
## Created by Daniel Jones 
## Date: 07/03/2017
## Tested using test cases found here: 
## https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/ePlO1eMdEeahzg7_4P4Vvg
##
## Completion of the assignment was helped by reading the following post:
## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md

## makeCacheMatrix is a function that creates a special matrix object which is 
## a list containing functions to: 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 2. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(
                set = set, #gives name of set to set() function above
                get = get, #gives name of get to get() function above
                setsolve = setsolve, #gives name of setsolve to setsolve() function above
                getsolve = getsolve #gives name of getsolve to getsolve() function above
        )
}


## cacheSolve is a function that calculcates the inverse of the matrix using the
## functions created in makeCacheMatrix. It returns the inverse of the special
## matrix created in makeCacheMatrix. However, it first checks to see if the 
##inverse has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates 
#the inverse and sets the value of the inverse in the cache 
#via the setsolve function.

cacheSolve <- function(x, ...) {
        
                m <- x$getsolve()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setsolve(m)
                m
}
