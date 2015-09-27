## Put comments here that give an overall description of what your
## functions do

## This function needs as argument an invertible matrix and returns a list of functions
##to call this function do something like my_inv<-makeCacheMatrix(m)
##where m is a matrix that can be inverted
##functions included in this function are described below

makeCacheMatrix <- function(x = matrix()) {
        ##start inv with null value
        inv <- NULL
        ##set a new matrix (just if required) 
        set<-function(y){
                x <<- y
                inv <<- NULL
        }
        ##get the current value of the matrix
        get <- function() x
        ##set the inverse of the matrix if known by user
        setinv <- function(inver) inv <<- inver
        ##get the inverse (current set value) of the matrix
        getinv <- function() inv 
        ##return list with the function described above
        list(set = set, get = get, setinv = setinv, getinv = getinv) 
}

## this function solves the inverse of the matrix
## to call this function do something like cacheSolve(my_inv)
##where my_inv was set with the previous function makeCacheMatrix

cacheSolve <- function(x, ...) {
        ##looks for the current value of the inverted matrix
        inv <- x$getinv()
        ##if exist just returns it
        if(!is.null(inv)){
                message("getting the cached inverse of the matrix")
                return(inv)
        }
        ##if does not exist assigns the value of the matrix to data
        ##calculates the inverse with solve(), sets the inverse, and returns the inverse
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
