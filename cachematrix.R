## Put comments here that give an overall description of what your
## functions do


# MAKECACHEMATRIX FUNCTION
# The makeCacheMatrix function creates a matrix object that can cache the inverse of a matrix.
# The function takes a matrix as argument input.
# The inverse is a matrix of the same dimension of the entered matrix and is initialized 
# with NAs values.
# Others functions defined in the main function allows the user to set or get the 
# value of the matrix and to set and get the inverse of the matrix.

# CACHESOLVE FUNCTION
# The cacheSolve function return the inverse of the matrix object created with the makeCacheMatrix
# function.
# If the inverse has already been calculated, the function doesn't recalculate it and the
# stored value is just returned, otherwise the inverse matrix will be computed using the solve() 
# function.


## Write a short comment describing this function


############### makeCacheMatrix ####################
# The makeCacheMatrix function:
# 1. set the value of a matrix
# 2. get the value of a matrix
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
    s=matrix(NA,ncol = ncol(x),nrow = nrow(x))
    set=function(y){
        x<<-y
        s<<-matrix(NA,ncol = ncol(x),nrow = nrow(x))
    }
    get=function()x
    setsolve=function(solve)s <<- solve
    getsolve=function()s
    list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


## Write a short comment describing this function

################### cacheSolve #####################
# The cacheSolve function:
# 1.takes a matrix object as argument
# 2.returns the inverse of the entered matrix

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    i=x$getsolve()
    if(!is.na(i[1,1])){
        message("Getting inverse of matrix from cached data")
        return(i)
    }
    else {
        matr=x$get()
        i=solve(matr)
        x$setsolve(i)
        i
    }
}