## Put comments here that give an overall description of what your
## functions do

# Overall, we have two functions: 
# (1) makeCacheMatrix: this one create special matrix object that can cache its inverse
# (2) this one compute the inverse of special matrix returned by the first function
# (3) This is extra part for checking that the functions work 

########################### 
# Part 1: makeCacheMatrix #
###########################

## Write a short comment describing this function
# This is the makeCacheMatrix function that creates the special matrix

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function(){x}
        setInverse <- function(inverse){inv <<- inverse}
        getInverse <- function(){inv}
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

###################### 
# Part 2: cacheSolve #
######################

## Write a short comment describing this function
# This is the cacheSolve function that return inverse matrix of the makeCacheMatrix's result

cacheSolve <- function(x, ...){
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat,...)
        x$setInverse(inv)
        inv
}

################### 
# Part 3: Testing #
###################

# Create a matrix by makeCacheMatrix function
pmatrix <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
pmatrix$get()
pmatrix$getInverse()
# Inverse of matrix by cacheSolve function 
cacheSolve(pmatrix)
pmatrix$getInverse()
