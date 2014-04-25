## -----------------------------------------------------------------
## 1. Function 'makeCacheMatrix'
## =============================
## This function creates a special matrix object that can cache 
## its inverse.
## 
## It takes an argument 'mat' of matrix type SQUARE Matrix
## It returns a list with four list items which are actually four
## functions wrapped in a list. The returned functions are 
##	set - set (cache) the matrix 
##	get - return the matrix
##	setinverse - set the value of inverse of the matrix
##	getinverse - returns the inverse of matrix
## 
## No validity check is performed for a square matrix, hence it is 
## assumed that user will pass a square matrix as an argument to  
## this function.
##
##
## 2. Function 'cacheSolve'
## ========================
## This function computes the inverse of the square matrix 
## which is part of the matrix object returned by 'makeCacheMatrix'
## function.  
##
## If the inverse has already been calculated (and the matrix has 
## not changed), then the cacheSolve should retrieve the inverse 
## from the cache, other wise it will inverse the square matrix 
## using the native 'solve' function of R
##
## No validity check is performed for a square matrix, hence it is 
## assumed that user has passed square matrix as an argument to  
## the 'makeCacheMatrix' function.
##
##
## Test R Commands:
## ================
## source("cachematrix.R")
## matObj <- makeCacheMatrix(matrix(nrow=2, ncol=2, c(2,3,4,5)))
##    OR
## matObj <- makeCacheMatrix(matrix(rnorm(25), ncol=5, nrow=5))
##
## mat1 <- matObj$get()
## mat1
##
## mat1_inv <- cacheSolve(matObj)
## mat1_inv
## idnt_mat <- mat1 %*% mat1_inv
## idnt_mat
## 
## Note: idnt_mat should be an Identity Matrix
##-----------------------------------------------------------------

##---------------------------------------------------------------
## This function creates a special matrix object that can cache 
## its inverse.
##---------------------------------------------------------------
makeCacheMatrix <- function(mat = matrix()) {

       matInv <- NULL

       # set the value of 'mat' matrix, and 
       # initialize 'matInv' (inverse of matrix 'mat') as null
       set <- function(y) {
                mat <<- y
                matInv <<- NULL
        }

        # returns matrix 'mat'
        get <- function() mat

        # set the value of 'matInv' (inverse of matrix)
        setinverse <- function(solve) matInv <<- solve

        # returns the matInv (inverse of matrix)
        getinverse <- function() matInv

        # return a list of four functions wrapped as four list items
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##------------------------------------------------------------------
## This function computes the inverse of the square matrix 
## which is part of the matrix object returned by 'makeCacheMatrix'
## function. 
##------------------------------------------------------------------
cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'x'

        # retrieve inverse matrix if it is found in cache, 
        # and then return it
        matInv <- mat$getinverse()
        if(!is.null(matInv)) {
                message("getting cached inverse matrix")
                return(matInv)
        }
        #
        # If matrix inverse is not found in cache, perform 
        # inverse of the matrix assuming its a square matrix
        # 
        # get the matrix 'data' from the 'mat' object
        data <- mat$get()

        # perform inverse of the matrix 'data'
        matInv <- solve(data, ...)

        # cache the inverse matrix 'matInv' in 'mat' object
        # for future use, by using setinverse function
        mat$setinverse(matInv)

        # return the inverse matrix
        matInv
}
