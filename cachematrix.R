#------------------------------------------------------------------------------
#File 		: cacheMatrix.R
#Author 	: Wynnie
#Course 	: R programming assignment 2
#Description:
#This file contains the following two functions
#makeCacheMatrix : Creates a special matrix object that caches its inverse
#cacheSolve      : Returns the inverse of a special matrix object. Returns 
#				   the cached value if the inverse has already been solved 
#				   and cached
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
#Function makeCacheMatrix
#This creates a special matrix object that can cache its inverse.
#Returns a list of functions to
#  - Set the value of the matrix
#  - Get the value of the matrix
#  - Set the value of the inverse
#  - Get the value of the inverse
#------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL

	#Set the matrix object
	setmatrix <- function(y) {
		x <<- y  #Save in parent environment to be persistent
		inverse <<- NULL #Save in parent environment to be persistent
	}
	#Get the matrix object
	getmatrix <- function() {
		x
	}
	#Set the inverse matrix
	setinverse <- function(inv) {
		inverse <<- inv
	}
	#Get the inverse matrix
	getinverse <- function() {
		inverse
	}
	#Output the list of functions
	list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinverse = setinverse, getinverse = getinverse)

}

#------------------------------------------------------------------------------
#Function cacheSolve
#Returns the inverse of a special matrix object. Uses the cached value if the 
#inverse has already been calculated.
#------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()  #Get the inverse matrix
        if (!is.null(inv)) { #Inverse has been calculated previously if it is not null
        	return(inv)  #Returning cached value
        }
        #If inverse has not been calculated, function falls through to the following
        mat <- x$getmatrix()   	#Get the matrix
        inv <- solve(mat)  		#Find inverse using solve
        x$setinverse(inv)  		#Cache the inverse in the special matrix object
        inv   					#Return the caluclated inverse
}
