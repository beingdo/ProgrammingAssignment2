## For this Assignment, I adopted two approaches.

## First, I adapted the examples reported in the guidelines
## of the Assignment 2 (simply, I adapted the scripts of the
## examples to retrieve the matrix inverse). 

## In the second approach, I developed  a completely new script
## to execute the required computations.

## If you do not want to check the entire script, please,
## check just the 'First Approach'

## ---First approach---

# Part 1 - This function sets the 'special matrix (sm)'

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set.sm<-function(y){
  x<<-y
  m<<-NULL
}
get.sm<-function() x
set.smmatrix<-function(solve) m<<- solve
get.smmatrix<-function() m
list(set.sm=set.sm, get.sm=get.sm,
   set.smmatrix=set.smmatrix,
   get.smmatrix=get.smmatrix)
}


# Part 2 - This function retrive from cache the 'special matrix' inverse

cacheSolve <- function(x=matrix(), ...) {
    m<-x$get.smmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get.sm()
    m<-solve(matrix, ...)
    x$set.smmatrix(m)
    m
}

## ---End of First approach---

# How to verify if First Approach is correct:
# Step 1 - Define variable 'a': a<-makeCacheMatrix()
# Step 2 - Set 'special matrix': a$set.sm(matrix(data,nrow,ncol))
# Step 3- Retrive inverse from cache: cacheSolve(a)


## ---Second approach---

# Part 1 - This function sets the 'special matrix (sm)'

makeCacheMatrix2 <- function(data,nrow,ncol){

#defining the Special Matrix and cache it

	sm<<-matrix(data,nrow,ncol)

#Special Matrix inverse and cache it

	INV_sm<<-solve(sm)

#Show/Print Special Matrix
	sm
}


# Part 2 - This function retrive from cache the 'special matrix' inverse
# Note: in this part, if the inverse does not exist, the script calculate INV_sm

cacheSolve2<-function(z,...) {

#recover Special Matrix Inverse

	m2<-INV_sm

#test if informed matrix is equal to special matrix, and if INV_sm is not null

	if (is.matrix(z) && is.matrix(sm) && dim(z) == dim(sm) && all(z == sm) && (!is.null(m2))) {
		message("getting cached data")
		return(m2)
	} else {
		message("calculating inverse - not in cache")
		m2<-solve(z)
	}
	m2
}


## ---End of Second approach---

# How to verify if Second Approach is correct:
# Step 1 - Set Special Matrix: b<-makeCacheMatrix2(data,nrow,ncol)
# Step 2- Retrive the Special Matrix from cache: cacheSolve2(b)