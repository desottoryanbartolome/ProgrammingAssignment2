##This are the functions I have made to cache the inverse of a matrix

##As required, this first function creates a special matrix object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
  	        x<<-y
 	        m<<-NULL
	}
        get<-function() x
        setinv<-function(solve) m<<- solve
        getinv<-function() m
        list(set=set, get=get, setinv=setinv, getinv=getinv)
        }


#This next function computes for the inverse of the matrix (note that the values of the matrix should be set/defined first.
#should the matrix remain unchanged, the function will just recall the cached inverse.

cacheSolve <- function(x, ...) {
        m<-x$getinv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        orig<-x$get()
        m<-solve(orig, ...)
        x$setinv(m)
        m
        }
#start by placing the makeCacheMatrix function to a variable (e.g. mx)
#set the matrix (e.g. mx$set(matrix(c(1,3.24,12.52,1.122,2,2))
#to view the matrix use get() (e.g. mx$get())
#Get the inverse of the matrix using cacheSolve (e.g. cacheSolve(mx))
#repeating cacheSolve(mx) will yield to message stating that the matrix has not changed

#Another check done was to do mx$set(mx$getinv())
#when cacheSolve(mx) is executed, the new inverse matrix should result to the original matrix set.
