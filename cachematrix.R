## Put comments here that give an overall description of what your
## functions do

##makeCachematrix and cacheSolve combine to set and get a matrix inverse that is
##saved in the cache if it has been computed before which will save time and not
##force the computer to recompute an inverse which is computationally intensive


## Write a short comment describing this function

##makeCacheMatrix creates a special matrix that is really a list containing
## a list of 1. Set the value of the matrix, 2. get the  value of the matrix, 
##3. set the value of the inverse, 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {   #creates special matrix
m <- NULL  ##erases any previous value
set<-function(y){   #creates new matrix
    x<<-y   #copies new matrix
    m<<-NULL   #erases old cached matrix
}
get<-function()x  #returns matrix
setinverse<-function(solve) m <<-solve  #stores incoming inverse
getinverse<-function() m  #returns inverse matrix
list(set = set, get = get, setinverse=setinverse,
     getinverse=getinverse)  #last value is what is returned by this function
}

## Write a short comment describing this function

##cacheSolve calculates the invers of the special matrix created by 
##makeCacheMatris. It checks to see if the inverse has already been computed 
##and uses the value in the cache if so. Otherwise it computes the inverse

cacheSolve <- function(x, ...) { 
        ## Return a matrix that is the inverse of 'x'
    m<- x$getinverse()
    if(!is.null(m)){   #if there is a matrix m
        message("getting cached data")   #print getting it from cache
        return(m)   #and returns the cached value
    }
    data <-x$get()   #if there is no matrix m then this part computes and prints one
    m <- solve(data, ...)
    x$setinverse(m)
    m    #last value of function so this is what is returned (inverse of matrix)
}
