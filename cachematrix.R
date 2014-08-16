## The following functions compute and cache the inverse 
## of a square matrix. 
     

## Create functions to be utilized in caching
## the inverse of a square matrix

makeCacheMatrix <- function(x = matrix()) {
          minv<- NULL                ## initialize inverse matrix
          set<-function(y) {         ## cache initial values
               x<<-y
               minv<<-NULL
          }
          get<-function() x                            ## to get matrix
          setinverse<-function(solve) minv <<- solve   ## to solve for inverse and cache
          getinverse<-function() minv                  ## to get inverted matrix
          list(set=set, get=get,
               setinverse=setinverse,
               getinverse=getinverse)
}


## Compute the inverse of a square function if it has not 
## already been computed and cached. Return inverse

cacheSolve <- function(x, ...) {
     minv<-x$getinverse()          ## get current value of inverse matrix
     if(!is.null(minv)) {          ## is it populated with a cached matrix?
          message("getting cached data")  
          return(minv)             ## if yes, return inverse matrix with message
     }
     data<-x$get()                 ## get matrix to invert
     minv<-solve(data)             ## compute inverse
     x$setinverse(minv)            ##   and cache
     minv                          ## return cached inverse matrix
}
