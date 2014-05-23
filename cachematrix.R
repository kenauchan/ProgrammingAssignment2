#20140520.these functions creates a matrix object into cache 
#         this cached matrix object has customized get, set, 
#          getinverse, and setinverse functions 
#         the data of the matrix is stored in cache 
#         the first request for an inverse of the matrix 
#          will inverse the matrix and store in cache
#          for subsequent retrieval
#
#20140520.create an object of a cached matrix with
#          methods get, set, getinverse, setinverse
makeCacheMatrix <- function(mMatrix = matrix()) {
  #20140520.class definition makeCacheMatrix
  #vFileId <- 'cachematrix.R'
  #vAuthor <- 'aubreyChan'
  #vDateStarted <- '20140520'
  #vDateLastUpdated <- '20140522'
  mInverse <- NULL
  set <- function(inMatrix){
    mMatrix <<- inMatrix
    mInverse <<- NULL
  }
  get <- function() mMatrix
  setinverse <- function(inInverse) mInverse <<- inInverse
  getinverse <- function() mInverse
  list(set=set, get=get, 
    setinverse=setinverse,
    getinverse=getinverse)
}

#20140520.return the inverse of the cached matrix object
cacheSolve <- function(aCacheMatrix, ...) {
  #an instance of makeCacheMatrix object
  #sample matrix vMatrix<-matrix(c(1,-0.25,-0.25,1),nrow=2,ncol=2,byrow=TRUE)
  vInverse <- aCacheMatrix$getinverse()
  #20140522.check if inverse is cached 
  if(!is.null(vInverse)){
    message('getting cached data')
    return(vInverse)
  }
  vData <- aCacheMatrix$get()
  vInverse <- solve(vData)
  aCacheMatrix$setinverse(vInverse)
  vInverse
}

