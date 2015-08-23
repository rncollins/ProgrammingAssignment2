##Creates special matrix that can hold its inverse

makeCacheMatrix <- function(x = matrix()) {
  cachedInv <- NULL
  set <- function(uservalue = matrix()) {
    x <<- uservalue
    cachedInv <<- NULL
  }
  get <- function() x
  setInverse <- function(invVal) {
    cachedInv <<- invVal
    return(cachedInv)
  }
  getInverse <- function() cachedInv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

##Creates special matrix that is an inverse of the special matrix calculated in the first function

cachesolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...){
  calculatedInverse <- x$getInverse()
  if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)) {
    message("Cached data available! Efficiency!")
    return(calculatedInverse)
  }
  matrixToSolve <- x$get()
  calculatedInverse <- tryCatch({
      solve(matrixToSolve)
  }, warning=function(w) {
    message("Something may have went wrong")
    message(w)
  }, warning = function(e) {
    message("Something definitely went wrong!")
    message(e)
    message("\n")
  })
  message("setting the value of inverse to:")
  x$setInverse(calculatedInverse)
}
