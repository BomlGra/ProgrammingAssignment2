
## This function  crates cacheable matrix 


##  * get() - to get internal R matrix
##  * set(setedInversion) - to set internal R matrix and reset earlier calculated inverse
##  * getInversion() - to get cached inverse, or NULL if no cached inverse is present
##  * setInversion(invertedMatrix) - to set cached inverse

makeCacheMatrix <- function(baceMatrix = matrix()) {
 
  invertedMatrix <- NULL
    set <- function(setedInversion) {
      baceMatrix <<- setedInversion
      invertedMatrix <<- NULL
    }
    
    get <- function() baceMatrix
    setInversion <- function(inversion) invertedMatrix <<- inversion
    getInversion <- function() invertedMatrix
    list(set = set, get = get,
         setInversion = setInversion,
         getInversion = getInversion)
  

}


## With operation solve(X) we can return inverse matrix 
## Function checked if a value was calculated earlier, then caching data and return the matrix or 
## just return inverted matrix depends on result of this check

cacheSolve <- function(baceMatrix, ...) {

  invertedMatrix <- baceMatrix$getInversion()
    if(!is.null(invertedMatrix)) {
      message("getting cached data")
      return(invertedMatrix)
    }
    data <- baceMatrix$get()
    invertedMatrix <- solve(data, ...)
    baceMatrix$setInversion(invertedMatrix)
    invertedMatrix
  
        ## Return a matrix that is the inverse of 'baceMatrix'
}
