## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
  inverse <- NULL 
  set <- function(new) { #seting the inverse and matrix x
    x <<- new #super assign a value to an object in this subfunction to x object and making it 
    # available in global environment
    inverse <<- NULL #resetting the variable to zero 
    }
  get <- function()x
    setinverse<- function(new_inverse) {
    inverse <<-new_inverse #Pattern of set function- superassigning newinv matrix to original obj inv
  }
  getinverse <- function() inverse 
     print (list (set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse))
         #return in a list format
        #while tesing the functions against each other
  
}


cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  invm <- x$getinverse() 
  if (is.null(invm)) {  
    message("retrieving cached data")
    print (invm)
    
  }
  b<-x$get()
  invm<-solve(b,...)
  x$setinverse(invm)
  return (invm)
}


#Testing

test_m <- makeCacheMatrix(matrix(1:4, 2, 2))
test_m$get()
test_m$getinverse()
cacheSolve(test_m)
test_m$getinverse()
