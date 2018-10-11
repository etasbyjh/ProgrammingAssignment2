## 1: make a function that creates a list of vectors has 4 functions: set,get,setInverse,getInverse
## 2: make a function that either cacluates matrix inversion or returns its cached inversed matrix

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
      i <- NULL #init null vector for inversed matrix
      set <- function(y){
            x <<- y #update x with the new input y
            i <<- NULL #reset i
      } 
      get <- function() x #get
      setInverse <- function(inverse) i <<- inverse #update i with the newly calculated one
      getInverse <- function() i #get i
      list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

## Write a short comment describing this function
cacheSolve <- function(mcm, ...) {
      i <- mcm$getInverse() #try to get a cache data from the input, mcm
      if(!is.null(i)){
            #if there is, return the cache data
            message("getting cache data")
            return(i)
      }
      else{
            #if there isn't, calculate and set the calculated one to the input, mcm
            im = solve(mcm$get())
            mcm$setInverse(im)
      }
      #then return
      mcm$getInverse()
}
