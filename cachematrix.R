## This functions are designed to create a inverse matrix of a matrix. The inverse will be cached 
## so it can be recalled later withoud new computing done. 

## The first function "makeCacheMatrix" will create a matrix that can cactch it's inverse matrix. 

makeCacheMatrix <-function(x = matrix()) { 
        m <- NULL
        set <- function(y){
          x <<- y
          m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) {m <<- solve(x)}            #this store the inverse in memory
      getinverse <- function() {m}
      list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## the second function will compute the inverse and store it so it can be recalled later. 

cacheSolve <- function(x) { 
          m <- x$getinverse()
          if(!is.null(m)){                      #when there an inverse stored the function will not read a NULL
          message("getting cached data")        #the function will then return the inverse stored
            return(m)                           
            }
            data <- x$get
            m <- solve(data)                    #this command will compute the inverse of the given matrix
            x$setsolve(m)
            m
}
        ## Return a matrix that is the inverse of 'x'
}
