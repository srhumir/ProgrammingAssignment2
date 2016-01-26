## The fillowing to functions save a matrix then cach the inverse of that 
## matrix the first time the in inverse is caclulated. From then if the 
## user try to compute the inverse again the cached inverse will be 
##returned without computation.

#Create a list that cach the matrix and its inverse
makeCacheMatrix <- function(A, ...){
       I <- NULL
       set <- function(y){
              A <<- y
              I <<- NULL
       }
       get <- function() A
       setinverse <- function(Inverse) I <<- Inverse
       getinverse <- function() I
       list(get = get, set = set, getinverse = getinverse, setinverse = setinverse)
}


#Get the output of abive function and compute the inverse of the matrix
#if necessary.
cacheSolve <- function(a, ...){
       i <- a$getinverse()
       if (!is.null(i)){
              print("Cached inverse in returned")
              return(i)
       }
       data <- a$get()
       i <- solve(data)
       a$setinverse(i)
       i
}