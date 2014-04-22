## Cache the invese of a Matrix
## The makeCacheMatrix creates a special type of data structure that can store 
## the inverse of the matrix passed. 
## The cacheSolve function utlizes the matrix created by makeCacheMatrix to actually store the inverse
## if it has not been done so yet.

## This function creates a data structure to store the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  #initialize the inverse of the matrix to null
  i <- NULL
  set<-function(y){
    #this function sets the value of y to x outside the scope by using << operator
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i<<-inverse
  getinverse <- function() i
  #return a list with all this
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## This function stores the inverse of the matrix if it is not present 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## assuming x is already created using makeCacheMatrix
        
        ## first check if the matrix inverse has been calculated already
        i<-x$getinverse()
        if(!is.null(i)){
          print("Returning the cached version of the inverse")
          return(i)
        }
        mat<-x$get()
        i<-solve(mat)
        #store the value of i
        x$setinverse(i)
        i
}

## Sample Output

# > m=rnorm(16)
# > dim(m)=c(4,4)
# > x=makeCacheMatrix(m)
# > cacheSolve(x)
# [,1]       [,2]      [,3]       [,4]
# [1,] -3.133854 -1.3590956 0.7184212 -1.8190570
# [2,] -1.044973 -0.3485191 0.7785478 -0.5137769
# [3,] -8.277366 -4.8142103 1.4345948 -3.1555297
# [4,] -5.977132 -2.3658888 1.0369784 -2.3303350
# > cacheSolve(x)
# [1] "Returning the cached version of the inverse"
# [,1]       [,2]      [,3]       [,4]
# [1,] -3.133854 -1.3590956 0.7184212 -1.8190570
# [2,] -1.044973 -0.3485191 0.7785478 -0.5137769
# [3,] -8.277366 -4.8142103 1.4345948 -3.1555297
# [4,] -5.977132 -2.3658888 1.0369784 -2.3303350
# > 
