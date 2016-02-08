## makeCacheMatrix makes a list that contains the original matrix and the inverse
##cacheSolve compute inverse of the matrix but first check if it is already cached

#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  n <- NULL #initiate variable n(local) as NULL
  set <- function(y) { #set value of matrix in set
    x <<- y #store y into x(global)
    n <<- NULL #store n(gloval)
  }
  get <- function() x #get value of x(global), no x(local) defined yet
  setinverse <- function(solve) n <<- solve #asign the inverse matrix into n(global)
  getinverse <- function() n #get inverse of vector
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) #make a list of 4 elements
}
##This function computes the inverse special "matrix". if computed before, get the cache first
cacheSolve <- function(x, ...) {
  n <- x$getinverse() #set getinverse in variable n(local)
  if(!is.null(n)) { #if n(local) is not NULL, then get the cache
    message("getting cached data")
    return(n) #returns n and finish the function here
  }
  data <- x$get() #get the orivinal numeric vector and assign to data
  n <- solve(data, ...) #
  x$setinverse(n)
  n
  ## Return a matrix that is the inverse of 'x'
}

##Test the functions with these
# mtrx <- matrix(c(1, 0, 5, 2, 1, 6, 3, 5, 0), nrow = 3)
# amatrix <- makeCacheMatrix(mtrx)
# amatrix$get()
# amatrix$getinverse()
# cacheSolve(amatrix)
# amatrix$getinverse()
# cacheSolve(amatrix)