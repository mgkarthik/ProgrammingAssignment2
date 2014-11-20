## The first function sets and gets the values of a matrix and its inverse.
## The second function checks if the inverse of the matrix from function 1 has been computed,
## if it has, then the second function retrieves this inverse, else, it computes the inverse and returns it.

## This function sets and gets the values of a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y)
  {x<<-y
  i<<-NULL}
  get<-function() x
  setinv<-function(inv) i<<-inv
  getinv<-function() i
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function checks if the inverse of the matrix has already been computed. If it has, then it retrieves it from the cache, else it computes the inverse and returns it.

cacheSolve <- function(x, ...) {
i<-x$getinv()
if(!is.null(i))
{message("Getting cached data")
return(i)}
else
{data<-x$get()
i<-solve(data,...)
x$setinv(i)
i}}
