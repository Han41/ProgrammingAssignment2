## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## get the value of matrix and inverse(if existed), for cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL    # initialize
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function()x    # get the value of matrix
        setInv<-function(invCalculation)inv<<-invCalculation    # set the value of inverse
        getInv<-function()inv    # get the value of inverse
        list(set=set, get=get, setInv=setInv, getInv=getInv)    

}


## Write a short comment describing this function
## Determine if the matrix inverse has been calculated. 
## If so,extract the cached value directly.
## If not,calculate the inverse of the matrix and set the value of inverse for next time to get

cacheSolve <- function(x, ...) {
        inv<-x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
