## These functions cache the inverse of a given matrix.

## The first one creates a special "matrix" object that can cache its inverse.

makeCacheMatrix<-function(x=matrix()) {
    i<-NULL    # initializing the inverse property
    set<-function(y) {    # variant to set the matrix
            x<<-y
            i<<-NULL
    }
    get<-function() {x}    # variant to get the matrix
    setinverse<-function(inverse) {i<<-inverse}    # variant to set the inverse
    getinverse<-function() {i}    # variant to get the inverse
    list(set = set, get = get,    # returning a list of the variants
         setinverse = setinverse,
         getinverse = getinverse)
}

## The second one computes the inverse of the special matrix returned by the 
## first function. If the inverse has already been computed the cacheSolve 
## will retrieve the inverse from the cache.

cacheSolve<-function(x,...) {
    i<-x$getinverse()    # returning matrix that is the inverse of 'x'
    if(!is.null(i)) {    # returning the inverse if it's already set
            message("getting cached data")
            return(i)
    }
    data<-x$get()    # getting matrix
    i<-solve(data,...)    # computing the inverse
    x$setinverse(i)    # setting the inverse to the object
    i    # returning the matrix
}
