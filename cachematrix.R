##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix<-function(x=matrix()){
        ## we create first an empty object that will be used later to store the matrix's inverse
        v <-NULL
        
        ## set the value of the matrix
        set<-function(y){
           x<<-y
           v<<-NULL
        }
        
        ## get the value of the matrix
        get<-function() x
        
        ## set the value of the inverse of the matrix
        setInverse<-function(invers) v<<-invers
        
        ## get the value of the inverse of the matrix
        getInverse<-function() v
        
        ## We now build the list of functions that will constitute our item
        list(set=set,get=get,
             setInverse=setInverse,
             getInverse=getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

CacheSolve<-function(x,...){
        ## At first we run the getInverse function to try to get the cached inverse if it already
        ## exists in its enrironment
        v<-x$getInverse()
        ## if it exists, we will print a message, and return the cached value
        if(!is.null(v)){
                message("getting cached inversed matrix")
                return(v)
        }
        ## if the inverse doesn't exits, then we will calculate the inverse. 
        ## we get first the matrix
        matrix<-x$get()
        ## then we calculate the inverse of the matrix
        v<-solve(matrix,...)
        ## we store the inverse in makeCacheMatrix's environment
        x$setInverse(v)
        ## The inverse is displayed
        v
}
