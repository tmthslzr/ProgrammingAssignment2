## Put comments here that give an overall description of what your
## functions do

## this function takes a matrix, and can cache the inverse so that it only needs to be computed once

makeCacheMatrix <- function(x = matrix()) {
        #set function 
        set <- function(y){
                x<<-y
                cache<<-NULL
                
        }
        #get function to get matrix
        get <- function()x
        #setcache function
        setcache<- function(z) cache <<- z
        #getcache funtion to retrieve cached value
        getcache <- function() cache
        #function returns a list of above functions
        list(set = set, get = get, setcache = setcache, getcache = getcache)
        
}


## this function computes the inverse of a matrix, but first checks to see if the inverse has already been cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #check if there is a cached inverse, in which case return it
        if(!is.null(x$getcache())){
                message("Inverse matrix already cached. Retrieving...")
                return(x$getcache())
        }
        #if no cached inverse, 'get' matrix and compute the inverse
        inver<- solve(x$get(),...)
        #cache computed inverse
        x$setcache(inver)
        #and return the newly computed result
        inver
}


