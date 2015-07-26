## makeCacheMatrix() accepts a matrix and returns a list of functions which can be used to 
## set and fetch the matrix and its inverse. The returned list of functions is then used by the cacheSolve() function to fetch
## the save value of inverse if cached already. Otherwise cacheSolve() chaces the inverse value to be used next time.


## makeCacheMatrix() takes a square matrix as input
## returns a list of four functions which can get, set the matrix or its inverse. 
## variable inv is used to hold the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inv<-NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        set_inverse <- function(inverse) inv<<-inverse
        
        get_inverse <- function() inv
        
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
        
}


## cacheSolve() accepts a list returned by the makeCacheMatrix()  function. Performs a simple check. If the value of inverse
## is already cached it returns that value otherwise calculates the inverse and also caches it for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        if (!is.null(x$get_inverse())){
                print('fetching the cached inverse....')
        }else{
                print('Caching the inverse for the first time....')
                x$set_inverse(solve(x$get()))
        }
        x$get_inverse()
}
