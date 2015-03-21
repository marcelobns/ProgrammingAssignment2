## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(matrix = matrix()) {    
    
    ## Private variable inverse
    inverse <- NULL
    
    ## Encapsulation of matrix and inverse values
    set <- function(value) {
        matrix <<- value
        inverse <<- NULL
    }
    get <- function(){
        return(matrix)
    }     
    set_inv <- function(solved){
        inverse <<- solved  
    } 
    get_inv <- function(){
        return(inverse)
    }
    
    ## Returning of encapsulated methods
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(input, ...) {
    ## get inverse matrix
    inverse <- input$get_inv()
    
    ## if inverse is not null and return inverse
    if(!is.null(inverse)) {
        message("cached")
        return(inverse)
    }
    
    ## get matrix and solve inverse by native function of R
    matrix <- input$get()    
    inverse <- solve(matrix)    
    input$set_inv(inverse)
    
    message("no cached")
    return(inverse)
}

## test functions
testMatrix <- function(matrix = cbind(c(3,1),c(2,1))){    
    m = makeCacheMatrix(matrix)    
    cacheSolve(m)    
    cacheSolve(m)
    
    m$get_inv()
}