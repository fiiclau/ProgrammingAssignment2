## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        mat_inverse <- NULL #Create variable to store inverse
        
        ##A function to assign value in cache
        set <- function(y){
                x <<- y
                mat_inverse <<- NULL
        }
        
        ##A function to return the cached value
        get <- function(){
                x
        }
        
        ##
        setinverse <- function(solve){
                mat_inverse <<- solve
        }
        
        getinverse <- function(){
                mat_inverse
        }
        
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ##Retrieve stored inverse and assign to mat_inverse
        mat_inverse <- x$getinverse()
        
        ##Check if there is cached inverse and return value if TRUE
        if(!is.null(mat_inverse)){
                message("Retrieving cached data...")
                return(mat_inverse)
        }
        
        retrieved_val <- x$get() #Get matrix if inverse not calculated
        mat_inverse <- solve(a=retrieved_val, ...) #Calculate inverse
        x$setinv(mat_inverse) #Store calculated inverse in cache
        mat_inverse #Return inverse
}
