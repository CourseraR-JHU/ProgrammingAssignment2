
##Functions to compute and Cache the Inverse of a Matrix



 
##This function takes a matrix as an argument and creates 
##a list containing a function to set and get the matrix and
## to set and get the inverse of the matrix. 
 
makeCacheMatrix <- function(x = matrix()) {
             s  <- NULL
            set <- function(y){
                   x <<- y
                   s <<- NULL
            }
            get <- function()x
            setInverseMatrix <- function(InverseMatrix) s <<- InverseMatrix
            getInverseMatrix <- function() s
            list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}



##This function first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. 
##If not in cache, the inverse is calculated and sets the value of inverse in cache via setInverseMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       s <- x$getMatrix()
       if(!is.null(s)){
            message("getting cached data")
            return(s)
       }
       data <- x$get()
       s <- solve(data) %*% data
       x$setInverseMatrix(s)
       s
}
