 
# Part 1: makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. 
 
 
 makeCacheMatrix <- function(x = matrix()) { 
         # sets Inverse equal to an empty matrix 
         Inverse <- NULL 
         # Set the inverse matrix equal to NULL 
          
         set <- function(y){ 
                 x <<- y 
                 # set function assigns the argument to x 
                 Inverse <<- NULL 
                 # Once the set function is called, Inverse is re-set to NULL (this is important if you redefine the matrix, x) 
         } 
         get <- function() x 
         # get function returns the matrix 
          
         setInverse <- function(solve) Inverse <<- solve 
         # setInverse overrides the previous value of Inverse and assigns the argument to Inverse (which is supposed to be the inverse of matrix x) 
          
         getInverse <- function() Inverse 
         # getInverse returns the Inverse matrix
          
         list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
         # creates a list of the functions 
          
 } 
 
 
# Part 2: cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# However, it first checks to see if the inverse has already been calculated (and the matrix has not changed).
# If so, it  gets the Inverse from the cache and skips the computation. Otherwise, it calculates the Inverse of the matrix and sets the value of the matrix in the cache via the setInverse function. 
 
 cacheSolve <- function(x, ...) { 
         Inverse <- x$getInverse() 
         # Retrives the most recent value for the inverse 
          
         if(!is.null(Inverse)){ 
                 message("getting cached data") 
                 return(Inverse) 
                 # If the value of Inverse is NOT null (was previously calculated), cacheSolve returns that value         
         } 
         # If the value of Inverse is NULL, then you retrive matrix x and calculate the inverse with the solve() function 
         message("calculating inverse matrix")
         data <- x$get() 
         Inverse <- solve(data, ...) 
         x$setInverse(Inverse) 
         # Sets Inverse to the newly calculated value    
         Inverse #Returns the new Inverse value 
 } 

