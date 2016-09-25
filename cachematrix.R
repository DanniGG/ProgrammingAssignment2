##FUNCTION: makeCacheMatrix
##THIS FUNCTION IS DOING FOUR THINGS: 
##        1) Takes the input value (assuming matrix) and stores it.
##        2) Returns the stored matrix from step 1.
##        3) Sets the inverse from step 2. 
##        4) Returns the inverse from step 3.
# ESSENTIALLY THIS FUNCTION IS JUST A SET OF OTHER FUNCTIONS. IT DOES NOT RETURN ANYTHING OF VALUE.
makeCacheMatrix <- function(x = matrix()) {
        
        #Empty to start with. Otherwise holds the chached value.
        cached <- NULL
        
        #Storing the input
        setMatrix <- function(matrix) {
                x <<- matrix
                cached <<- NULL
        }
        
        #Returning the matrix stored above.
        getMatrix <- function() {
                x
        }
        
        #Caching the inverse of the stored matrix from above.
        setInverse <- function(solve) {
                cached <<- solve
        }
        
        
        getInverse <- function(){
                cached
        }
        #Returning the list the instruktions are asking for
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

##FUNCTION: cacheSolve
#THIS FUNCTION IS CALCULATING THE INVERSE MATRIX FROM THE INSTRUKTIONS AND TREATS IT USING THE
#FUNCTIONS DEFINED IN: makeCacheMatrix. DEPENDING ON THE SITUATION IT MIGHT BE RETURNING THE
#ALREADY CACHED INVERSE MATRIX AND IN THE OTHER CASE IT WILL CALCULATE THE INVERSE MATRIX.
cacheSolve <- function(y, ...) {
        #First we get the inverse that was cached in makeCacheMatrix.
        inverse <- y$getInverse()
        #Check to see if there is a cached matrix that can already be used.
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        #If the condition can't find cached data, we get the input and inverts it.
        originalMatrix <- y$getMatrix()
        
        #Using the solve function to invert the matrix.
        inverse <- solve(originalMatrix)
        
        #Then using the caching function to store the inverted matrix for future use.
        y$setInverse(inverse)
        
        #Returns the inversed matric
        inverse
}
