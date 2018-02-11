# Cache a matrix and its inverse if it is accessed. The constructor takes any matrix; it
# defaults to an empty matrix if none is specified.
#
# This function returns the folowing API: 
#    getMatrix - returns the current matrix
#    setMatrix - sets a matrix; clears the inverse cache since it needs recalculation
#    getInverse - get the inverse of the current matrix  
#    setInverse - sets the inverse of the matrix and returns the the inverse 
#    cacheHits - returns a named vector with the number of matrix hits served from  cache
#                and the number of inverse matrix hits served from cache.  Useful for
#                testing and measuring cache efficiency.  This extends beyond the actual
#                assignment, of course.  I was playing and did use it in my tests.
#
# Notes: I would not design an API like this.  It would be more efficient to simply have one 
# inverse management function as part of the makeCacheMatrix API (e.g., inverse() which could 
# act as both a getter and setter, calculating the inverse the first time and serving from cache
# subsequently) and lose that cacheSolve wrapper entirely.  However, that is not the assignment,
# so I did what was asked. 
makeCacheMatrix <- function(cachedMatrix = matrix()) {
    
    # Cache the specified matrix.  This will clear inverse matrix, & reset cache hit info
    # since we are starting over.  Return its value to allow chaining
    setMatrix <- function(aMatrix) {
        cachedMatrix <<- aMatrix
        inverseMatrix <<- NULL
        cache_hits <<- c("matrixHits" = 0, "inverseHits" = 0)
        cachedMatrix
    }
    
    # Retrieve the matrix from cache
    getMatrix <- function() {
        cache_hits["matrixHits"] <<- cache_hits["matrixHits"] + 1
        cachedMatrix
    }
    
    # Retrieve the matrix inverse from cache.  
    # May return Null if it has not been set yet.
    getInverse <- function() { 
      if (!is.null(inverseMatrix)) {  
          # Increment cache hit count to monitor usefulness and enable better testing
          cache_hits["inverseHits"] <<- cache_hits["inverseHits"]  + 1
      } 
      
      # Return whatever the current inverse is.
      inverseMatrix
    }
    
    # Cache the inverse matrix and return its value to allow chaining.
    setInverse <- function(newInverseMatrix) {
        cache_hits["inverseHits"] <<- 0 
        inverseMatrix <<- newInverseMatrix
        inverseMatrix
    }
    
    
    # Function to return the cache hit metrics.  Not required by assignment
    # Returns a vector with two fields: matrixHits, and inverseHits
    cacheHits <- function() cache_hits

    # Initialization of specified matrix
    setMatrix(cachedMatrix)
     
    # Return the API
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         getInverse = getInverse, setInverse = setInverse,
         cacheHits = cacheHits)
}


# Given a matrix cache object, returns the inverse of the curret matrix, if it is 
# possible to generate an inverse (only square matrices have inverses, and not even all
# square ones can be inverted).  
#
# The matrix inverse will be calculated only the first time, and then will be cached.
# Subsequent calls will be retrieved from cache until such a time as the the base matrix
# itself is reset.
# 
cacheSolve <- function(matrixCache, ...) {
  
    inverseMatrix <- matrixCache$getInverse()
    if (is.null(inverseMatrix)) { 
      
        # Null inverse matrix means we don't have it right now.  Try to calculate it.  Not all 
        # matrices are invertible -> use a tryCatch for graceful failure and reset to null
        # if solve does fail.
        currentMatrix = matrixCache$getMatrix()
  
        if (is.null(currentMatrix) || ncol(currentMatrix) != nrow(currentMatrix)) {
            print("Matrix is not a square!  Cannot calculated the inverse.")
        } else {
          
            # There are matrices that have no inverse -> solve can fail.
            # Using tryCatch to handle that gracefully.  
            inverseMatrix <- tryCatch( solve( currentMatrix ), 
                                       warning = function(w) 
                                           print("Matrix is not an invertible matrix!")
                                       ,
                                       error = function(e) {
                                           print("Matrix is not an invertible matrix!")
                                           NULL
                                       })
            matrixCache$setInverse(inverseMatrix)
        }
    }  
    
    inverseMatrix
}
