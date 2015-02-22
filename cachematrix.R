# Adaptation of makeVector function by rdpeng (Dr. Roger Peng) 
# source https://github.com/A-Desh/ProgrammingAssignment2

# Similar to the makeVector function, the makeCacheMatrix function
# performs the following actions
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the matrix inverse
# 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    mat_inv    <- NULL # Set the inverse value to NULL prior to any computation

    # Step 1: set the value of the matrix
    set <- function(y) {
        x <<- y
        mat_inv <<- NULL   # since the matrix changed
    }

    # Step 2: get the value of the matrix
    get    <- function() x

    # Step 3: set the value of the matrix inverse
    setinv <- function(minv) mat_inv <<- minv

    # Step 4: get the value of the matrix inverse
    getinv <- function() mat_inv

    # return a list of all the above functions
    list(set = set, get = get
         , setinv = setinv
         , getinv = getinv
         )
}


# Adaptation of cachemean function by rdpeng 
# source https://github.com/A-Desh/ProgrammingAssignment2

# The function calculates the matrix inverse of the special "matrix"
# created using "makeCacheMatrix" function (written above). 
# Whether the matrix inverse has already been calculated is checked first. 
# If the matrix inverse is calculated then it gets the inverse from the cache. 
# If not, then it calculates the inverse of the matrix and sets its value in the cache 
# uses the setmean function to set value.

cacheSolve <- function(x, ...) {
        # Check if the matrix inverse is already cached
        mat_inv    <- x$getinv()
        if(!isnull(mat_inv)){
            message("getting cached data")
            return(mat_inv)
        }
        # If the matrix is not cached then we load it into data
        data <- x$get()
        # Solve the matrix for the inverse
        mat_inv <- Solve(data, ...)
        # Then cache the inverse
        x$setinv(mat_inv)
        # And return the value
        mat_inv
}

