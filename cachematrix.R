#Programming assignment that uses nested functions and lexical scoping to reduce the burden of calculating the inverse of a matrix over and over again

#Basically, the first new matrix inversion that is input is calculated and stored in memory in case it's required later. If it is then it's not recaculated, just recalled.

#When it's "just recalled" we tell the function to tell us that with "getting cached data". Otherwise, if it's a new matrix to be inverted it's calculated.

#Hard to imagine how this will be relevant to my work but was helpful to understand the scoping and how R searches for things!

#makeCacheMatrix takes a matrix as its argument and stores a list and the inverse of the matrix

#cacheSolve takes an argument that is returned by makeCacheMatrix in order to return the inverse of the matrix stored in the environment of makeCacheMatrix 

#1. Initializing objects

##x is initialized as an empty matrix
##m is initialized and set to NULL as an object within the makeCacheMatrix environment

makeCacheMatrix <- function(x = matrix()) {             
        m <- NULL

#2. Defining behaviors of getters and setters
##set the value of x in the parent environment to y
##set the value of m in the parent environment to NULL, which clears previous executions of cacheSolve        

        set <- function(y) {
        x <<- y
        m <<- NULL
        }

        ##get() takes advantage of lexical scoping here because x isn't defined in the function, it retrieves value of x from the parent environment

        get <- function() x

##define the setter for inverse. m is defined above in the parent environment, hence the <<- operator.        
        
        setinverse <- function(inverse) m <<- inverse

##same deal as above for get(), taking advantage of lexical scoping to find the desired value of m
        
        getinverse <- function() m

#3. Return a fully formed object and naming the list elements, which lets us use the $ extract in cacheSolve        
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#cacheSolve takes the argument x and allows for additional agruments using the "..."

cacheSolve <- function(x, ...) {

#calling the getinverse function from argument x. we can use $ because of the list() defined in makeCacheMatrix

        m <- x$getinverse()

#checking to see if the inverse if equal to NULL or not. if it is NOT equal to NULL, then the we know it's been cached and return it (without recalucating it)        

        if(!is.null(m)) {
        message("getting cached data")
        return(m)
        }
#if the value if equal to NULL, we know we it's not cached and the inverse needs to be calculated.         
        
        data <- x$get()
        
        m <- solve(data, ...)
        
        x$setinverse(m)
        
        m
}
