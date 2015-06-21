
## This function is written following a pattern provided in 
## the assignment's instruction. It has not been tested yet.
## My undersatnding this function transforms a regular matrix 
## into a "sort-of" inherited object that allows cached 
## Inverse operation. And the next function cacheSolve implements 
## that cashed inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL;
        set <- function(y) {
                x <<- y;
                inv <<- NULL;
        };
        get <- function() x;
        setinverse <- function(solve) inv <<- solve;
        getinverse <- function() inv;
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse);
}


## This function is written following a pattern provided in 
## the assignment's instruction. It has not been tested yet.
## (see also comments for makeCacheMatrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getinverse();
        if(!is.null(inv)) {
                message("getting cached data");
                return(inv);
        };
        data <- x$get();
        inv <- solve(data, ...);
        x$setinverse(inv);
        inv;
}

## these two functions are taken from the instruction 
## to the assignment. They are being used as a reference


makeVector <- function(x = numeric()) {
        m <- NULL;
        set <- function(y) {
                x <<- y;
                m <<- NULL;
        };
        get <- function() x;
        setmean <- function(mean) m <<- mean;
        getmean <- function() m;
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean);
}

cachemean <- function(x, ...) {
        m <- x$getmean();
        if(!is.null(m)) {
                message("getting cached data");
                return(m);
        };
        data <- x$get();
        m <- mean(data, ...);
        x$setmean(m);
        m;
}