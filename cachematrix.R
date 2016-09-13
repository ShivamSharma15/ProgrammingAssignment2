## Matrix inverse are usually expensive to compute but can be really useful for some purposes.
## The following functions are used to cache the inverse of a matrix

## makeCacheMatrix makes a list which contains function which can:
## 1. set the value of the matrix
## 2. get the value of that.
## 3. set the value of inverse of the matrix.
## 4. get the value of that. 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv<<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set=set,get=get, setinverse=setinverse, getinverse=getinverse)
}

## The following function would return the inverse of the matrix. 
## First it checks if the inverse has already been computed,
## If so, it gives the result and skips the computation.
## If not, it computes the inverse, sets the value in the cache via
## setinverse function. 

## This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
        	message("giving cached data")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}


## Crosschecking Example:
## > x = rbind(c(1, -1/2), c(-1/2, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##      [,1] [,2]
## [1,]  1.0 -0.5
## [2,] -0.5  1.0


## No cache in first run then
## > cacheSolve(m)
##        [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333

## In the secnd run, retrieving from the cache
## > cacheSolve(m)
## getting cached data
##         [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333
