### MichaelEsparza012 
### Comments that give an overall description of what the my functions do.
### Our Obective in this assignment is to write 2 functions.  The 2 functions are:
### Function 1, "makeCacheMatrix" 
### Function 2, "cacheSolve"
### In the Case of Function 1, "makeCacheMatrix", it is a special function that creates a special matrix object that can cache its own inverse for the input. As described in the instructions for Programming Assignment 2, the assumption is that the matrix supplied is always invertible.
### Code for "makeCacheMatrix"
makeCacheMatrix <- function(x = matrix()) {
+        inv <- NULL
+        set <- function(y) {
+                x <<- y
+                inv <<- NULL
+        }
+        get <- function() x
+        setInverse <- function(inverse) inv <<- inverse
+        getInverse <- function() inv
+        list(set = set,
+             get = get,
+             setInverse = setInverse,
+             getInverse = getInverse)
 }
### In the Case of Function 2, "cacheSolve", it is scripted as a function that calculates the inverse of the special matrix returned by "makeCacheMatrix" as presented above. In the event that the inverse has already been computed, and the matrix has not changed, then "cacheSolve" shall retrieve the inverse of the cache.
### Code for "cacheSolve"
cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'	       
+  inv <- x$getinv()
+  if(!is.null(inv)) {
+    message("getting cached result")
+    return(inv)
+  }
+  data <- x$get()
+  inv <- solve(data, ...)
+  x$setinv(inv)
+  inv
 }
### Proof of Concept - CHECKING THE CODE
###--------------------------------------
### Methodology: Running the Code in RStudio
### Function 1 - "makeCacheMatrix"
### Source returns: > source('~/.active-rstudio-document')
### Run Returns: > makeCacheMatrix <- function(x = matrix()) {
+   inv <- NULL
+   set <- function(y) {
+     x <<- y
+     inv <<- NULL
+     }
+   get <- function() x
+   setinv <- function(inverse) inv <<- inverse
+   getinv <- function() inv
+   list(set = set, get = get, setinv = setinv, getinv = getinv)
+ }
### > my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
> my_matrix$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
my_matrix$getInverse()
NULL
### Function 2 - "cacheSolve"
### Source Returns: > source('~/.active-rstudio-document')
### Run Returns: > cacheSolve <- function(x, ...) {
+   inv <- x$getInverse()
+   if (!is.null(inv)) {
+     message("getting cached data")
+     return(inv)
+     }
+   mat <- x$get()
+   inv <- solve(mat, ...)
+   x$setInverse(inv)
+   inv
+ }
### cacheSolve(my_matrix)
> cacheSolve(my_matrix)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
### cachesolve(my_matrix)
> cacheSolve(my_matrix)
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
### my_matrix$getInverse
> my_matrix$getInverse()
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
### my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
### my_matrix$get()
> my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
> my_matrix$get()
     [,1] [,2]
[1,]    2    1
[2,]    2    4
### my_matrix$getInverse()
> my_matrix$getInverse()
NULL
### cacheSolve(my_matrix)
> cacheSolve(my_matrix)
           [,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333
### cacheSolve(my_matrix)
> cacheSolve(my_matrix)
getting cached data
           [,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333
### my_matrix$getInverse()
> my_matrix$getInverse()
           [,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333
### End of Proof of Concept - CHECKING THE CODE
### --------------------------------------------
