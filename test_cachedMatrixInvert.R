
# this will define makeCacheMatrix and cacheSolve
source('cachematrix.R')

source('cachemean.R')


# test a vector for mean-caching behavior under makeVector and cacheMean
test_cacheMean <- function(some_vector) {
  its_mean <- mean(some_vector)
  v <- makeVector(some_vector)
  stopifnot(v$get() == some_vector)
  stopifnot(v$getmean() == NULL)
  stopifnot(cachemean(v) == its_mean)
  stopifnot(v$getmean() == its_mean)
}

# test a matrix for inverse-caching behavior under makeCacheMatrix and
# cacheSolve
test_makeCacheMatrix <- function(some_matrix) {
  message(some_matrix)
  its_inverse <- solve(some_matrix)
  m <- makeCacheMatrix(some_matrix)
  stopifnot(m$get() == some_matrix)
  stopifnot(m$getinverse() == NULL)
  stopifnot(cacheSolve(m) == its_inverse)
  stopifnot(m$getinverse() == its_inverse)
}

test_cacheMean(c(2,3,5,7))
test_cacheMean(rnorm(100))
lapply(seq(1000), function(x) test_cacheMean(rnorm(100)))

test_makeCacheMatrix(matrix(c(2,3,5,7),2))
# it is possible to randomly generate a singular matrix but unlikely. if the
# test fails, just run it again ;-)
test_makeCacheMatrix(matrix(rnorm(100), 10))
lapply(seq(1000), function(x) test_makeCacheMatrix(matrix(rnorm(100), 10)))

