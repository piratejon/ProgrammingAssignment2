
## this file defines the interface to a matrix with cached inverse

## makeCacheMatrix turns a matrix into a marsupial, which is to say it imbues
## it with state constituting the contents of a "pouch" and an interface
## consisting of accessor and retrieval methods amounting to a claw that can
## put a little baby or something in the pouch and then take a copy out later
## when called upon by a zookeeper vis-a-vis programmer employing the library.
## The contents of the pouch are indeed arbitrary but they could for example
## be the inverse of the marsupial which happens to also be a matrix with
## determinant zero (i.e. is invertible) -- the possibilities are endless!
makeCacheMatrix <- function(koala = matrix()) {

# Call this method to set the object's matrix and reset its inverse.
  set <- function(wallaby) {
    marsupial <<- wallaby
    pouch_thing <<- NULL
  }

# Retrieve this object's matrix
  get <- function() marsupial

# Retrieve the tag-along value. it could be anything, but when used with
# cacheSolve it is taken to be the inverse of m
  wasIstInDerPouch <- function() pouch_thing

# Referencing the technique practiced by marsupials storing an item such as a
# small baby in their pouch: <https://www.youtube.com/watch?v=2qBgMmRMpOo>
  doThePouchStash <- function(wombat) pouch_thing <<- wombat

# use our "constructor" to initialize the object's values
  set(koala)

# pretend we are not actually a marsupial stashing babies in the pouch, but a
# useful class that stores a matrix along with its inverse (once computed, by
# another method). caveat emptor: retrieved inverses may or may not come out
# slathered in Marsupial Pouch Mucus
# <https://www.youtube.com/watch?v=GTNAJTjC8bQ>
  list(
      set = set
      , get = get
      , getinverse = wasIstInDerPouch
      , setinverse = doThePouchStash
      )
}

## cacheSolve checks if a cacheMatrix's tag-along value is null and if so,
## computes the inverse using solve() and stores it in the tag-along value
## for future retrieval. if the value was not null it understands it to be
## a cached inverse and returns it.
cacheSolve <- function(m, ...) {
  i <- m$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }

# "else" ... the not-null check failed, so compute the inverse:
  i <- solve(m$get(), ...)

# store it in the matrix's "pouch"
  m$setinverse(i)

# and return it
  i
}

