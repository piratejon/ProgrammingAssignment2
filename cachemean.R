
# x defaults to an empty numeric vector in case a vector wasn't specified
makeVector <- function(x = numeric()) {

# we're making a new one so let the mean be null
  m <- NULL

# set the value of the vector associated with this object. the scope assignment
# operator looks in the enclosing scope for x and m. x is assigned with the
# vector value we're setting here. m is set to null in case it held a cached
# mean from the previous x vector.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

# return the value of whatever was passed in as x or the default numeric()
  get <- function() x

# set the parent-scope m to the passed mean value
  setmean <- function(mean) m <<- mean

# return m
  getmean <- function() m

# returns the list with the given named elements, constituting the object's
# programmatic interface
  list(set = set, get = get, setmean = setmean, getmean = getmean)
}

cachemean <- function(x, ...) {
# evaluate x's getmean() and store the result in m
  m <- x$getmean()

# if it was not null then it was cached result and can be returned
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }

# otherwise...
# in makeVector, get() took the value of the passed vector, so we retrieve it
# by calling get() now
  data <- x$get()

# compute the mean on it along with whatever other options were passed in such
# as na.rm=TRUE or similar
  m <- mean(data, ...)

# cache the computed mean by calling the object's store function, setmean()
  x$setmean(m)

# and finally return the mean
  m
}

