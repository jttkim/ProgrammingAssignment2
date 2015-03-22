## Construct a matrix object that lugs around a cache containing its
## inverse

makeCacheMatrix <- function(x = matrix())
{
  # initialise cached inverse to NULL, indicating we don't have a cached value
  cachedInverse <- NULL;

  # set a new matrix value, and invalidate cache
  set <- function(newMatrix)
  {
    x <<- newMatrix;
    cachedInverse <<- NULL;
    return(invisible(x));
  }

  # get the current matrix value
  get <- function()
  {
    return(x);
  }

  # set the cached inverse
  setInverse <- function(newInverse)
  {
    cachedInverse <<- newInverse;
    return(invisible(newInverse));
  }

  # get the cached inverse
  getInverse <- function()
  {
    return(cachedInverse);
  }

  return(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse));
}


## Write a short comment describing this function

cacheSolve <- function(x, ...)
{
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse();
  if (is.null(inverse))
  {
    message("no cached value, computing inverse")
    inverse <- solve(x$get(), ...);
    x$setInverse(inverse);
  }
  else
  {
    message("found cached value");
  }
  return(inverse);
}


## the format of the oursework seems to require two separate
## functions, one to construct an inverse caching matrix and one to
## get at the inverse (cached where possible).

## This design seems unnecessarily clumsy, as the two functions are
## rather tightly coupled, and the setInverse function is rather
## undesirable as it enables setting of any inverse value, and
## therefore incurs the risk of constructing an inconsistent object.
## So here's a more compact alternative:
makeCacheMatrix2 <- function(x = matrix())
{
  # initialise cached inverse to NULL, indicating we don't have a cached value
  cachedInverse <- NULL;

  # set a new matrix value, and invalidate cache
  set <- function(newMatrix)
  {
    x <<- newMatrix;
    cachedInverse <<- NULL;
    return(invisible(x));
  }

  # get the current matrix value
  get <- function()
  {
    return(x);
  }

  # get the inverse and cache it for future invocations
  getInverse <- function(...)
  {
    if (is.null(cachedInverse))
    {
      message("computing cached inverse");
      cachedInverse <<- solve(x, ...);
    }
    else
    {
      message("returning cached inverse");
    }
    return(cachedInverse);
  }

  return(list(set = set, get = get, getInverse = getInverse));
}


