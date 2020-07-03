## The <<- operator assigns a value to an object in an environment that is
## different from the current environment.

## Creates a list that contains four separate functions that sets/gets the value
## of a numeric vector and sets/gets the value of the mean.
makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get, setmean = setmean, getmean = getmean)
}

## Calculates the mean of the value enclosed in makeVector and any extra data
## passed in as a parameter. It uses a caching mechanism to retrieve the mean
## value 'm' if it is not NULL.
cachemean <- function(x, ...) {
    m <- x$getmean()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}