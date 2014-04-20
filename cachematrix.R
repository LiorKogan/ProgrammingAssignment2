# Creates a a list of functions for set/get a matrix and its inverse:
# set   : set the value of the matrix
# get   : get the value of the matrix
# setinv: set the value of the inverse matrix
# getinv: get the value of the inverse matrix
makeMatrix <- function(x = matrix())
{
	# x stores the input matrix
	# i stores the inverse of x

	i   <- NULL                               # inverse needs recalc
	set <- function(y)                        # construct a func that sets x to its input param
	{
		x <<- y                               # set matrix
		i <<- NULL                            # matrix had changed - inverse needs recalc
	}
	
	get     <- function()    x                # construct a func that returns x
	set_inv <- function(inv) i <<- inv        # construct a func that sets "i" to its input param
	get_inv <- function()    i                # construct a func that returns i
	
	list(set = set, get = get, setinv = set_inv, getinv = get_inv) # return list of functions
}

# Calculates the inverse of a matrix prepared using makeMatrix()
# If the inverse had already been calculated - it gets it from the cache and skip the computation.
# Otherwise, it calculates the inverse of the matrix and sets it in the cache via the setinv function.
cacheInv <- function(x, ...)
{
	i <- x$getinv()                           # try getting inverse matrix from the cache
	if (!is.null(i))                          # if inverse matrix was calculated before
	{
		message("getting cached data")
		return(i)                             # return the cached inverse
	}
	
	data <- x$get()                           # get the cached matrix
	i    <- solve(data, ...)                  # caclulate its inverse
	
	x$setinv(i)                               # cache the inverse matrix
	i                                         # return the inverse matrix
}

# Sample use
# ================================================
# x<- matrix(sample(1:100), nrow=10, ncol=10) # random permutation of 1:100
# y<- makeMatrix(x)                           # generate cacheable matrix
# cacheInv(y)                                 # calc and cache Inverse
# cacheInv(y)                                 # get inverse from cache
