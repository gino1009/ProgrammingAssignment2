##Two functions to cache the inverse of a matrix

#The first:

#Create a special matrix

makeCacheMatrix <- function(m = matrix()){
        #Initialization
        z <- NULL
        #Setting the matrix
        set <- function(matrix){
                m <<- matrix
                z <<- NULL
        }
        
        ##Getting the matrix
        get <- function(){
                #Return the matrix
                m
        }
        
        ##Setting the inverse of the matrix
        
        set.inverse <- function(inverse){
                z <<- inverse
        }
        
        ##Getting the inverse of the matrix
        
        get.inverse <<- function(){
                #Return the inverse property
                z
        }
        
        ##Return a list of the methods
        list(set=set, get=get, set.inverse = set.inverse, get.inverse = get.inverse)
}

cacheSolve <- function(x,...){
        
        m <- x$get.inverse()
        
        if(!is.null(m)){
                message('cached data')
                return(m)
        }
        
        ##Getting matrix from the object
        
        data <- x$get()
        
        ##Calculate the inverse
        
        m <- solve(data) %*% data
        
        ##Setting the inverse of the object
        
        x$set.inverse(m)
        
        ##Return the matrix
        
        m
}

        
