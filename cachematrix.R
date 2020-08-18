#This function creates a special matrix object that can cache its inverse 
makecachematrix<-function(x=matrix()){
  inv<-NULL
  
  #set a matrix x
  set<-function(y){
    x<<-y
  #first set inverse of matrix=null
    inv<<-NULL
  }
  
  #This function returns x  
  get<-function() x
  
  #setting inverse of a matrix using inbuild inverse method
  setInverse <- function(inverse) inv <<- inverse
  
  #returning inverse of a matrix
  getInverse <- function() inv
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cachsolve<-function(x, ...){
  
  #return a matrix that is inverse of x
  inv<-x$getInverse()
  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$getInverse(inv)
  inv
  
}

  
