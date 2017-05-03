#Function: randomMod
#use uniform distribution to pick random index in matrix
#to modify
#still need modification for collsion
#mat ->matrix to modify
#numE ->number of indexes to modify
randomMod<-function(mat, numE){
  if (numE==0){return(mat)}
  rmod<- floor(runif(numE, min=1, max=nrow(mat)+1))
  cmod<- floor(runif(numE, min=1, max=ncol(mat)+1))
  
  for(i in 1:length(rmod)){
    mat[rmod[i],cmod[i]]<- !mat[rmod[i],cmod[i]]
  }
  return(mat)
  
}

#Function: numDiff
#Count the number of discrepancies between matrices
#a,b -> the matrix to compare 
numDiff<-function(a,b){
  return(sum(a!=b))
  
}

#Function: oldALS
#Generate A matrix using ALS technique
#with 7 iterations in total
#R -> result matrix
#Q -> expert defined Q matrix
oldALS<-function(R,Q){
  S <- solve(t(Q) %*% Q) %*% t(Q) %*% R
  Q1 <- R %*% t(S) %*% solve(S %*% t(S))
  for(i in 2:7){
    S <- solve(t(Q1) %*% Q1) %*% t(Q1) %*% R
    Q1 <- R %*% t(S) %*% solve(S %*% t(S))
  }
  
  return(round(Q1))
}


#Function: ALS
#Generate A matrix using ALS technique
#R -> result matrix
#Q -> expert defined Q matrix
#tol ->tolerance level
#maxIt ->maximum number of iteration
#
#Output:
#Q -> new Q matrix
#S -> new Student ability matrix
#N -> number of iterations
ALS <- function(R, Q0, tol=0.001, maxIt=20){

    cont <- TRUE
    N <- 1
    
    while(cont) {
        
        S <- solve(t(Q0) %*% Q0) %*% t(Q0) %*% R
        Q <- R %*% t(S) %*% solve(S %*% t(S))
        N <- N+1 
        
        cont <- (squareError(Q, Q0)>tol) & (N <= maxIt)
        Q0 <- Q
    }
    
    return(list(Q=Q, S=S, N=N-1))

}


#Function: numError
#Count the number of discrepancies between matrices
#a,b -> the matrix to compare 
numError<-function(a,b){
  N<-((a&!b)|(!a&b))
  return(N)
}

#Function: squareError
#Calculate the square error between two matrices
#a,b -> the matrix to compare 
#return square error
squareError<-function(a,b)
{
  return(sum((a-b)*(a-b)))
}

#Function: roundProc
#Round the numbers in a matrix according to the threshold
#round to 0,1
#a -> the matrix to to round 
#
roundProc<-function(a, threshold=.5){
    return(1*(a > threshold))
}  




