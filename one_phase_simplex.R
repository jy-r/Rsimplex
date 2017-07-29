library("knitr")

A <- matrix(c(15,2,5,12,0,5,0,3,4),nrow=2,byrow=TRUE)
b <- c(2500,3020)

C <- -c(8,2,5, 8, 2)

Rsimplex(A,b,C)


Rsimplex <- function(A,b,C,log=TRUE){
#check Input
if(dim(A)[1]!=dim(b)[1]){print("Matrix A doesnt correspond to vector b")}
  
  
# Setting up the matrix and vectors
I <- diag(dim(A)[1])
z <- c(C,rep(0,dim(I)[2]))
A <- cbind(A,I)
# Naming variables in matrix
colnames(A)<- paste0(rep("x",dim(A)[2]),seq(1,dim(A)[2]))
rownames(A)<- paste0(rep("x",dim(A)[1]),seq((dim(A)[2]-dim(I)[1]+1),dim(A)[2]))
# Setting object value for first iteration
z0 <- 0


repeat{
  #Checking for end
  if(all(z>=0)){
    print(paste("Optimal solution found:",round(z0,2),"Values:"))
    print(paste0(rownames(A),"=",round(b,3)))
    break
  }
  if(all(b<0)){
    print("Unbounded function")
    break}
  if(all(A[,key.col]<0)){
    print("Unbounded function")
    break}
  
  key.col <- which.min(z)
  t <- b/A[,key.col]
  key.row <- which.min(t[t>0])
  
  
  rownames(A)[key.row]<- paste0("x",key.col)
  
  if(isTRUE(log)){
  print(kable(round(rbind(cbind(A,b),z=c(z,z0)),2)))
  print(paste("key row: ",key.row,"| key column: ",key.col))
  }
  
  b[key.row] <- b[key.row]/A[key.row,key.col] 
  A[key.row,] <- A[key.row,]/A[key.row,key.col] 
  for(i in (1:dim(A)[1])[-key.row]){
    b[i] <- b[i]-b[key.row]*A[i,key.col]  
    A[i,] <- A[i,]-A[key.row,]*A[i,key.col]
  }
  z0 <- z0 - b[key.row]*z[key.col]
  z <- z-A[key.row,]*z[key.col] 
  
}

}
