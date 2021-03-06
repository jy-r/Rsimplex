#' Rsimplex
#' 
#' educational simplex solver (one, two phase)
#' @param A matrix of coefficeinets
#' @param b vector of constrains
#' @param constrains logical vector of constrain types TRUE >=, FALSE <=
#' @param max logical, default TRUE
#' @param log print iterations, default TRUE
#' @keywords simplex, linear programing
#' @import knitr
#' @export Rsimplex
#' @examples 
#' A <- matrix(c(1,0,1,2,1,1),nrow=2,byrow=TRUE)
#' b <- c(24,30)
#' constrains <- c(TRUE, TRUE)
#' C <- c(8,2,5)
#' (sol <- Rsimplex(A,b,C,constrains,max=FALSE, log = TRUE))
Rsimplex <- function(A,b,C, constrains=c(FALSE),max=TRUE, log=TRUE){
  C = -C
  #check Input
  if(dim(A)[1]!=length(b)){print("Dimensions of matrix A doesnt correspond to vector b")
    stop()}
  if(dim(A)[2]!=length(C)){print("Dimensions of matrix A doesnt correspond to vector C")
    stop()}  
  
  # Setting up the matrix and vectors
  I <- cbind(diag(dim(A)[1]))
  
  if(any(constrains)){
    Y <- as.matrix(I[,constrains])
    I[,constrains] <- as.matrix(-I[,constrains])
    A <- cbind(A,I,Y)
    colnames(A)<- c(paste0(rep("x",(dim(A)[2]-dim(Y)[2])),seq(1,(dim(A)[2])-dim(Y)[2])),paste0(rep("y",dim(Y)[2]),seq(1,dim(Y)[2])))
    rownames(A)<- paste0(rep("x",dim(A)[1]),seq((dim(A)[2]-dim(I)[1]),(dim(A)[2]-dim(Y)[2])))
    rownames(A)[constrains]<- paste0(rep("y",dim(Y)[2]),seq(1,dim(Y)[2]))
    #objective function
    z <- c(C,rep(0,(dim(I)[2])+dim(Y)[2]))
    z0<-0
    zy <- c(colSums(rbind(A[constrains,seq(1,(dim(A)[2])-dim(Y)[2])],rep(0,((dim(A)[2]-dim(Y)[2]))))),rep(0,dim(Y)[2]))
    zy0 <- sum(b[constrains])
  }else{
    z <- c(C,rep(0,dim(I)[2]))
    zy<-c(0)
    A <- cbind(A,I)
    # Naming variables in matrix
    colnames(A)<- paste0(rep("x",dim(A)[2]),seq(1,dim(A)[2]))
    rownames(A)<- paste0(rep("x",dim(A)[1]),seq((dim(A)[2]-dim(I)[1]+1),dim(A)[2]))
    # Setting object value for first iteration
    z0 <- 0
  }
  
  
  if(any(constrains)){
    repeat{
      if(all(zy<=0)&(zy0==0)){
        print(kable(round(rbind(cbind(A,b),z=c(z,z0),zy=c(zy,zy0)),2)))
        print("Frist phase is done")
        A<- A[,1:(dim(A)[2]-dim(Y)[2])]
        z <- z[1:(length(z)-dim(Y)[2])]
        break
      }else if(all(zy<=0)){
        stop("Doenst have a solution")
      }
      
      #select key
      key.col <- which.max(zy)
      t <- b/A[,key.col]
      t[t<0] <- NA
      key.row <- which.min(t)   
      
      if(all(b<0)){
        print("Unbounded function")
        break}
      if(all(A[,key.col]<0)){
        print("Unbounded function")
        break}
      
      #logs
      if(isTRUE(log)){
        print(kable(round(rbind(cbind(A,b),z=c(z,z0),zy=c(zy,zy0)),2)))
        print(paste("key row: ",key.row,"| key column: ",key.col))
      }
      rownames(A)[key.row]<- paste0("x",key.col)
      
      b[key.row] <- b[key.row]/A[key.row,key.col] 
      A[key.row,] <- A[key.row,]/A[key.row,key.col] 
      for(i in (1:dim(A)[1])[-key.row]){
        b[i] <- b[i]-b[key.row]*A[i,key.col]  
        A[i,] <- A[i,]-A[key.row,]*A[i,key.col]
      }
      z0 <- z0 - b[key.row]*z[key.col]
      zy0 <- zy0 - b[key.row]*zy[key.col]
      z <- z-A[key.row,]*z[key.col] 
      zy <- zy-A[key.row,]*zy[key.col] 
      
    }
  }
  
  repeat{
    #min/max
    if(max==TRUE){
      check.opt<-z>=0
      key.col <- which.min(z)
      t <- b/A[,key.col]
      t[t<0] <- NA
      key.row <- which.min(t)
    }else{
      check.opt<-z<=0
      key.col <- which.max(z)
      t <- b/A[,key.col]
      t[t<0] <- NA
      key.row <- which.min(t)
    }
    #Check for end
    if(all(check.opt)){
      print(kable(round(rbind(cbind(A,b),z=c(z,z0)),2)))
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
    
    #log on/off 
    if(isTRUE(log)){
      print(kable(round(rbind(cbind(A,b),z=c(z,z0)),2)))
      print(paste("key row: ",key.row,"| key column: ",key.col))
    }
    rownames(A)[key.row]<- paste0("x",key.col)
    #calculate
    b[key.row] <- b[key.row]/A[key.row,key.col] 
    A[key.row,] <- A[key.row,]/A[key.row,key.col] 
    for(i in (1:dim(A)[1])[-key.row]){
      b[i] <- b[i]-b[key.row]*A[i,key.col]  
      A[i,] <- A[i,]-A[key.row,]*A[i,key.col]
    }
    z0 <- z0 - b[key.row]*z[key.col]
    z <- z-A[key.row,]*z[key.col] 
    
    
    
    
  }
  output<- list(simplex.table=round(rbind(cbind(A,b),z=c(z,z0)),2),
                obj=z0)
  invisible(output)
}