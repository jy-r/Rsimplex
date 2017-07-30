#Example 1
#------------------
A <- matrix(c(2,2,3,1,3,2),nrow=2,byrow=TRUE)
b <- c(28,21)
constrains <- c(FALSE, TRUE) #Contrains >= are 1, <= are 0
C <- -c(77,27,56)

(sol <- Rsimplex(A,b,C,constrains, log = FALSE))
#------------------

#Example2
#------------------
A <- matrix(c(1,0,1,2,1,1),nrow=2,byrow=TRUE)
b <- c(24,30)
constrains <- c(TRUE, TRUE) #Contrains >= are 1, <= are 0
C <- -c(8,2,5)

(sol <- Rsimplex(A,b,C,constrains,max=FALSE, log = TRUE))
#Solution 132, x3=24, x2=6
#------------------