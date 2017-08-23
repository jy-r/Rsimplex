A <- matrix(c(2,1,1,1,2,1),nrow=2,byrow=TRUE)
b <- c(25,8)
constrains <- c(FALSE, FALSE) #Contrains >= are 1, <= are 0
C <- -c(24,20,14)
C2 <- c(24,20,14)


library(boot)


t1<-system.time(
simplex(C2,A,b, maxi=TRUE)
)

t2<-system.time((sol <- Rsimplex(A,b,C,constrains,max=TRUE, log = FALSE)))

print(t1)
print(t2)
