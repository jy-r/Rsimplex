A <- matrix(rnorm(1000),nrow=100,byrow=TRUE)
b <- runif(100,30,45)
constrains <- c(FALSE, FALSE) #Contrains >= are 1, <= are 0
C <- -runif(10,0,10)
C2 <- -C


library(boot)


t1<-system.time(
simplex(C2,A,b, maxi=TRUE)
)

t2<-system.time((sol <- Rsimplex(A,b,C,constrains,max=TRUE, log = FALSE)))

print(t1)
print(t2)
