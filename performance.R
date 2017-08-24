library(Rsimplex)
library(boot)
library(ggplot2)
t1<-c()
t2<-c()
size <- c()
p=1


for (i in seq(100,2000,10)){
size[i]=i
p = p+1
j=i/10
  
A <- matrix(rnorm(i),nrow=j,byrow=TRUE)
b <- runif(j,30,45)
constrains <- c(FALSE, FALSE) #Contrains >= are 1, <= are 0
C <- runif(10,0,10)






t1[p]<-system.time(invisible(simplex(C,A,b, maxi=TRUE)))[3]

t2[p]<-system.time(invisible(Rsimplex(A,b,C,constrains,max=TRUE, log = FALSE)))[3]

}


ggplot(data.frame(t1,t2,size))+geom_line(aes(size,t1,color="t1"))+geom_line(aes(size,t2,color="t2"))
