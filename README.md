# Rsimplex
Implementation of simplex method in R. This implementation is not computationally efficient and goal is just to create simple educational solver, which can be somewhat useful to check manual calculation of simple exercises.  

## Installation

Package can be installed by:

```
devtools::install_github("jy-r/Rsimplex")
```

## Guide

```
Rsimplex(A, b, C, constrains = c(FALSE), max = TRUE, log = TRUE)
```

where:


*A* is matrix of coefficeinets

*b* vector of constrains

*constrains* logical vector of constrain types TRUE >=, FALSE <=

*max* logical value, whenever objective function is maximizing or minimalizing , default TRUE

*log* print iterations, default TRUE

## Example

For more examples see ```example.r```

```
A <- matrix(c(1,0,1,2,1,1),nrow=2,byrow=TRUE)
b <- c(24,30)
constrains <- c(TRUE, TRUE)
C <- c(8,2,5)
(sol <- Rsimplex(A,b,C,constrains,max=FALSE, log = TRUE))
```
