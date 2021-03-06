% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rsimplex.R
\name{Rsimplex}
\alias{Rsimplex}
\title{Rsimplex}
\usage{
Rsimplex(A, b, C, constrains = c(FALSE), max = TRUE, log = TRUE)
}
\arguments{
\item{A}{matrix of coefficeinets}

\item{b}{vector of constrains}

\item{constrains}{logical vector of constrain types TRUE >=, FALSE <=}

\item{max}{logical, default TRUE}

\item{log}{print iterations, default TRUE}
}
\description{
educational simplex solver (one, two phase)
}
\examples{
A <- matrix(c(1,0,1,2,1,1),nrow=2,byrow=TRUE)
b <- c(24,30)
constrains <- c(TRUE, TRUE)
C <- c(8,2,5)
(sol <- Rsimplex(A,b,C,constrains,max=FALSE, log = TRUE))
}
\keyword{linear}
\keyword{programing}
\keyword{simplex,}
