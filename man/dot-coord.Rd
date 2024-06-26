% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coordinate.r
\name{.coord}
\alias{.coord}
\title{coordinate function}
\usage{
.coord(n.dk, n.tk)
}
\arguments{
\item{n.dk}{vector of integers indicating the coordinates of interest of a matrix or array. The vector length should match the dimension of the object of interest.}

\item{n.tk}{vector of integers indicating the dimension of the array of interest, typically dim(obj), where obj is the object of interest.}
}
\value{
output is a (numeric) scalar.
}
\description{
gives the matrix or array position corresponding to the coordinates of interest. Allows fast filling of arrays (via pointers).
}
\examples{
dimw = c(10,20,30,4) 
arw  = array(1:prod(dimw),dimw)
arw[.coord(c(10,20,15,3),dim(arw))] == .coord(c(10,20,15,3),dim(arw))
}
