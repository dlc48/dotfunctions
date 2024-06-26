% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/installpackages.r
\name{.ip}
\alias{.ip}
\title{shorcut}
\usage{
.ip(pkg, mirror = 1, dependencies = TRUE, quiet = FALSE, path = NULL)
}
\arguments{
\item{pkg}{string indicating the name of the package to install}

\item{mirror}{source mirror: 1 for ETH, any other number for Imperial}

\item{dependencies}{logical for dependencies}

\item{quiet}{logical for messages}

\item{path}{if not NULL (default), path to a text file to which the name of the installed package will be added}
}
\description{
function allowing to install and load CRAN packages
}
