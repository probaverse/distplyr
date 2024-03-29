% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mutate_parameters.R
\name{mutate_parameters}
\alias{mutate_parameters}
\title{Mutate Parameters}
\usage{
mutate_parameters(distribution, ...)
}
\arguments{
\item{distribution}{Parametric distribution}

\item{...}{Named expressions; quoted. Names should be parameters names of the
distribution. Expressions can involve computations with other parameters.}
}
\value{
The input distribution, with the parameters modified as specified
in \code{...}.
}
\description{
Modify distribution parameters. A lightweight function that only
checks that the parameter you're modifying already exists in the
distribution.
}
\details{
Parameters are not modified on-the-fly. That means parameters that are
modified earlier in \code{...} still retain their original values for use
downstream in \code{...}.
}
\examples{
d <- distionary::dst_unif(1, 3)
distplyr:::mutate_parameters(d, min = max - min)
}
