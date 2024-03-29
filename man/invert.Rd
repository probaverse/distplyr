% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/verb-invert.R
\name{invert}
\alias{invert}
\title{Inverse Distribution}
\usage{
invert(distribution)
}
\arguments{
\item{distribution}{A probability distribution.}
}
\value{
An inverse distribution.
Specifically, a distribution with subclass "inverse".
}
\description{
Access the inverse of a distribution. Specifically, if \code{X} is a random
variable coming from a distribution, \code{invert()} returns the
distribution of \code{1 / X}.
}
\note{
An error is returned if the original distribution
has 0 as a possible outcome
(i.e., \code{eval_pmf(distribution, at = 0, strict = FALSE)} is non-zero),
because 0 does not have a reciprocal.

You can also obtain the inverse distribution by putting
the distribution in the denominator of \code{/}.
}
\examples{
1 / (distionary::dst_pois(3.4) + 1)
invert(distionary::dst_norm(0, 1))
}
\seealso{
\code{flip()}, \code{scale()}
}
