% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootstrap.R
\name{bootstrap2}
\alias{bootstrap2}
\title{Bootstrap a confidence interval from a set of data.}
\usage{
bootstrap2(iter = 10000, x, fun = "mean", alpha = 0.05)
}
\arguments{
\item{iter}{provide the number of iterations to experiment with.}

\item{x}{provide the population o sample from.}

\item{fun}{provide the function you want to apply to your sample.}

\item{alpha}{provide the confidence interval you want to get back.}
}
\value{
list of useful items.
}
\description{
Bootstrap a confidence interval from a set of data.
}
\examples{
bootstrap2(10000, c(1,2,3,4,5), fun = "mean", alpha = 0.2)
}
