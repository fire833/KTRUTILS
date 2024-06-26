% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distributions.R
\name{hyper_sim}
\alias{hyper_sim}
\title{Run a hypergeometric simulation with a fixed bag of elements.}
\usage{
hyper_sim(iter = 100, N = 20, r = 12, n = 5)
}
\arguments{
\item{iter}{provide the number of iterations you wish to run in your
simulation.}

\item{N}{provide the size of the sample you will sample from.}

\item{r}{provide the number of items in the sample that will be red or
false, what will the binary split in the sample be?}

\item{n}{Provide the sampled size for every iteration.}
}
\value{
nothing, just print out a barplot and the resulting tables of
the simulation.
}
\description{
Run a hypergeometric simulation with a fixed bag of elements.
}
\examples{
hyper_sim(iter = 10000, N = 20, r = 10, n = 6)
hyper_sim(iter = 500, N = 30, r = 5, n = 10)
}
