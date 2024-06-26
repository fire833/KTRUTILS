% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_rss}
\alias{get_rss}
\title{Get the residual sum of squares for a vector of quantitative
data as predicted values.}
\usage{
get_rss(y, yhat)
}
\arguments{
\item{y}{provide the vector of actual values you are wishing
to model against.}

\item{yhat}{provide the vector of fitted values for the dataset
based on your model.}
}
\value{
The RSS of a dataset
}
\description{
Get the residual sum of squares for a vector of quantitative
data as predicted values.
}
\examples{
rss <- get_rss(c(1, 2, 3, 4, 5), c(2, 3, 4, 5, 6))
}
