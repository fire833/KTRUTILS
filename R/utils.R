# Copyright (C) 2024 Kendall Tauser
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

#' @param vector provide the vector you wish to extract z values from
#'
#' @title Get the z values for a vector
#' @return another vector with z values
#' @export
#'
#' @examples
#' z <- get_z(c(1, 2, 3, 4, 5))
get_z <- function(vector) {
  return((vector - mean(vector)) / stats::sd(vector))
}

#' @param vector provide the vector you wish to get the variance from
#'
#' @title Get the variance for a vector of quantitative data.
#' @return The variance of a dataset
#' @export
#'
#' @examples
#' s_squared <- get_variance(c(1, 2, 3, 4, 5))
get_variance <- function(vector) {
  return(sum((vector - mean(vector))^2 / length(vector) - 1))
}

#' @param y provide the vector of actual values you are wishing
#' to model against.
#' @param yhat provide the vector of fitted values for the dataset
#' based on your model.
#'
#' @title Get the total sum of squares for a vector of quantitative
#' data as predicted values.
#' @return The TSS of a dataset
#' @export
#'
#' @examples
#' tss <- get_tss(c(1, 2, 3, 4, 5), c(2, 3, 4, 5, 6))
get_tss <- function(y, yhat) {
  return(sum((y - mean(y))^2))
}

#' @param y provide the vector of actual values you are wishing
#' to model against.
#' @param yhat provide the vector of fitted values for the dataset
#' based on your model.
#'
#' @title Get the mean sum of squares for a vector of quantitative
#' data as predicted values.
#' @return The MSS of a dataset
#' @export
#'
#' @examples
#' mss <- get_mss(c(1, 2, 3, 4, 5), c(2, 3, 4, 5, 6))
get_mss <- function(y, yhat) {
  return(sum((y - yhat)^2))
}

#' @param y provide the vector of actual values you are wishing
#' to model against.
#' @param yhat provide the vector of fitted values for the dataset
#' based on your model.
#'
#' @title Get the residual sum of squares for a vector of quantitative
#' data as predicted values.
#' @return The RSS of a dataset
#' @export
#'
#' @examples
#' rss <- get_rss(c(1, 2, 3, 4, 5), c(2, 3, 4, 5, 6))
get_rss <- function(y, yhat) {
  return(sum((yhat - mean(y))^2))
}

#' @param k provide the number of standard deviations from the
#' mean you are hoping to capture.
#'
#' @title Get the minimum percentage of observations within k
#' standard deviations of the mean for any given sample.
#' @return The percentage of sample within k standard deviations
#' of the mean.
#' @export
#'
#' @examples
#' pct <- get_chebyshev(2)
#' pct <- get_chebyshev(4)
get_chebyshev <- function(k) {
  return(1 - 1 / (k^2))
}

#' @param x The x param to utilize in your table.
#' @param y The y param to utilize in your table.
#'
#' @title Create a table with marginals given two variables.
#' @return The table printed to stdout.
#' @export
#'
#' @examples
#' get_table_with_margins(c(1,2,3,4), c(4,5,6,7))
get_table_with_margins <- function(x, y) {
  stats::addmargins(table(x, y))
}

#' @param u The probability someone is a user
#' @param tu The probability someone tests positive
#' given they are a user. More specifically, what is the
#' probability for a true positive.
#' @param tubar The probability somenoe tests positive given
#' that they aren't a user. More specifically, what is the
#' probability someone gets a false positive when they aren't
#' a user.
#'
#' @title Run Baye's testing theorem on given probabilities.
#' @return The probability someone is a user given they test
#' positive.
#' @export
#'
#' @examples
#' pct <- get_bayes_testing(3, 95, 30)
get_bayes_testing <- function(u, tu, tubar) {
  ubar <- 1 - u
  return((u * tu) / ((u * tu) + (ubar * tubar)))
}

#' @param a The probability of event A happening.
#' @param b The probability of event B happening.
#' @param ba The probability of event B given event A.
#' 
#' @title Returns the probability of event A given B with Baye's
#' rule.
#' @return A given B.
#' @export
#' 
#' @examples
#' p <- get_bayes(0.75, 0.6, 0.25)
get_bayes <- function(a, b, ba) {
  return((ba * a) / b)
}
