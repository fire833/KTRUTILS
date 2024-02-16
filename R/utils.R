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
  (vector - mean(vector)) / stats::sd(vector)
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
  sum((vector - mean(vector))^2 / length(vector) - 1)
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
  sum((y - mean(y))^2)
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
  sum((y - yhat)^2)
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
  sum((yhat - mean(y))^2)
}
