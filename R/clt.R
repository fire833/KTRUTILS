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

#' @param iter provide the number of iterations to experiment with.
#' @param n provide the size of the sample you want per iteration.
#' @param a provide the lower bound for samples.
#' @param b provide the upper bound for samples.
#'
#' @title Run a uniform distribution simulation of n samples iter times.
#' Print out the resulting histogram superimposed with a theoretical
#' normal curve and corresponding density curve.
#' @return nothing, just print out resulting histogram.
#' @export
#'
#' @examples
#' clt_unif(n = 5, iter = 10000, a = 0, b = 5)
clt_unif <- function(n, iter, a = 0, b = 10) {
  # r-random sample from the uniform
  y <- stats::runif(n * iter, a, b)
  # Place these numbers into a matrix
  # The columns will correspond to the iteration and the rows
  # will equal the sample size n
  data <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  # apply the function mean to the columns (2) of the matrix
  # these are placed in a vector w
  w <- apply(data, 2, mean)
  # We will make a histogram of the values in w
  # How high should we make y axis?
  # All the values used to make a histogram are
  # placed in param (nothing is plotted yet)
  param <- graphics::hist(w, plot = FALSE)
  # Since the histogram will be a density plot we will find the max density

  ymax <- max(param$density)
  # To be on the safe side we will add 10% more to this
  ymax <- 1.1 * ymax
  # Now we can make the histogram
  graphics::hist(w, freq = FALSE, ylim = c(0, ymax), main = paste(
    "histogram of sample mean",
    "\n", "sample size= ", n,
    sep = ""
  ), xlab = "Sample mean")
  # add a density curve made from the sample distribution
  graphics::lines(stats::density(w), col = "Blue", lwd = 3) # add a density plot
  # Add a theoretical normal curve
  x <- NULL
  graphics::curve(
    stats::dnorm(x, mean = (a + b) / 2, sd = (b - a) / (sqrt(12 * n))),
    add = TRUE, col = "Red", lty = 2, lwd = 3
  ) # add a theoretical curve
  # Add the density from which the samples were taken
  graphics::curve(stats::dunif(x, a, b), add = TRUE, lwd = 4)
}

#' @param iter provide the number of iterations to experiment with.
#' @param n provide the size of the sample you want per iteration.
#' @param p the probability for success for any given sample.
#'
#' @title Run a binomial distribution simulation of n samples iter times.
#' Print out the resulting histogram superimposed with a theoretical
#' normal curve and corresponding density curve.
#' @return nothing, just print out resulting histogram.
#' @export
#'
#' @examples
#' clt_binom(n = 5, iter = 10000, p = 0.7)
clt_binom <- function(n, iter, p = 0.5) {
  # r-random sample from the Binomial
  y <- stats::rbinom(n * iter, size = n, prob = p)
  # Place these numbers into a matrix
  # The columns will correspond to the iteration
  # and the rows will equal the sample size n
  data <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  # apply the function mean to the columns (2) of the matrix
  # these are placed in a vector w
  w <- apply(data, 2, mean)
  # We will make a histogram of the values in w
  # How high should we make y axis?
  # All the values used to make a histogram are placed
  # in param (nothing is plotted yet)
  param <- graphics::hist(w, plot = FALSE)
  # Since the histogram will be a density plot we will find the max density

  ymax <- max(param$density)
  # To be on the safe side we will add 10% more to this
  ymax <- 1.1 * ymax

  # Now we can make the histogram
  # freq=FALSE means take a density
  graphics::hist(w,
    freq = FALSE, ylim = c(0, ymax),
    main = paste("histogram of sample mean", "\n",
      "sample size= ", n,
      sep = ""
    ),
    xlab = "Sample mean"
  )
  # add a density curve made from the sample distribution
  # Add a theoretical normal curve
  x <- NULL
  graphics::curve(stats::dnorm(x, mean = n * p, sd = sqrt(p * (1 - p))),
    add = TRUE, col = "Red", lty = 2, lwd = 3
  )
}

#' @param iter provide the number of iterations to experiment with.
#' @param n provide the size of the sample you want per iteration.
#' @param lambda provide the average number of successes for the
#' underlying Poisson distribution samples.
#'
#' @title Run a poisson distribution simulation of n samples iter times.
#' Print out the resulting histogram superimposed with a theoretical
#' normal curve and corresponding density curve.
#' @return nothing, just print out resulting histogram.
#' @export
#'
#' @examples
#' clt_pois(n = 5, iter = 10000, lambda = 5)
clt_pois <- function(n, iter, lambda = 10) {
  # r-random sample from the Poisson
  y <- stats::rpois(n * iter, lambda = lambda)
  # Place these numbers into a matrix
  # The columns will correspond to the iteration
  # and the rows will equal the sample size n
  data <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  # apply the function mean to the columns (2) of the matrix
  # these are placed in a vector w
  w <- apply(data, 2, mean)
  # We will make a histogram of the values in w
  # How high should we make y axis?
  # All the values used to make a histogram are
  #  placed in param (nothing is plotted yet)
  param <- graphics::hist(w, plot = FALSE)
  # Since the histogram will be a density plot we will find the max density

  ymax <- max(param$density)
  # To be on the safe side we will add 10% more to this
  ymax <- 1.1 * ymax

  # Make a suitable layout for graphing
  graphics::layout(matrix(c(1, 1, 2, 3), nrow = 2, ncol = 2, byrow = TRUE))

  ## Now we can make the graphics::histogram
  graphics::hist(w,
    freq = FALSE, ylim = c(0, ymax), col = grDevices::rainbow(max(w)),
    main = paste("graphics::histogram of sample mean", "\n",
      "sample size= ", n, " iter=", iter, " lambda=", lambda,
      sep = ""
    ),
    xlab = "Sample mean"
  )
  # add a density curve made from the sample distribution
  # Add a theoretical normal curve
  graphics::curve(stats::dnorm(x, mean = lambda, sd = sqrt(lambda / n)),
    add = TRUE, col = "Red", lty = 2, lwd = 3
  ) # add a theoretical curve

  # Now make a new plot
  # Since y is discrete we should use a barplot
  graphics::barplot(table(y) / (n * iter),
    col = grDevices::rainbow(max(y)),
    main = "Barplot of sampled y", ylab = "Rel. Freq", xlab = "y"
  )
  x <- 0:max(y)
  plot(x, stats::dpois(x, lambda = lambda),
    type = "h", lwd = 5, col = grDevices::rainbow(max(y)),
    main = "Probability function for Poisson",
    ylab = "Probability", xlab = "y"
  )
}
