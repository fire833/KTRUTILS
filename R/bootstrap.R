# Copyright (C) 2025 Kendall Tauser
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
#' @param x provide the population o sample from.
#' @param fun provide the function you want to apply to your sample.
#' @param alpha provide the confidence interval you want to get back.
#'
#' @title Bootstrap a confidence interval from a set of data.
#' @return list of useful items.
#' @export
#'
#' @examples
#' bootstrap2(10000, c(1,2,3,4,5), fun = "mean", alpha = 0.2)
bootstrap2 <- function(iter = 10000, x, fun = "mean", alpha = 0.05) {
  # Notice where the ... is repeated in the code
  n <- length(x) # sample size

  # Now sample with replacement
  y <- sample(x, n * iter, replace = TRUE) # A

  # Make a matrix with all the resampled values
  rs_mat <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)

  xstat <- apply(rs_mat, 2, fun)

  # xstat is a vector and will have iter values in it
  ci <- stats::quantile(xstat, c(alpha / 2, 1 - alpha / 2)) # B

  # Nice way to form a confidence interval
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para <- graphics::hist(xstat,
    freq = FALSE, las = 1,
    main = "Histogram of Bootstrap sample statistics"
  )

  # mat will be a matrix that contains the data, this is done
  # so that I can use apply()
  mat <- matrix(x, nrow = length(x), ncol = 1, byrow = TRUE)

  # pte is the point estimate
  # This uses whatever fun is
  pte <- apply(mat, 2, fun)
  graphics::abline(v = pte, lwd = 3, col = "Black") # Vertical line
  graphics::segments(ci[1], 0, ci[2], 0, lwd = 4) # Make the segment for the ci
  graphics::text(ci[1], 0, paste("(", round(ci[1], 2), sep = ""), col = "Red", cex = 3)
  graphics::text(ci[2], 0, paste(round(ci[2], 2), ")", sep = ""), col = "Red", cex = 3)
  # plot the point estimate 1/2 way up the density
  graphics::text(pte, max(para$density) / 2, round(pte, 2), cex = 3)

  # Some output to use if necessary
  return(list(ci = ci, fun = fun, x = x, xstat = xstat))
}