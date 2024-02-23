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

#' @param iter provide the number of iterations you wish to run in your
#' simulation.
#' @param n provide the size of the sample you want per iteration.
#' @param prob provide the probability of 1 occurring in your simulation.
#'
#' @title Run a binomial simulation with a specific sample sieze, probability,
#' and number of iterations.
#' @return nothing, just print out a barplot and the resulting tables of
#' the simulation.
#' @export
#'
#' @examples
#' binomial_sim(iter = 10000, n = 5, prob = 0.2)
#' binomial_sim(iter = 500, n = 10, prob = 0.8)
binomial_sim <- function(iter = 100, n = 10, prob = 0.7) {
  # make a matrix to hold the samples
  # initially filled with NA's
  sim_mat <- matrix(NA, nrow = n, ncol = iter, byrow = TRUE)
  # Make a vector to hold the number of successes in each trial
  successes <- c()
  for (i in 1:iter) {
    # Fill each column with a new sample
    sim_mat[, i] <- sample(c(1, 0), n, replace = TRUE, prob = c(prob, 1 - prob))
    # Calculate a statistic from the sample (this case it is the sum)
    successes[i] <- sum(sim_mat[, i])
  }
  # Make a table of successes
  successes_table <- table(factor(successes, levels = 0:n))
  # Make a barplot of the proportions
  graphics::barplot(successes_table / (iter),
    col = grDevices::rainbow(n + 1),
    main = "Binomial simulation", xlab = "Number of successes"
  )
  print(successes_table / iter)
  print(successes_table)
}

#' @param iter provide the number of iterations you wish to run in your
#' simulation.
#' @param N provide the size of the sample you will sample from.
#' @param r provide the number of items in the sample that will be red or
#' false, what will the binary split in the sample be?
#' @param n Provide the sampled size for every iteration.
#'
#' @title Run a hypergeometric simulation with a fixed bag of elements.
#' @return nothing, just print out a barplot and the resulting tables of
#' the simulation.
#' @export
#'
#' @examples
#' hyper_sim(iter = 10000, N = 20, r = 10, n = 6)
#' hyper_sim(iter = 500, N = 30, r = 5, n = 10)
hyper_sim <- function(iter = 100, N = 20, r = 12, n = 5) {
  # make a matrix to hold the samples
  # initially filled with NA's
  sim_mat <- matrix(NA, nrow = n, ncol = iter, byrow = TRUE)
  # Make a vector to hold the number of successes over the trials
  successes <- c()
  for (i in 1:iter) {
    # Fill each column with a new sample
    sim_mat[, i] <- sample(rep(c(1, 0), c(r, N - r)), n, replace = FALSE)
    # Calculate a statistic from the sample (this case it is the sum)
    successes[i] <- sum(sim_mat[, i])
  }
  # Make a table of successes
  successes_table <- table(factor(successes, levels = 0:n))
  # Make a barplot of the proportions
  graphics::barplot(successes_table / (iter),
    col = grDevices::rainbow(n + 1),
    main = "HYPERGEOMETRIC simulation", xlab = "Number of successes"
  )
  print(successes_table / iter)
  print(successes_table)
}

# ptauser <- function(y) {
#   if (y < 0) return (0)
#   if (0 <= y <= 2) return ((1/8)*y^3)
#   if (y > 2) return (0)
# }
