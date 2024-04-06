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

#' @param n provide the number of seats available on a flight.
#' @param gamma provide the probability of overbooking a flight.
#' @param p provide the probability of someone showing up for a flight.
#'
#' @title Predict the number of tickets you are able to sell given the
#' number of seats available and the probability of overbooking/someone
#' actually showing up. Utilizes a discrete binomial distribution under
#' the hood.
#' @return list of solution, N, gamma, and p
ntickets_discrete <- function(n, gamma, p) {
  obj <- function(x) {
    return(1 - gamma - stats::pbinom(n, x, p))
  }

  ind <- seq(n, n * 1.11, 1)
  dep <- obj(ind)
  sol <- ind[which.min(abs(dep))]

  plot(dep ~ ind,
    type = "b", pch = 20,
    bg = "blue", ylim = c(0, 1),
    ylab = "Objective", xlab = "n",
    main = sprintf("Objective v. N Optimal Tickets to Sell
(%d) gamma = %.2f N = %d Discrete", sol, gamma, n)
  )
  graphics::abline(v = sol, h = 0, col = "red")
  return(list(n = sol, N = n, gamma = gamma, p = p))
}

#' @param n provide the number of seats available on a flight.
#' @param gamma provide the probability of overbooking a flight.
#' @param p provide the probability of someone showing up for a flight.
#'
#' @title Predict the number of tickets you are able to sell given the
#' number of seats available and the probability of overbooking/someone
#' actually showing up. Utilizes a continuous normal distribution approximation
#' under the hood.
#' @return list of solution, N, gamma, and p
ntickets_continuous <- function(n, gamma, p) {
  q <- 1 - p
  obj <- function(x) {
    return(1 - gamma - stats::pnorm(n + 0.5, x * p, sqrt(x * p * q)))
  }

  sol <- stats::uniroot(obj, interval = c(n, n * 1.11))
  graphics::curve(obj, from = n, to = n * 1.11, xlab = "n", ylab = "Objective")
  graphics::title(sprintf("Objective v. N Optimal Tickets to Sell
(%.4f) gamma = %.2f N = %d Continuous", sol$root, gamma, n))
  graphics::abline(v = sol, h = 0, col = "red")
  return(list(n = sol$root, N = n, gamma = gamma, p = p))
}

#' @param n provide the number of seats available on a flight.
#' @param gamma provide the probability of overbooking a flight.
#' @param p provide the probability of someone showing up for a flight.
#'
#' @title Predict number of tickets to sell for a flight
#' @description Predict the number of tickets you are able to sell given the
#' number of seats available and the probability of overbooking/someone
#' actually showing up. Utilizes both binomial and continuous normal
#' distributions for predicting natural and real numbers for the
#' number of tickets to sell.
#' @return list of solution, N, gamma, and p
#' @export
#'
#' @examples
#' ntickets()
#' ntickets(n = 400)
ntickets <- function(n = 200, gamma = 0.02, p = 0.95) {
  graphics::layout(matrix(1:2, nrow = 2, ncol = 1, byrow = TRUE))
  disc <- ntickets_discrete(n, gamma, p)
  cts <- ntickets_continuous(n, gamma, p)
  return(list(nd = disc$n, nc = cts$n, N = n, gamma = gamma, p = p))
}