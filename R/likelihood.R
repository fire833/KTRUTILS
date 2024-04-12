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

#' @param theta1 provide the vector of theta1 values to find max from.
#' @param theta2 provide the vector of theta2 values to find max from.
#' @param lfun provide the function to get max likelihood based on
#' parameters.
#' @param ... provide additional arguments to contour
#'
#' @title Create a maximum likelihood simulation
#' @description Used for a maximum likelihood simulation optimization 
#' for two variables given a particular likelihood function.
#' @return a plot to the current device, and the estimation for theta1
#' and theta2.
#' @export
#'
#' @examples
#' demofunc <- function(theta1, theta2) log(dbinom(10, 20, theta1)*dpois(5, theta2))
#' max_likelihood_g2(seq(0, 1, length = 1000), seq(0, 1, length = 1000), lfun = demofunc)
max_likelihood_g2 <- function(theta1, theta2, lfun, ...) {
    n1 <- length(theta1)
    n2 <- length(theta2)
    z <- outer(theta1, theta2, lfun)
    graphics::contour(theta1, theta2, exp(z), levels = 25, ...) # exp(z) gives the lik
    maxl <- max(exp(z)) # max lik
    coord <- which(exp(z) == maxl, arr.ind = TRUE) # find the co-ords of the max
    th1est <- theta1[coord[1]] # mxlik estimate of theta1
    th2est <- theta2[coord[2]]
    graphics::abline(v = th1est, h = th2est)
    graphics::axis(3, th1est, round(th1est, 2))
    graphics::axis(4, th2est, round(th2est, 2), las = 1)
    return(list(th1est = th1est, th2est = th2est))
}
