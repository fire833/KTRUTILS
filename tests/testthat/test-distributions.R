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

library(testthat)

test_that("get_norm_curve is correct 1", {
  vals <- get_norm_curve(val = 11)
  expect_equal(vals, list(mu = 0, sigma = 10, area = pnorm(11, 0, 10)))
})

test_that("get_norm_curve is correct 2", {
  vals <- get_norm_curve(0, 4, val = 6)
  expect_equal(vals, list(mu = 0, sigma = 4, area = pnorm(6, 0, 4)))
})

test_that("get_norm_curve is correct 3", {
  vals <- get_norm_curve(5, 4, val = 6)
  expect_equal(vals, list(mu = 5, sigma = 4, area = pnorm(6, 5, 4)))
})

test_that("get_norm_curve is correct 4", {
  vals <- get_norm_curve(3, 3, val = 6)
  expect_equal(vals, list(mu = 3, sigma = 3, area = pnorm(6, 3, 3)))
})

test_that("get_norm_curve is correct 5", {
  vals <- get_norm_curve(7, 7, val = 10)
  expect_equal(vals, list(mu = 7, sigma = 7, area = pnorm(10, 7, 7)))
})
