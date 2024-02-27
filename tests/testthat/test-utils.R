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

test_that("z score returns correct size and values", {
  v <- c(1, 2, 5, 5, 3)
  z <- ktrutils::get_z(v)
  expect_length(z, 5)
  expect_equal(z, c(-1.2298374, -0.6708204, 1.0062306, 1.0062306, -0.1118034))
})
