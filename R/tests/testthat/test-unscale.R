library(testthat)

test.unscale = function() {

  #------------------------------------------------------------------------------
  # 1-D data
  data = matrix(c(1,2,3,4,0,-7,6,2), ncol=1)

  scaled_data = scale(data)

  expect_equal(length(attr(scaled_data, "scaled:center")), 1)
  expect_equal(length(attr(scaled_data, "scaled:scale")), 1)
  expect_equal(attr(scaled_data, "scaled:center"), mean(data))
  expect_equal(attr(scaled_data, "scaled:scale"), sd(data))
  unscaled_data = unscale(scaled_data)

  expect_equal(unscaled_data, data)

  #------------------------------------------------------------------------------
  # 2-D data - example 1
  data = matrix(c(1,2,3,5), nrow=2, ncol=2)

  scaled_data = scale(data)

  expect_equal(length(attr(scaled_data, "scaled:center")), 2)
  expect_equal(length(attr(scaled_data, "scaled:scale")), 2)
  expect_equal(attr(scaled_data, "scaled:center"), c(1.5, 4))
  expect_equal(attr(scaled_data, "scaled:scale"), c(0.70710678, 1.414213562373))
  unscaled_data = unscale(scaled_data)

  expect_equal(unscaled_data, data)

  # Explicit scaling parameters
  unscaled_data_2 = unscale(scaled_data, c(1.5, 4), c(0.70710678, 1.414213562373))
  expect_equal(unscaled_data_2, data)

  #------------------------------------------------------------------------------
  # 2-D data - example 2
  data = matrix(c(1,2,3,4,0,-7,6,2), nrow=4, ncol=2)

  scaled_data = scale(data)

  expect_equal(length(attr(scaled_data, "scaled:center")), 2)
  expect_equal(length(attr(scaled_data, "scaled:scale")), 2)
  expect_equal(attr(scaled_data, "scaled:center"), apply(data, 2, mean))
  expect_equal(attr(scaled_data, "scaled:scale"), apply(data, 2, sd))
  unscaled_data = unscale(scaled_data)

  expect_equal(unscaled_data, data)

}

test.unscale()
