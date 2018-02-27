library(testthat)

test.sharpe.ratio = function() {
  actual_sharpe_ratio = sharpe.ratio(c(1,-1,2,1,-3, 3), 0.5)
  expect_equal(0.799, round(actual_sharpe_ratio, 3))

  actual_sharpe_ratio = sharpe.ratio(c(2,-3,4,0,-6,-2), 2)
  expect_equal(-0.401, round(actual_sharpe_ratio, 3))
}

test.sharpe.ratio()
