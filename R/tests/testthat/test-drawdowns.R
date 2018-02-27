library(testthat)

test.drawdowns = function() {

  #------------------------------------------------------------------------------
  # drawdowns

  expect_equal(drawdowns(c(0)), c(0))
  expect_equal(drawdowns(c(0, 1)), c(0, 0))
  expect_equal(drawdowns(c(0, 1, 1)), c(0, 0, 0))
  expect_equal(drawdowns(c(0, 1, 0)), c(0, 0, -1))
  expect_equal(drawdowns(c(0, 1, 0, -1)), c(0, 0, -1, -2))
  expect_equal(drawdowns(c(0, 1, 0, -1, 1)), c(0, 0, -1, -2, 0))
  expect_equal(drawdowns(c(0, 1, 0, -1, 2)), c(0, 0, -1, -2, 0))
  expect_equal(drawdowns(c(0, 1, 0, -1, 1, 2)), c(0, 0, -1, -2, 0, 0))
  expect_equal(drawdowns(c(0, 1, 0, -1, 1, 2, 0)), c(0, 0, -1, -2, 0, 0, -2))
  expect_equal(drawdowns(c(0, 1, 0, -1, 1, 2, 0, 1, -1)), c(0, 0, -1, -2, 0, 0, -2, -1, -3))
  expect_equal(drawdowns(c(0, 1, 0, -1, 1, 2, 0, 1, -1, 3, 0, 2)), c(0, 0, -1, -2, 0, 0, -2, -1, -3, 0, -3, -1))

  expect_equal(drawdowns(c(-1)), c(-1))
  expect_equal(drawdowns(c(-1,0)), c(-1,0))

  #------------------------------------------------------------------------------
  # worst.drawdown
  expect_equal(worst.drawdown(c(0), percentage=FALSE), 0)
  expect_equal(worst.drawdown(c(-1), percentage=FALSE), -1)

  expect_equal(worst.drawdown(c(0, 1), percentage=FALSE), 0)
  expect_equal(worst.drawdown(c(-1, 1), percentage=FALSE), -1)

  expect_equal(worst.drawdown(c(0, 1, 1), percentage=FALSE), 0)
  expect_equal(worst.drawdown(c(-1, 1, 1), percentage=FALSE), -1)

  expect_equal(worst.drawdown(c(0, 1, 0), percentage=FALSE), -1)
  expect_equal(worst.drawdown(c(-2, 1, 0), percentage=FALSE), -2)

  expect_equal(worst.drawdown(c(0, 1, 0, -1), percentage=FALSE), -2)
  expect_equal(worst.drawdown(c(-1, -2, 0, -1), percentage=FALSE), -2)

  expect_equal(worst.drawdown(c(0, 1, 0, -1, 1), percentage=FALSE), -2)
  expect_equal(worst.drawdown(c(0, 1, 0, -1, 2), percentage=FALSE), -2)
  expect_equal(worst.drawdown(c(0, 1, 0, -1, 1, 2), percentage=FALSE), -2)
  expect_equal(worst.drawdown(c(0, 1, 0, -1, 1, 2, 0), percentage=FALSE), -2)
  expect_equal(worst.drawdown(c(0, 1, 0, -1, 1, 2, 0, 1, -1), percentage=FALSE), -3)

  expect_equal(worst.drawdown(c(0, 1, 0, -1, 1, 2, 0, 1, -1, 3, 0, 2), percentage=FALSE), -3)
  expect_equal(worst.drawdown(c(-4, 1, 0, -1, 1, 2, 0, 1, -1, 3, 0, 2), percentage=FALSE), -4)

  #------------------------------------------------------------------------------
  # worst.drawdown percentage
  expect_equal(worst.drawdown(c(100), percentage = TRUE), 0)

  # Absolute
  expect_equal(worst.drawdown(c(100, 99), percentage=FALSE), -1)
  # Percentage
  expect_equal(worst.drawdown(c(100, 99), percentage = TRUE), -0.01)

  # Absolute
  expect_equal(worst.drawdown(c(100, 99, 98), percentage=FALSE), -2)
  # Percentage
  expect_equal(worst.drawdown(c(100, 99, 98), percentage = TRUE), -0.02)

  # Absolute
  expect_equal(worst.drawdown(c(100, 99, 98, 200, 199, 250), percentage=FALSE), -2)
  # Percentage
  expect_equal(worst.drawdown(c(100, 99, 98, 200, 199, 250), percentage = TRUE), -0.02)
}

test.drawdowns()
