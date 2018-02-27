library(testthat)

test.pairs.execution = function() {
  #------------------------------------------------------------------------------
  # Testing clip.count

  # Residual of 0
  expect_equal(0, clip.count(0, c(2)))
  expect_equal(0, clip.count(0, c(2,3)))

  # Positive residual
  expect_equal(0, clip.count(1, c(2)))
  expect_equal(-1, clip.count(2, c(2)))

  # Positive residual
  expect_equal(0, clip.count(1, c(2,3)))
  expect_equal(-1, clip.count(2, c(2,3)))
  expect_equal(-2, clip.count(3, c(2,3)))
  expect_equal(-2, clip.count(4, c(2,3)))

  # Negative residual
  expect_equal(0, clip.count(-1, c(2)))
  expect_equal(1, clip.count(-2, c(2)))

  # Positive residual
  expect_equal(0, clip.count(-1, c(2,3)))
  expect_equal(1, clip.count(-2, c(2,3)))
  expect_equal(2, clip.count(-3, c(2,3)))
  expect_equal(2, clip.count(-4, c(2,3)))

  #------------------------------------------------------------------------------
  # Testing short.entry.threshold

  # Clip count of 0
  expect_equal(2, short.entry.threshold(0, c(2)))
  expect_equal(2, short.entry.threshold(0, c(2, 3)))

  # Max clip count
  expect_equal(NA, short.entry.threshold(-1, c(2)))
  expect_equal(NA, short.entry.threshold(-2, c(2, 3)))

  # Clip count of -1
  expect_equal(3, short.entry.threshold(-1, c(2, 3)))
  expect_equal(3, short.entry.threshold(-1, c(2, 3, 4)))

  # Clip count of -2
  expect_equal(4, short.entry.threshold(-2, c(2, 3, 4)))

  # Clip count of 1
  expect_equal(0, short.entry.threshold(1, c(2)))
  expect_equal(0, short.entry.threshold(1, c(2, 3)))

  # Clip count of 2
  expect_equal(-2, short.entry.threshold(2, c(2, 3)))
  expect_equal(-2, short.entry.threshold(2, c(2, 3, 4)))

  # Clip count of 3
  expect_equal(-5, short.entry.threshold(3, c(2, 5, 8)))

  #------------------------------------------------------------------------------
  # Testing long.entry.threshold

  # Clip count of 0
  expect_equal(-2, long.entry.threshold(0, c(2)))
  expect_equal(-2, long.entry.threshold(0, c(2, 3)))

  # Max clip count
  expect_equal(NA, long.entry.threshold(1, c(2)))
  expect_equal(NA, long.entry.threshold(2, c(2, 3)))

  # Clip count of 1
  expect_equal(-3, long.entry.threshold(1, c(2, 3)))
  expect_equal(-3, long.entry.threshold(1, c(2, 3, 4)))

  # Clip count of 2
  expect_equal(-4, long.entry.threshold(2, c(2, 3, 4)))

  # Clip count of -1
  expect_equal(0, long.entry.threshold(-1, c(2)))
  expect_equal(0, long.entry.threshold(-1, c(2, 3)))

  # Clip count of -2
  expect_equal(5, long.entry.threshold(-2, c(5, 7)))
  expect_equal(5, long.entry.threshold(-2, c(5, 7, 10)))

  # Clip count of -3
  expect_equal(7, long.entry.threshold(-3, c(5, 7, 10)))

  #------------------------------------------------------------------------------
  # Testing stdev.thresholds
  expect_equal(matrix(c(2, 2.1, 1.9, 0.2), nrow = 4, ncol = 1), stdev.thresholds(c(2, 2.1, 1.9, 0.2), max_clip_count = 1))

  expected_result = matrix(c(2, 2.1, 1.9, 0.2, 4, 4.2, 3.8, 0.4, 6, 6.3, 5.7, 0.6), nrow = 4, ncol = 3)
  expect_equal(expected_result, stdev.thresholds(c(2, 2.1, 1.9, 0.2), max_clip_count = 3))

  #------------------------------------------------------------------------------
  # Testing execution

  residual = c(1, 2, 2.1, 1.9, 2.1, 0, -1, -2, -2.1, -1.9, -2.1, 0, 4, -6, -4, -2, 0)
  N_residuals = length(residual)

  start_idx = 1
  time = 1:N_residuals
  hedge_ratio = 2*c(1:N_residuals)
  x = 1:N_residuals
  y = 1:N_residuals

  pairs_data = data.frame(time=time, residual = residual, slope=hedge_ratio, x=x, y=y)

  test.expectation = function(execution, expected_position, expected_position_delta, expected_hedge_ratio, expected_pnl) {

    expect_equal(length(expected_position), N_residuals)
    expect_equal(length(expected_position_delta), N_residuals)

    expect_equal(names(execution), c("time", "residual", "position", "position_delta", "hedge_ratio", "x_price", "y_price", "pnl", "pnl_delta"))
    expect_equal(execution$time, time)
    expect_equal(execution$residual, residual)
    expect_equal(execution$position, expected_position)
    expect_equal(execution$position_delta, expected_position_delta)
    expect_equal(execution$hedge_ratio, expected_hedge_ratio)
    expect_equal(execution$x_price, x)
    expect_equal(execution$y_price, y)
    expect_equal(execution$pnl, expected_pnl)
    expect_equal(execution$pnl_delta, c(0, diff(expected_pnl)))
  }

  expected_hedge_ratio = hedge_ratio

  #------------------------------------------------------------------------------
  # bandwidth = 2, max_clip_count = 1

  execution = execute.pairs.strategy(pairs_data, thresholds = bandwidth.thresholds(bandwidth = 2, max_clip_count = 1, N=N_residuals))

  expected_position =       c(0, -1, -1, -1, -1,  0,  0,  1,  1,   1,   1,   0,  -1,   1,   1,   1,   0)
  expected_position_delta = c(0, -1,  0,  0,  0,  1,  0,  1,  0,   0,   0,  -1,  -1,   2,   0,   0,  -1)
  expected_pnl =            c(0,  0,  3,  8, 15, 24, 24, 24,  9,  -8, -27, -48, -48, -23, -50, -79,-110)

  test.expectation(execution, expected_position, expected_position_delta, expected_hedge_ratio, expected_pnl)

  # ------------------------------------------------------------------------------
  # bandwidth = 2, max_clip_count = 5
  execution = execute.pairs.strategy(pairs_data, thresholds = bandwidth.thresholds(bandwidth = 2, max_clip_count = 5, N=N_residuals))

  expected_position =       c(0, -1/5, -1/5, -1/5, -1/5,    0,    0,  1/5,  1/5,   1/5,   1/5,     0,  -2/5,  3/5,   2/5,    1/5,      0)
  expected_position_delta = c(0, -1/5,    0,    0,    0,  1/5,    0,  1/5,    0,     0,     0,  -1/5,  -2/5,  5/5,  -1/5,   -1/5,   -1/5)
  expected_pnl =            c(0,  0,    3/5,  8/5, 15/5, 24/5, 24/5, 24/5,  9/5,  -8/5, -27/5, -48/5, -48/5,  2/5, -79/5, -137/5, -168/5)

  test.expectation(execution, expected_position, expected_position_delta, expected_hedge_ratio, expected_pnl)

  #------------------------------------------------------------------------------
  # bandwidth = 1, max_clip_count = 10
  execution = execute.pairs.strategy(pairs_data, thresholds = bandwidth.thresholds(bandwidth = 1, max_clip_count = 10, N=N_residuals))

  expected_position =       c(-0.1, -0.2, -0.2, -0.2, -0.2,   0, 0.1,  0.2, 0.2,  0.2,  0.2,     0,  -0.4,  0.6,   0.4,   0.2,    0)
  expected_position_delta = c(-0.1, -0.1,    0,    0,    0, 0.2, 0.1,  0.1,   0,    0,    0,  -0.2,  -0.4,  1.0,  -0.2,  -0.2,  -0.2)
  expected_pnl =            c(   0,  0.1,  0.7,  1.7,  3.1, 4.9, 4.9,  3.6, 0.6, -2.8, -6.6, -10.8, -10.8, -0.8, -17.0, -28.6, -34.8)

  test.expectation(execution, expected_position, expected_position_delta, expected_hedge_ratio, expected_pnl)
}

test.pairs.execution()
