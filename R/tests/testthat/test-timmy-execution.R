library(testthat)

test.timmy.execution = function() {

  #' Execute timmy. The result of kalman.regression is used to generate
  #' buy and sell signal.
  #'
  #' @export execute.timmy
  execute.timmy = function(kalman_regression, bandwidth, max_clip_count=1) {
    N = nrow(kalman_regression)
    stopifnot(N > 0)

    residual = rep(NA, N)
    clip_count = rep(NA, N)
    clip_count_delta = rep(NA, N)
    position = rep(NA, N)
    position_delta = rep(NA, N)
    hedge_ratio = rep(NA, N)
    x_price = rep(NA, N)
    y_price = rep(NA, N)
    short_threshold = rep(NA, N)
    long_threshold = rep(NA, N)

    date = kalman_regression$date

    short_threshold[1] = bandwidth
    long_threshold[1] = -bandwidth

    prev_clip_count = 0
    prev_position = 0
    for (i in 1:N) {
      residual[i] = kalman_regression$residuals[i]
      hedge_ratio[i] = kalman_regression$slope.mean[i]
      x_price[i] = kalman_regression$x[i]
      y_price[i]= kalman_regression$y[i]
      if (residual[i] >= short_threshold[i]) {
        clip_count[i] = -trunc(residual[i]/bandwidth)
        if (clip_count[i] < -max_clip_count) {
          clip_count[i] = -max_clip_count
        }
      } else if (residual[i] <= long_threshold[i]) {
        clip_count[i] = -trunc(residual[i]/bandwidth)
        if (clip_count[i] > max_clip_count) {
          clip_count[i] = max_clip_count
        }
      } else {
        clip_count[i] = prev_clip_count
      }
      clip_count_delta[i] = clip_count[i] - prev_clip_count
      position[i] = clip_count[i]/max_clip_count
      position_delta[i] = position[i] - prev_position
      if (i < N) {
        short_threshold[i+1] = short_threshold[i] - clip_count_delta[i]*bandwidth
        long_threshold[i+1] = long_threshold[i] - clip_count_delta[i]*bandwidth
      }
      prev_clip_count = clip_count[i]
      prev_position = position[i]
    }

    result = data.frame(date=date, residual=residual, position=position, position_delta=position_delta, hedge_ratio=hedge_ratio, x_price=x_price, y_price=y_price)
    rownames(result) = date

    attr(result, "pairs.execution.kalman.regression") = kalman_regression
    attr(result, "pairs.execution.bandwidth") = bandwidth
    attr(result, "pairs.execution.max.clip.count") = max_clip_count

    return(result)
  }

  residuals = c(1, 2, 2.1, 1.9, 2.1, 0, -1, -2, -2.1, -1.9, -2.1, 0, 4, -6, -4, -2, 0)
  N = length(residuals)

  start_idx = 1
  date = c(1:N)
  slope = data.frame(mean=2*c(1:N), var=rep(1,N))
  x = c(1:N)
  y = c(1:N)

  regression_model = data.frame(date=date, residuals = residuals, slope=slope, x=x, y=y)

  test.expectation = function(execution, expected_position, expected_position_delta, expected_hedge_ratio) {

    expect_equal(length(expected_position), N)
    expect_equal(length(expected_position_delta), N)

    expect_equal(names(execution), c("date", "residual", "position", "position_delta", "hedge_ratio", "x_price", "y_price"))
    expect_equal(execution$date, date)
    expect_equal(execution$residual, residuals)
    expect_equal(execution$position, expected_position)
    expect_equal(execution$position_delta, expected_position_delta)
    expect_equal(execution$hedge_ratio, expected_hedge_ratio)
    expect_equal(execution$x_price, x)
    expect_equal(execution$y_price, y)
  }

  expected_hedge_ratio = slope$mean

  #------------------------------------------------------------------------------
  # bandwidth = 2, max_clip_count = 1
  execution = execute.timmy(regression_model, bandwidth = 2, max_clip_count = 1)

  expected_position =       c(0, -1, -1, -1, -1, 0, 0, 1, 1, 1, 1,  0, -1, 1,  1,  1,  0)
  expected_position_delta = c(0, -1,  0,  0,  0, 1, 0, 1, 0, 0, 0, -1, -1, 2,  0,  0, -1)

  # test.expectation(execution, expected_position, expected_position_delta, expected_hedge_ratio)
  #
  # # ------------------------------------------------------------------------------
  # # bandwidth = 2, max_clip_count = 5
  # execution = execute.timmy(regression_model, bandwidth = 2, max_clip_count = 5)
  #
  # expected_position =       c(0, -1/5, -1/5, -1/5, -1/5,   0, 0, 1/5, 1/5, 1/5, 1/5,    0, -2/5, 3/5,  2/5,  1/5,  0)
  # expected_position_delta = c(0, -1/5,    0,    0,    0, 1/5, 0, 1/5,   0,   0,   0, -1/5, -2/5, 5/5, -1/5, -1/5, -1/5)
  #
  # test.expectation(execution, expected_position, expected_position_delta, expected_hedge_ratio)
  #
  # #------------------------------------------------------------------------------
  # # bandwidth = 1, max_clip_count = 10
  # execution = execute.timmy(regression_model, bandwidth = 1, max_clip_count = 10)
  #
  # expected_position =       c(-0.1, -0.2, -0.2, -0.2, -0.2, 0, 0.1, 0.2, 0.2, 0.2, 0.2,  0, -0.4,  0.6,  0.4,  0.2,  0)
  # expected_position_delta = c(-0.1, -0.1,  0,  0,  0, 0.2, 0.1, 0.1, 0, 0, 0, -0.2, -0.4, 1.0, -0.2, -0.2, -0.2)
  #
  # test.expectation(execution, expected_position, expected_position_delta, expected_hedge_ratio)
}

test.timmy.execution()
