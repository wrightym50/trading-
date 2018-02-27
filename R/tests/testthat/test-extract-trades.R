library(testthat)

test.extract.trades.1 = function() {

  times    =    c( 11,  12,  13,  14)
  residual =    c(  0,  -1,   1,   0)
  position =    c(  0,   2,  -1,   0)
  x_price =     c(100, 101, 102, 103)
  y_price =     c(  1,   2,   3,   2)
  hedge_ratio = c(  1,   3,   1,   1)
  pnl =         c(  0,   0,  -4,  -6)

  execution = data.frame(
    position=position,
    x_price=x_price,
    y_price=y_price,
    hedge_ratio=hedge_ratio,
    time=times,
    residual=residual,
    pnl=pnl
  )

  trades = extract.trades(execution)

  expect_equal(nrow(trades), 2)

  expect_equal(
    names(trades),
    c(
      "start_idx",
      "end_idx",
      "filtered_end_idx",
      "pnl",
      "filtered_pnl",
      "holding_period",
      "filtered_holding_period",
      "start_time",
      "end_time",
      "side",
      "mae"
    )
  )

  expect_equal(trades$start_idx, c(2, 3))
  expect_equal(trades$end_idx, c(3, 4))
  expect_equal(trades$filtered_end_idx, c(3, 4))
  expect_equal(trades$pnl, c(-4, -2))
  expect_equal(trades$filtered_pnl, c(-4, -2))
  expect_equal(trades$holding_period, c(1, 1))
  expect_equal(trades$filtered_holding_period, c(1, 1))
  expect_equal(trades$start_time, c(12, 13))
  expect_equal(trades$end_time, c(13, 14))
  expect_equal(trades$side, c(1, -1))
}

test.extract.trades.1()

test.extract.trades.2 = function() {

  times    =    c( 11,  12,  13,  14)
  residual =    c(  0,  -1,   1,   0)
  position =    c(  0,   0,   0,   0)
  x_price =     c(100, 101, 102, 103)
  y_price =     c(  1,   2,   3,   2)
  hedge_ratio = c(  1,   3,   1,   1)
  pnl =         c(  0,   0,  -4,  -6)

  execution = data.frame(
    position=position,
    x_price=x_price,
    y_price=y_price,
    hedge_ratio=hedge_ratio,
    time=times,
    residual=residual,
    pnl=pnl
  )

  trades = extract.trades(execution)

  expect_equal(nrow(trades), 0)

  expect_equal(
    names(trades),
    c(
      "start_idx",
      "end_idx",
      "filtered_end_idx",
      "pnl",
      "filtered_pnl",
      "holding_period",
      "filtered_holding_period",
      "start_time",
      "end_time",
      "side",
      "mae"
    )
  )

  expect_equal(trades$start_idx, integer(0))
  expect_equal(trades$end_idx, integer(0))
  expect_equal(trades$filtered_end_idx, integer())
  expect_equal(trades$pnl, integer(0))
  expect_equal(trades$filtered_pnl, integer(0))
  expect_equal(trades$holding_period, integer(0))
  expect_equal(trades$filtered_holding_period, integer(0))
  expect_equal(trades$start_time, integer(0))
  expect_equal(trades$end_time, integer(0))
  expect_equal(trades$side, integer(0))
}

test.extract.trades.2()
