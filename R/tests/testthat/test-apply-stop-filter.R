library(testthat)

test.apply.stop.filter = function() {

  excursions = vector("list", 3)
  trades = data.frame(start_idx=c(1,5,10), end_idx=c(3, 10, 13), holding_period=c(2,5,3), pnl=c(2,8,11))

  excursions[[1]] = data.frame(pnl=c(0, 1, 2), row_label=c(1:3))
  excursions[[2]] = data.frame(pnl=c(0, 4, -2.1, -4.5, -4.9, 8), row_label=c(1:6))
  excursions[[3]] = data.frame(pnl=c(0, 9,10,11), row_label=c(1:4))
  trades$excursion = excursions

  execution = data.frame(pnl=c(0, 1, 2, 2, 2, 6, -0.1, -2.5, -2.9, 10, 19, 20, 21))

  # #------------------------------------------------------------------------------
  # test.apply.holding.period.filter = function() {
  #   filtered_trades = apply.stop.filter(trades, holding.period.filter, 4)
  #
  #   expect_equal(names(trades), c("start_idx", "end_idx", "holding_period", "pnl", "excursion"))
  #   expect_equal(names(filtered_trades), c("start_idx", "end_idx", "holding_period", "pnl", "excursion", "filtered_end_idx", "filtered_pnl", "filtered_holding_period"))
  #   expect_equal(filtered_trades$start_idx, c(1,5,10))
  #
  #   expect_equal(filtered_trades$end_idx, c(3,10,13))
  #   expect_equal(filtered_trades$filtered_end_idx, c(3,9,13))
  #
  #   expect_equal(filtered_trades$pnl, c(2,8,11))
  #   expect_equal(filtered_trades$filtered_pnl, c(2,-4.5,11))
  #
  #   expect_equal(filtered_trades$holding_period, c(2,5,3))
  #   expect_equal(filtered_trades$filtered_holding_period, c(2,4,3))
  # }
  # test.apply.holding.period.filter()

  #------------------------------------------------------------------------------
  test.apply.max.loss.filter.1 = function() {

    # Filter doesn't modify any trades
    filtered_trades = apply.stop.filter(trades, max.loss.filter(execution, 5))

    expect_equal(names(trades), c("start_idx", "end_idx", "holding_period", "pnl", "excursion"))
    expect_equal(names(filtered_trades), c("start_idx", "end_idx", "holding_period", "pnl", "excursion", "filtered_end_idx", "filtered_pnl", "filtered_holding_period"))
    expect_equal(filtered_trades$start_idx, c(1,5,10))

    expect_equal(filtered_trades$end_idx, c(3,10,13))
    expect_equal(filtered_trades$filtered_end_idx, c(3,10,13))

    expect_equal(filtered_trades$pnl, c(2,8,11))
    expect_equal(filtered_trades$filtered_pnl, c(2,8,11))

    expect_equal(filtered_trades$holding_period, c(2,5,3))
    expect_equal(filtered_trades$filtered_holding_period, c(2,5,3))
  }
  test.apply.max.loss.filter.1()

  #------------------------------------------------------------------------------
  test.apply.max.loss.filter.2 = function() {

    # Second trade gets stopped when P&L gets to -2.1
    filtered_trades = apply.stop.filter(trades, max.loss.filter(execution, 2))

    expect_equal(names(trades), c("start_idx", "end_idx", "holding_period", "pnl", "excursion"))
    expect_equal(names(filtered_trades), c("start_idx", "end_idx", "holding_period", "pnl", "excursion", "filtered_end_idx", "filtered_pnl", "filtered_holding_period"))
    expect_equal(filtered_trades$start_idx, c(1, 5, 10))

    expect_equal(filtered_trades$end_idx, c(3, 10, 13))
    expect_equal(filtered_trades$filtered_end_idx, c(3, 7, 13))

    expect_equal(filtered_trades$pnl, c(2, 8, 11))
    expect_equal(filtered_trades$filtered_pnl, c(2, -2.1, 11))

    expect_equal(filtered_trades$holding_period, c(2, 5, 3))
    expect_equal(filtered_trades$filtered_holding_period, c(2, 2, 3))
  }
  test.apply.max.loss.filter.2()

  #------------------------------------------------------------------------------
  test.apply.max.loss.filter.3 = function() {

    # Second trade gets stopped when P&L gets to -4.5. There is also a loss of -4.9
    # directly after but we get stopped out before that
    filtered_trades = apply.stop.filter(trades, max.loss.filter(execution, 4))

    expect_equal(names(trades), c("start_idx", "end_idx", "holding_period", "pnl", "excursion"))
    expect_equal(names(filtered_trades), c("start_idx", "end_idx", "holding_period", "pnl", "excursion", "filtered_end_idx", "filtered_pnl", "filtered_holding_period"))
    expect_equal(filtered_trades$start_idx, c(1,5,10))

    expect_equal(filtered_trades$end_idx, c(3,10,13))
    expect_equal(filtered_trades$filtered_end_idx, c(3, 8, 13))

    expect_equal(filtered_trades$pnl, c(2, 8, 11))
    expect_equal(filtered_trades$filtered_pnl, c(2, -4.5, 11))

    expect_equal(filtered_trades$holding_period, c(2, 5, 3))
    expect_equal(filtered_trades$filtered_holding_period, c(2, 3, 3))
  }
  test.apply.max.loss.filter.3()
}

test.apply.stop.filter()
