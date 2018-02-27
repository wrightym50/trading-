library(testthat)

test.apply.position.sizing = function() {

  # First test is one trade only to get started
  result = apply.position.sizing(c(0.03), 0.02, 100000, 0.01, 2000, 25)

  expect_equal(result$start_capital, c(100000))
  expect_equal(result$max_dollar_loss, c(1000))
  expect_equal(result$max_lots, c(50))
  expect_equal(result$bet_lots, c(20))
  expect_equal(result$bet_pnl, c(1500))
  expect_equal(result$end_capital, c(101500))

  # Second test has 2 trades

  result = apply.position.sizing(c(0.03,-0.01), 0.02, 100000, 0.01, 2000, 25)

  expect_equal(result$start_capital, c(100000, 101500))
  expect_equal(result$max_dollar_loss, c(1000, 1015))
  expect_equal(result$max_lots, c(50, 50.75))
  expect_equal(result$bet_lots, c(20, 20.3))
  expect_equal(result$bet_pnl, c(1500, -507.5))
  expect_equal(result$end_capital, c(101500, 100992.5))

  # Third test has 3 trades

  result = apply.position.sizing(c(0.03,-0.01,0.05), 0.02, 100000, 0.01, 2000, 25)

  expect_equal(result$start_capital, c(100000, 101500,100992.5))
  expect_equal(result$max_dollar_loss, c(1000, 1015, 1009.925))
  expect_equal(result$max_lots, c(50, 50.75, 50.49625))
  expect_equal(result$bet_lots, c(20, 20.3, 20.1985))
  expect_equal(result$bet_pnl, c(1500, -507.5, 2524.8125))
  expect_equal(result$end_capital, c(101500, 100992.5, 103517.3125))

  # Third test has 1 trades with a bet ratio of 1 and a big loss which wipes
  # us out. The following trade must be 0.

  result = apply.position.sizing(c(-1.00, 0.5), 0.02, 100000, 1.0, 2000, 25)

  expect_equal(result$start_capital, c(100000, 0))
  expect_equal(result$max_dollar_loss, c(100000, 0))
  expect_equal(result$max_lots, c(50, 0))
  expect_equal(result$bet_lots, c(50, 0))
  expect_equal(result$bet_pnl, c(-125000, 0))
  expect_equal(result$end_capital, c(0, 0))
}

test.apply.position.sizing()
