library(testthat)

test.evaluate.leg = function() {

  clip_count = c(  0, 2, 0)
  leg_price  = c(100,  101, 102)
  leg_ratio = c(1, 1.2, 1.1)

  evaluation = evaluate.leg(clip_count, leg_price, leg_ratio)

  expect_equal(nrow(evaluation), 3)

  expect_equal(names(evaluation), c("cash", "cash_delta", "mtm", "pnl", "clip_count", "leg_qty", "leg_entry_ratio", "leg_entry_price", "leg_spot_price", "leg_spot_ratio"))

  expect_equal(class(evaluation[1,]), "data.frame")

  expect_equal(evaluation$cash, c(0, 0, 2.4))
  expect_equal(evaluation$cash_delta, c(0, 0, 2.4))
  expect_equal(evaluation$mtm, c(0, 0, 0))
  expect_equal(evaluation$clip_count, c(0, 2, 0))
  expect_equal(evaluation$leg_qty, c(0, 2.4, 0))
  expect_equal(evaluation$leg_entry_ratio, c(NA, 1.2, NA))
  expect_equal(evaluation$leg_entry_price, c(NA, 101, NA))
  expect_equal(evaluation$leg_spot_price, c(100, 101, 102))
  expect_equal(evaluation$leg_spot_ratio, c(1, 1.2, 1.1))
}

test.evaluate.leg()
