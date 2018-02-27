library(testthat)

test.evaluate.leg.expr = function() {
  data = data.frame(IR03=c(1,2,3,4), IR06=c(12,9,20,-10))

  result = evaluate.leg.expr(list(IR03=1, IR06=-1), data)

  expect_equal(result, c(-11, -7, -17, 14))
}

test.evaluate.leg.expr()
