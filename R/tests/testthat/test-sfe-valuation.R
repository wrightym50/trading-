library(testthat)

test.sfe.valuation = function() {

  expect_that(gov.bond.value(95.50, 100000, 600, 2, 10), equals(111972.78))
  expect_that(gov.bond.value(94.36, 100000, 600, 2, 10), equals(102723.06))
  expect_that(gov.bond.value(94.35, 100000, 600, 2, 10), equals(102646.19))

  expect_that(xt.value(95.50), equals(111972.78))
  expect_that(xt.value(94.36), equals(102723.06))
  expect_that(xt.value(94.35), equals(102646.19))

  expect_that(bank.bill.value(94.5), equals(986619.81))
  expect_that(bank.bill.value(95.0), equals(987821.38))
  expect_that(bank.bill.value(94.54), equals(986715.83))
  expect_that(bank.bill.value(94.51), equals(986643.82))

  expect_that(dv.01(bank.bill.value, 95.00), equals(24.06))
  expect_that(dv.01(xt.value, 94.36), equals(76.87))

}

test.sfe.valuation()
