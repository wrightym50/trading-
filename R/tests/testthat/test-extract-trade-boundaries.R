library(testthat)

test.extract.trade.boundaries = function() {

  empty.index = function() {
    return(vector(mode="integer", 0))
  }

  #------------------------------------------------------------------------------
  extract.trade.boundaries_test.0.0 = function() {
    pos = c(0, 1, 0)

    boundaries = extract.trade.boundaries(pos)

    expect_equal(boundaries$start_idx, c(2))
    expect_equal(boundaries$end_idx, c(3))
  }
  extract.trade.boundaries_test.0.0()

  #------------------------------------------------------------------------------
  extract.trade.boundaries_test.0.1 = function() {
    pos = c(0, -1, 0)

    boundaries = extract.trade.boundaries(pos)

    expect_equal(boundaries$start_idx, c(2))
    expect_equal(boundaries$end_idx, c(3))
  }
  extract.trade.boundaries_test.0.1()

  #------------------------------------------------------------------------------
  extract.trade.boundaries_test.0.2 = function() {
    pos = c(0, 1)

    boundaries = extract.trade.boundaries(pos)

    expect_equal(boundaries$start_idx, empty.index())
    expect_equal(boundaries$end_idx, empty.index())
  }
  extract.trade.boundaries_test.0.2()

  #------------------------------------------------------------------------------
  extract.trade.boundaries_test.0.3 = function() {
    pos = c(0, -1)

    boundaries = extract.trade.boundaries(pos)

    expect_equal(boundaries$start_idx, empty.index())
    expect_equal(boundaries$end_idx, empty.index())
  }
  extract.trade.boundaries_test.0.3()

  #------------------------------------------------------------------------------
  extract.trade.boundaries_test.1 = function() {
    pos = c(0, 1, 0, -1, -1, 0)

    boundaries = extract.trade.boundaries(pos)

    expect_equal(boundaries$start_idx, c(2, 4))
    expect_equal(boundaries$end_idx, c(3, 6))
  }
  extract.trade.boundaries_test.1()

  #------------------------------------------------------------------------------
  extract.trade.boundaries_test.2 = function() {
    pos = c(0, 2, 0)

    boundaries = extract.trade.boundaries(pos)

    expect_equal(boundaries$start_idx, c(2))
    expect_equal(boundaries$end_idx, c(3))
  }
  extract.trade.boundaries_test.2()

  #------------------------------------------------------------------------------
  extract.trade.boundaries_test.3 = function() {
    pos = c(0, -2, 0)

    boundaries = extract.trade.boundaries(pos)

    expect_equal(boundaries$start_idx, c(2))
    expect_equal(boundaries$end_idx, c(3))
  }
  extract.trade.boundaries_test.3()

  #------------------------------------------------------------------------------
  extract.trade.boundaries_test.4 = function() {
    pos = c(0, -2, -2)

    boundaries = extract.trade.boundaries(pos)

    expect_equal(boundaries$start_idx, empty.index())
    expect_equal(boundaries$end_idx, empty.index())
  }
  extract.trade.boundaries_test.4()

  #------------------------------------------------------------------------------
  extract.trade.boundaries_test.5 = function() {
    pos = c(-2, 0)

    boundaries = extract.trade.boundaries(pos)

    expect_equal(boundaries$start_idx, c(1))
    expect_equal(boundaries$end_idx, c(2))
  }
  extract.trade.boundaries_test.5()

  #------------------------------------------------------------------------------
  extract.trade.boundaries_test.6 = function() {
    pos = c(2, 0)

    boundaries = extract.trade.boundaries(pos)

    expect_equal(boundaries$start_idx, c(1))
    expect_equal(boundaries$end_idx, c(2))
  }
  extract.trade.boundaries_test.6()

  #------------------------------------------------------------------------------
  extract.trade.boundaries_test.7 = function() {
    pos = c(2, -2, 0)

    boundaries = extract.trade.boundaries(pos)

    expect_equal(boundaries$start_idx, c(1, 2))
    expect_equal(boundaries$end_idx, c(2, 3))
  }
  extract.trade.boundaries_test.7()

  #------------------------------------------------------------------------------
  extract.trade.boundaries_test.8 = function() {
    pos = c(-3, 3, 0)

    boundaries = extract.trade.boundaries(pos)

    expect_equal(boundaries$start_idx, c(1, 2))
    expect_equal(boundaries$end_idx, c(2, 3))
  }
  extract.trade.boundaries_test.8()

  #------------------------------------------------------------------------------
  extract.trade.boundaries_test.9 = function() {
    pos = c(-3, -2, -1, 2, 1, -1, 0)

    boundaries = extract.trade.boundaries(pos)

    expect_equal(boundaries$start_idx, c(1, 4, 6))
    expect_equal(boundaries$end_idx, c(4, 6, 7))
  }
  extract.trade.boundaries_test.9()
}

test.extract.trade.boundaries()
