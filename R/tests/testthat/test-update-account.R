library(testthat)

test.update.account = function() {

  initialise.account = function() {
    account = data.frame(cash=0, cash_delta=0, mtm=0, clip_count=0, leg_qty=0, leg_entry_ratio=NA, leg_entry_price=NA, leg_spot_price=NA, leg_spot_ratio=NA)
    expect_that(account, equals(data.frame(cash=0, cash_delta=0, mtm=0, clip_count=0, leg_qty=0, leg_entry_ratio=NA, leg_entry_price=NA, leg_spot_price=NA, leg_spot_ratio=NA)))
    return(account)
  }

  #------------------------------------------------------------------------------
  # Test long calculation with leg ratio of 1

  account = initialise.account()

  # No change to clip count but leg_ratio and leg_spot_price are updated
  account = update.account(account, clip_count=0, leg_price=99, leg_ratio=1)
  expect_that(account, equals(data.frame(cash=0, cash_delta=0, mtm=0, clip_count=0, leg_qty=0, leg_entry_ratio=NA, leg_entry_price=NA, leg_spot_price=99, leg_spot_ratio=1)))

  # Long 1
  account = update.account(account, clip_count=1, leg_price=100, leg_ratio=1)
  expect_equal(account, data.frame(cash=0, cash_delta=0, mtm=0, clip_count=1, leg_qty=1, leg_entry_ratio=1, leg_entry_price=100, leg_spot_price = 100, leg_spot_ratio=1))

  # Long 1. Mark-to-market changes by three basis points
  account = update.account(account, clip_count=1, leg_price=101, leg_ratio=1)
  expect_that(account, equals(data.frame(cash=0, cash_delta=0, mtm=1, clip_count=1, leg_qty=1, leg_entry_ratio=1, leg_entry_price=100, leg_spot_price = 101, leg_spot_ratio=1)))

  # Long 2. Mark-to-market on previous clip is now 2
  account = update.account(account, clip_count=2, leg_price=102, leg_ratio=1)
  expect_that(account, equals(data.frame(cash=0, cash_delta=0, mtm=2, clip_count=2, leg_qty=2, leg_entry_ratio=1, leg_entry_price=101, leg_spot_price = 102, leg_spot_ratio=1)))

  # Make two ticks on 1 lot
  account = update.account(account, clip_count=1, leg_price=103, leg_ratio=1)
  expect_that(account, equals(data.frame(cash=2, cash_delta=2, mtm=2, clip_count=1, leg_qty=1, leg_entry_ratio=1, leg_entry_price=101, leg_spot_price = 103, leg_spot_ratio=1)))

  # Lose 1 tick on 1 lot. Back to flat
  account = update.account(account, clip_count=0, leg_price=100, leg_ratio=1)
  expect_that(account, equals(data.frame(cash=1, cash_delta=-1, mtm=0, clip_count=0, leg_qty=0, leg_entry_ratio=NA, leg_entry_price=NA, leg_spot_price = 100, leg_spot_ratio=1)))

  #------------------------------------------------------------------------------
  # Test short calculation with leg ratio of 1

  account = initialise.account()

  # No change to clip count but leg_ratio and leg_spot_price are updated
  account = update.account(account, clip_count=0, leg_price=99, leg_ratio=1)
  expect_that(account, equals(data.frame(cash=0, cash_delta=0, mtm=0, clip_count=0, leg_qty=0, leg_entry_ratio=NA, leg_entry_price=NA, leg_spot_price=99, leg_spot_ratio=1)))

  # Short 1
  account = update.account(account, clip_count=-1, leg_price=100, leg_ratio=1)
  expect_that(account, equals(data.frame(cash=0, cash_delta=0, mtm=0, clip_count=-1, leg_qty=-1, leg_entry_ratio=1, leg_entry_price=100, leg_spot_price = 100, leg_spot_ratio=1)))

  # Short 2
  account = update.account(account, clip_count=-2, leg_price=102, leg_ratio=1)
  expect_that(account, equals(data.frame(cash=0, cash_delta=0, mtm=-2, clip_count=-2, leg_qty=-2, leg_entry_ratio=1, leg_entry_price=101, leg_spot_price = 102, leg_spot_ratio=1)))

  # Lose two ticks on 1 lot
  account = update.account(account, clip_count=-1, leg_price=103, leg_ratio=1)
  expect_that(account, equals(data.frame(cash=-2, cash_delta=-2, mtm=-2, clip_count=-1, leg_qty=-1, leg_entry_ratio=1, leg_entry_price=101, leg_spot_price = 103, leg_spot_ratio=1)))

  # Make 1 tick on 1 lot. Back to flat
  account = update.account(account, clip_count=0, leg_price=100, leg_ratio=1)
  expect_that(account, equals(data.frame(cash=-1, cash_delta=1, mtm=0, clip_count=0, leg_qty=0, leg_entry_ratio=NA, leg_entry_price=NA, leg_spot_price = 100, leg_spot_ratio=1)))

  #------------------------------------------------------------------------------
  # Test changing leg ratios but without ratio freezing

  account = initialise.account()

  # No change to clip count but leg_ratio and leg_spot_price are updated
  account = update.account(account, clip_count=0, leg_price=99, leg_ratio=1.5)
  expect_that(account, equals(data.frame(cash=0, cash_delta=0, mtm=0, clip_count=0, leg_qty=0, leg_entry_ratio=NA, leg_entry_price=NA, leg_spot_price=99, leg_spot_ratio=1.5)))

  # Long 1
  account = update.account(account, clip_count=1, leg_price=100, leg_ratio=1.6)
  expect_that(account, equals(data.frame(cash=0, cash_delta=0, mtm=0, clip_count=1, leg_qty=1.6, leg_entry_ratio=1.6, leg_entry_price=100, leg_spot_price = 100, leg_spot_ratio=1.6)))

  # Long 2
  account = update.account(account, clip_count=2, leg_price=102, leg_ratio=1.7)

  expected_leg_entry_price = (100*1.6 + 102*1.8)/3.4
  expected_mtm = (102-expected_leg_entry_price)*3.4
  expect_that(account, equals(data.frame(cash=0, cash_delta=0, mtm=expected_mtm, clip_count=2, leg_qty=3.4, leg_entry_ratio=1.7, leg_entry_price=expected_leg_entry_price, leg_spot_price = 102, leg_spot_ratio=1.7)))


  # Make two ticks on 1 lot.

  account = update.account(account, clip_count=1, leg_price=103, leg_ratio=1.8)

  # The old leg_qty is 3.4, the new leg_qty is 1.8. We have to sell 1.6 lots to get there. The entry_price is 'expected_leg_entry_price'
  # and the exit price is 103. Thus the expected cash is as follows:
  expected_cash = (3.4-1.8)*(103-expected_leg_entry_price)
  expected_mtm = (103-expected_leg_entry_price)*1.8
  expect_that(account, equals(data.frame(cash=expected_cash, cash_delta=expected_cash, mtm=expected_mtm, clip_count=1, leg_qty=1.8, leg_entry_ratio=1.8, leg_entry_price=expected_leg_entry_price, leg_spot_price = 103, leg_spot_ratio=1.8)))

  # Lose 1 tick on 1 lot. The old leg_qty is 1.8. We sell all of it at the price of 100.
  account = update.account(account, clip_count=0, leg_price=100, leg_ratio=1.9)

  cash_delta = 1.8*(100 - expected_leg_entry_price)
  expected_cash = expected_cash + cash_delta
  expect_that(account, equals(data.frame(cash=expected_cash, cash_delta=cash_delta, mtm=0, clip_count=0, leg_qty=0, leg_entry_ratio=NA, leg_entry_price=NA, leg_spot_price = 100, leg_spot_ratio=1.9)))

  # Short 1
  account = update.account(account, clip_count=-1, leg_price=100, leg_ratio=1.9)
  expect_that(account, equals(data.frame(cash=expected_cash, cash_delta=0, mtm=0, clip_count=-1, leg_qty=-1.9, leg_entry_ratio=1.9, leg_entry_price=100, leg_spot_price = 100, leg_spot_ratio=1.9)))

  # Short 2
  account = update.account(account, clip_count=-2, leg_price=102, leg_ratio=2.0)

  expected_leg_entry_price = (100*1.9 + 102*2.1)/4.0
  expected_mtm=(102-expected_leg_entry_price)*-4
  expect_that(account, equals(data.frame(cash=expected_cash, cash_delta=0, mtm=expected_mtm, clip_count=-2, leg_qty=-4.0, leg_entry_ratio=2.0, leg_entry_price=expected_leg_entry_price, leg_spot_price = 102, leg_spot_ratio=2.0)))

  # Lose two ticks on 1 lot. The old leg_qty is -4.0. The new leg_qty is -2.1. We have to buy 1.9 to get there.
  account = update.account(account, clip_count=-1, leg_price=103, leg_ratio=2.1)

  cash_delta = 1.9*(expected_leg_entry_price-103)
  expected_cash = expected_cash + cash_delta
  expected_mtm = (103-expected_leg_entry_price)*-2.1
  expect_that(account, equals(data.frame(cash=expected_cash, cash_delta=cash_delta, mtm=expected_mtm, clip_count=-1, leg_qty=-2.1, leg_entry_ratio=2.1, leg_entry_price=expected_leg_entry_price, leg_spot_price = 103, leg_spot_ratio=2.1)))

  # Make 1 tick on 1 lot. Back to flat. The old leg_qty is -2.1 and the new one is 0. We have to buy 2.1 to get there.
  account = update.account(account, clip_count=0, leg_price=100, leg_ratio=2.2)

  cash_delta = 2.1*(expected_leg_entry_price-100)
  expected_cash = expected_cash + cash_delta
  expect_that(account, equals(data.frame(cash=expected_cash, cash_delta=cash_delta, mtm=0, clip_count=0, leg_qty=0, leg_entry_ratio=NA, leg_entry_price=NA, leg_spot_price = 100, leg_spot_ratio=2.2)))

  #------------------------------------------------------------------------------
  # Test swing from long to short and back again, leg ratio stays the same

  account = initialise.account()

  account = update.account(account, clip_count=0, leg_price=99, leg_ratio=1)
  expect_that(account, equals(data.frame(cash=0, cash_delta=0, mtm=0, clip_count=0, leg_qty=0, leg_entry_ratio=NA, leg_entry_price=NA, leg_spot_price=99, leg_spot_ratio=1)))

  # Long 1
  account = update.account(account, clip_count=1, leg_price=100, leg_ratio=1)
  expect_that(account, equals(data.frame(cash=0, cash_delta=0, mtm=0, clip_count=1, leg_qty=1, leg_entry_ratio=1, leg_entry_price=100, leg_spot_price = 100, leg_spot_ratio=1)))

  # Short 2. Make 1 tick on the long position. Enter a new short position.
  account = update.account(account, clip_count=-2, leg_price=101, leg_ratio=1)
  expect_that(account, equals(data.frame(cash=1, cash_delta=1, mtm=0, clip_count=-2, leg_qty=-2, leg_entry_ratio=1, leg_entry_price=101, leg_spot_price = 101, leg_spot_ratio=1)))

  # Long 3. Price goes up one tick. Lose 2 ticks on the short position. Enter a new long position.
  account = update.account(account, clip_count=3, leg_price=102, leg_ratio=1)
  expect_that(account, equals(data.frame(cash=-1, cash_delta=-2, mtm=0, clip_count=3, leg_qty=3, leg_entry_ratio=1, leg_entry_price=102, leg_spot_price = 102, leg_spot_ratio=1)))

  #------------------------------------------------------------------------------
  # Test swing from short to long and back again, leg ratio stays the same

  account = initialise.account()

  account = update.account(account, clip_count=0, leg_price=99, leg_ratio=1)
  expect_that(account, equals(data.frame(cash=0, cash_delta=0, mtm=0, clip_count=0, leg_qty=0, leg_entry_ratio=NA, leg_entry_price=NA, leg_spot_price=99, leg_spot_ratio=1)))

  # Short 1
  account = update.account(account, clip_count=-1, leg_price=100, leg_ratio=1)
  expect_that(account, equals(data.frame(cash=0, cash_delta=0, mtm=0, clip_count=-1, leg_qty=-1, leg_entry_ratio=1, leg_entry_price=100, leg_spot_price = 100, leg_spot_ratio=1)))

  # Long 2. Lose 1 tick on the short position. Enter a new long position.
  account = update.account(account, clip_count=2, leg_price=101, leg_ratio=1)
  expect_that(account, equals(data.frame(cash=-1, cash_delta=-1, mtm=0, clip_count=2, leg_qty=2, leg_entry_ratio=1, leg_entry_price=101, leg_spot_price = 101, leg_spot_ratio=1)))

  # Short 3. Price goes up one tick. Lose 2 ticks on the long position. Enter a new short position.
  account = update.account(account, clip_count=-3, leg_price=102, leg_ratio=1)
  expect_that(account, equals(data.frame(cash=1, cash_delta=2, mtm=0, clip_count=-3, leg_qty=-3, leg_entry_ratio=1, leg_entry_price=102, leg_spot_price = 102, leg_spot_ratio=1)))

  #------------------------------------------------------------------------------
  # Test swing from long to short and back again, leg ratio changes

  account = initialise.account()

  account = update.account(account, clip_count=0, leg_price=99, leg_ratio=1)
  expect_that(account, equals(data.frame(cash=0, cash_delta=0, mtm=0, clip_count=0, leg_qty=0, leg_entry_ratio=NA, leg_entry_price=NA, leg_spot_price=99, leg_spot_ratio=1)))

  # Long 1
  account = update.account(account, clip_count=1, leg_price=100, leg_ratio=1)
  expect_that(account, equals(data.frame(cash=0, cash_delta=0, mtm=0, clip_count=1, leg_qty=1, leg_entry_ratio=1, leg_entry_price=100, leg_spot_price = 100, leg_spot_ratio=1)))

  # Short 2. Make 1 tick on the long position. Enter a new short position at new ratio of 2
  account = update.account(account, clip_count=-2, leg_price=101, leg_ratio=2)
  expect_that(account, equals(data.frame(cash=1, cash_delta=1, mtm=0, clip_count=-2, leg_qty=-4, leg_entry_ratio=2, leg_entry_price=101, leg_spot_price = 101, leg_spot_ratio=2)))

  # Long 3. Price goes up one tick. Lose 4 ticks on the short position. Enter a new long position at a new ratio
  account = update.account(account, clip_count=3, leg_price=102, leg_ratio=3)
  expect_that(account, equals(data.frame(cash=-3, cash_delta=-4, mtm=0, clip_count=3, leg_qty=9, leg_entry_ratio=3, leg_entry_price=102, leg_spot_price = 102, leg_spot_ratio=3)))

  #------------------------------------------------------------------------------
  # Test swing from short to long and back again, leg ratio changes

  account = initialise.account()

  account = update.account(account, clip_count=0, leg_price=99, leg_ratio=1)
  expect_that(account, equals(data.frame(cash=0, cash_delta=0, mtm=0, clip_count=0, leg_qty=0, leg_entry_ratio=NA, leg_entry_price=NA, leg_spot_price=99, leg_spot_ratio=1)))

  # Short 1
  account = update.account(account, clip_count=-1, leg_price=100, leg_ratio=1)
  expect_that(account, equals(data.frame(cash=0, cash_delta=0, mtm=0, clip_count=-1, leg_qty=-1, leg_entry_ratio=1, leg_entry_price=100, leg_spot_price = 100, leg_spot_ratio=1)))

  # Long 2. Lose 1 tick on the short position. Enter a new long position at new ratio of 2
  account = update.account(account, clip_count=2, leg_price=101, leg_ratio=2)
  expect_that(account, equals(data.frame(cash=-1, cash_delta=-1, mtm=0, clip_count=2, leg_qty=4, leg_entry_ratio=2, leg_entry_price=101, leg_spot_price = 101, leg_spot_ratio=2)))

  # Short 3. Price goes up one tick. Make 4 ticks on the long position. Enter a new short position at a new ratio
  account = update.account(account, clip_count=-3, leg_price=102, leg_ratio=3)
  expect_that(account, equals(data.frame(cash=3, cash_delta=4, mtm=0, clip_count=-3, leg_qty=-9, leg_entry_ratio=3, leg_entry_price=102, leg_spot_price = 102, leg_spot_ratio=3)))

  #------------------------------------------------------------------------------
  # Test getting further into a long spread position but where the hedge ratio changes so much that the leg position actually reduces

  account = initialise.account()

  account = update.account(account, clip_count=0, leg_price=99, leg_ratio=1)
  expect_that(account, equals(data.frame(cash=0, cash_delta=0, mtm=0, clip_count=0, leg_qty=0, leg_entry_ratio=NA, leg_entry_price=NA, leg_spot_price=99, leg_spot_ratio=1)))

  # Long 1, leg_ratio=4, leg_qty=4
  account = update.account(account, clip_count=1, leg_price=100, leg_ratio=4)
  expect_that(account, equals(data.frame(cash=0, cash_delta=0, mtm=0, clip_count=1, leg_qty=4, leg_entry_ratio=4, leg_entry_price=100, leg_spot_price = 100, leg_spot_ratio=4)))

  # Long 2, leg_ratio=1.5, leg_qty=3. We have to sell 1 lot at the 101, cash_delta=1, mtm is 3 lots at 1 (= 3)
  account = update.account(account, clip_count=2, leg_price=101, leg_ratio=1.5)
  expect_that(account, equals(data.frame(cash=1, cash_delta=1, mtm=3, clip_count=2, leg_qty=3, leg_entry_ratio=1.5, leg_entry_price=100, leg_spot_price = 101, leg_spot_ratio=1.5)))

  #------------------------------------------------------------------------------
  # Test getting further into a short position but where the hedge ratio changes so much that the leg position actually reduces

  account = initialise.account()

  account = update.account(account, clip_count=0, leg_price=99, leg_ratio=1)
  expect_that(account, equals(data.frame(cash=0, cash_delta=0, mtm=0, clip_count=0, leg_qty=0, leg_entry_ratio=NA, leg_entry_price=NA, leg_spot_price=99, leg_spot_ratio=1)))

  # Short 1, leg_ratio=4, leg_qty=-4
  account = update.account(account, clip_count=-1, leg_price=100, leg_ratio=4)
  expect_that(account, equals(data.frame(cash=0, cash_delta=0, mtm=0, clip_count=-1, leg_qty=-4, leg_entry_ratio=4, leg_entry_price=100, leg_spot_price = 100, leg_spot_ratio=4)))

  # Short 2, leg_ratio=1.5, leg_qty=-3. We have to buy 1 lot at the 101, cash_delta=-1, mtm is 3 lots at -1 (= -3)
  account = update.account(account, clip_count=-2, leg_price=101, leg_ratio=1.5)
  expect_that(account, equals(data.frame(cash=-1, cash_delta=-1, mtm=-3, clip_count=-2, leg_qty=-3, leg_entry_ratio=1.5, leg_entry_price=100, leg_spot_price = 101, leg_spot_ratio=1.5)))

  #------------------------------------------------------------------------------
  # Test getting further into a long spread position but where the hedge ratio changes sign

  account = initialise.account()

  # Long 1, leg_ratio=4, leg_qty=4
  account = update.account(account, clip_count=1, leg_price=100, leg_ratio=4)
  expect_that(account, equals(data.frame(cash=0, cash_delta=0, mtm=0, clip_count=1, leg_qty=4, leg_entry_ratio=4, leg_entry_price=100, leg_spot_price = 100, leg_spot_ratio=4)))

  # Long 2, leg_ratio=-3, leg_qty=-6. We have to sell 10 lots at the 101, cash_delta=4, mtm is 0 because of new position
  account = update.account(account, clip_count=2, leg_price=101, leg_ratio=-3)
  expect_that(account, equals(data.frame(cash=4, cash_delta=4, mtm=0, clip_count=2, leg_qty=-6, leg_entry_ratio=-3, leg_entry_price=101, leg_spot_price = 101, leg_spot_ratio=-3)))

  #------------------------------------------------------------------------------
  # Test staying in a long spread position but where the hedge ratio changes

  account = initialise.account()

  # Long 1, leg_ratio=4, leg_qty=4
  account = update.account(account, clip_count=1, leg_price=100, leg_ratio=4)
  expect_that(account, equals(data.frame(cash=0, cash_delta=0, mtm=0, clip_count=1, leg_qty=4, leg_entry_ratio=4, leg_entry_price=100, leg_spot_price = 100, leg_spot_ratio=4)))

  # Long 1, leg_ratio=5, leg_qty=5. We have to buy 1 lot at the 101, for an average price of 100.2 and an mtm of 0.8*5 (=4)
  account = update.account(account, clip_count=1, leg_price=101, leg_ratio=5)
  expect_that(account, equals(data.frame(cash=0, cash_delta=0, mtm=4, clip_count=1, leg_qty=5, leg_entry_ratio=5, leg_entry_price=100.2, leg_spot_price = 101, leg_spot_ratio=5)))

  # Long 1, leg_ratio=3, leg_qty=3. We have to sell 2 lots at the 102. Cash delta is 1.8*2 (=3.6), MTM is 1.8*3(=5.4)
  account = update.account(account, clip_count=1, leg_price=102, leg_ratio=3)
  expect_that(account, equals(data.frame(cash=3.6, cash_delta=3.6, mtm=5.4, clip_count=1, leg_qty=3, leg_entry_ratio=3, leg_entry_price=100.2, leg_spot_price = 102, leg_spot_ratio=3)))

  # Long 1, leg_ratio=-2, leg_qty=-2. We have to sell 5 lots at the 103. Cash delta is (103-100.2)*3 (=8.4), MTM is 0 due to new position
  account = update.account(account, clip_count=1, leg_price=103, leg_ratio=-2)
  expect_that(account, equals(data.frame(cash=12.0, cash_delta=8.4, mtm=0, clip_count=1, leg_qty=-2, leg_entry_ratio=-2, leg_entry_price=103, leg_spot_price = 103, leg_spot_ratio=-2)))

}

test.update.account()
