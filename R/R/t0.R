library(GA)
library(tictoc)
library(lubridate)
library(tseries)
library(egcm)
library(tidyr)
library(jsonlite)

#' Exponential moving average
#' @export
ema = function(tau, t, z) {
  stopifnot(length(z) > 0)
  result = rep(0, length(z))

  result[1] = z[1]
  for (i in 2:length(z)) {
    delta_t = t[i] - t[i-1]
    mu = exp(-delta_t/tau)
    result[i] = mu*result[i-1] + (1-mu)*z[i]
  }

  return(result)
}

#' Iterated exponential moving average
#' @export
iema = function(tau, n, t, z) {

  stopifnot(n >= 1)

  result = z
  for (i in 1:n) {
    result = ema(tau, t, result)
  }

  return(result)
}

#' Smoothed moving average
#' @export
ma = function(tau, n, t, z) {

  stopifnot(n >= 1)

  tau_prime = 2*tau/(n+1)
  result = rep(0, length(z))

  for (i in 1:n) {
    result = result + iema(tau_prime, i, t, z)
  }

  return(result/n)
}

#' Moving standard deviation
#' @export
msd = function(tau, n, t, z) {

  # Moving mean
  mean = ma(tau, n, t, z)

  # Moving variance. Moving average of the
  # squared differences
  variance = ma(tau, n, t, (z-mean)^2)

  return(sqrt(variance))
}

#' Exponential moving sum
#' @export
ems = function(tau, t, z) {
  stopifnot(length(z) > 0)
  result = rep(0, length(z))

  result[1] = z[1]
  for (i in 2:length(z)) {
    delta_t = t[i] - t[i-1]
    mu = exp(-delta_t/tau)
    result[i] = mu*result[i-1] + z[i]
  }

  return(result)
}

#' One step of the Kalman filter
#' @export
linear.update = function(x,P,F,Q,z,H,R) {
  # x - most recent estimate of hidden state's mean
  # P - most recent estimate of hidden state's covariance matrix
  # F - state transition matrix - how does the hidden state evolve?
  # Q - the true covariance matrix of the hidden state
  # z - the new observation
  # H - the observation model matrix - how does z relate to x?
  # R - the covariance matrix of the observation
  x = F %*% x
  P = F %*% P %*% t(F) + Q
  z_predict = H %*% x
  y = z - z_predict
  S = H %*% P %*% t(H) + R
  K = P %*% t(H) %*% solve(S)
  x = x + K %*% y
  P = (diag(length(x)) - K %*% H) %*% P
  return (list(mean=x,cov=P))
}

#' Test for Kalman filter
estimate.whitenoise = function() {

  x = rep(0, 1000)

  state_mean = 10

  state_var = 0.01

  print(state_var)
  observation_var = 2.0

  P = matrix(1)
  F = diag(1)
  Q = matrix(state_var)
  H = diag(1)
  R = matrix(observation_var)

  N = 200
  x = rnorm(N, state_mean, state_var)
  z = H %*% x + rnorm(N, 0, observation_var)

  state_estimate = data.frame(mean=double(N), cov=double(N))

  state_estimate[1,] = list(mean=5, cov=1)

  for (i in 2:N) {
    prev_state = state_estimate[i-1,]
    new_state = linear.update(as.matrix(prev_state$mean), as.matrix(prev_state$cov), F, Q, z[i], H, R)
    state_estimate[i,] = new_state
    prev_state = new_state
  }

  plot(x, type="l", ylim=c(0,13), col="black")
  lines(z[1,], type="l", ylim=c(0,13), col="blue")

  lines(state_estimate$mean, col="red")
  stderr = sqrt(state_estimate$cov)
  lines(state_estimate$mean+2*stderr, col="green")
  lines(state_estimate$mean-2*stderr, col="green")
}

#' Use a Kalman filter to do a rolling regression between a leading leg and dependent leg in the
#' data
#' @export
kalman.regression = function(data, p1, p2, q1, q2, r) {

  # dependent_leg
  # leading_leg
  # p1 - initial estimated variance of intercept term
  # p2 - initial estimated variance of slope term
  # q1 - the process noise of the intercept term
  # q2 - the process noise of the slope term
  # r - the process noise of the dependent leg; i.e., the observation process

  time = data$time
  dependent_leg = data$y
  leading_leg = data$x

  stopifnot(length(dependent_leg) == length(leading_leg) && length(time) == length(dependent_leg))

  N = length(leading_leg)

  # Transition matrix - identity transition
  F = diag(2)

  # Covariance of state estimate
  P = matrix(c(p1, 0, 0, p2), nrow=2, ncol=2)
  # Covarinace of state process noise
  Q = matrix(c(q1, 0, 0, q2), nrow=2, ncol=2)
  # Variance of observation noise
  R = matrix(r)

  states = list()
  first_state = list(mean=matrix(0, nrow=2, ncol=1), cov=P)
  states[[1]] = first_state

  for (i in 2:N) {
     prev_state = states[[i-1]]
     H = matrix(c(1, leading_leg[i]), nrow = 1, ncol = 2)
     new_state = linear.update(prev_state$mean, prev_state$cov, F, Q, dependent_leg[i], H, R)
     states[[i]] = new_state
  }

  prediction = rep(0.0, N)
  residual = rep(0.0, N)

  intercept = data.frame(mean=rep(0.0,N), var=rep(0.0,N))
  slope = data.frame(mean=rep(0.0,N), var=rep(0.0,N))

  for (i in 1:length(states)) {
    intercept[i,] = c(states[[i]]$mean[1], states[[i]]$cov[1,1])
    slope[i,] = c(states[[i]]$mean[2], states[[i]]$cov[2,2])
    prediction[i] = intercept$mean[i] + slope$mean[i]*leading_leg[i]
    residual[i] = dependent_leg[i]-prediction[i]
  }

  result = data.frame(time=time, intercept=intercept, slope=slope, y_hat=prediction, residual=residual, y=dependent_leg, x=leading_leg)

  attr(result, "kalman.regression.p1") = p1
  attr(result, "kalman.regression.p2") = p2
  attr(result, "kalman.regression.q1") = q1
  attr(result, "kalman.regression.q2") = q2
  attr(result, "kalman.regression.r") = r

  return(result)
}

kalman.regression.attributes = function(model) {
  attrs = attributes(model)
  return(attrs[grep("kalman.*", names(attrs))])
}

summarise.kalman.regression = function(model) {
  attrs = attributes(model)
  return(as.data.frame(attrs[grep("kalman.*", names(attrs))]))
}

#' Plot the Kalman regression result
#' @export plot.kalman.regression
plot.kalman.regression = function(model, plot_data=FALSE) {

  graph_count = 3
  if (plot_data) {
    graph_count = 5
  }

  layout(1:graph_count)

  time = model$time

  if (plot_data) {
    plot(time, model$x, type="l", ylab="X")
    plot(time, model$y, type="l", ylab="Y")
  }

  plot(time, model$intercept.mean, type="l", ylab="Intercept", xlab="Time")
  abline(0,0)

  title_text = sprintf(
    "P1: %f, P2: %f, Q1: %f, Q2: %f, R: %f",
    attr(model, "kalman.regression.p1"),
    attr(model, "kalman.regression.p2"),
    attr(model, "kalman.regression.q1"),
    attr(model, "kalman.regression.q2"),
    attr(model, "kalman.regression.r")
  )

  title(title_text)

  plot(time, model$slope.mean, type="l", ylab="Slope", xlab="Time")
  abline(0,0)

  plot.bands(time, model$residual)
}

#' @export test.kalman.regression
test.kalman.regression = function(model) {
  return(pgff.test(model$residual))
}

#' @export
rolling.regression = function(data, window_size) {
  N = nrow(data)

  intercept = rep(NA, N)
  slope = rep(NA, N)
  y_hat = rep(NA, N)
  residual = rep(NA, N)

  for (i in window_size:N) {
    sub_data = data[i-window_size+1:window_size,]
    sub_model = lm(sub_data$y ~ sub_data$x)
    intercept[i] = sub_model$coefficients[1]
    slope[i] = sub_model$coefficients[2]

    y_hat[i] = intercept[i] + slope[i]*data$x[i]
    residual[i] = data$y[i] - y_hat[i]

    stopifnot(abs(y_hat[i] - tail(sub_model$fitted.values, 1)) < 1e-9)
    stopifnot(abs(residual[i] - tail(sub_model$residuals, 1)) < 1e-9)
  }

  result = data.frame(time=data$time, intercept=intercept, slope=slope, y_hat=y_hat, residual=residual, x=data$x, y=data$y)

  attr(result, "window.size") = window_size
  return(result)
}

#' @export half.life
half.life = function(z) {
  delta_z = c(diff(z), 0)
  z_model = lm(delta_z ~ z)
  lambda = summary(z_model)$coefficients[2]
  return(-log(2)/lambda)
}

#' @export
clip.count = function(residual, thresholds) {
  if (residual > 0) {
    if (residual >= tail(thresholds, 1)) {
      return(-length(thresholds))
    }
    return(-(which(thresholds>residual)[1]-1))
  }
  if (residual < 0) {
    if (-residual >= tail(thresholds, 1)) {
      return(length(thresholds))
    }
    return(which(thresholds>-residual)[1]-1)
  }
  return(0)
}

#' @export
long.entry.threshold = function(clip_count, thresholds) {
  stopifnot(abs(clip_count) <= length(thresholds))
  if (clip_count >= 0) {
    if (clip_count == length(thresholds)) {
      return(NA)
    }
    return(-thresholds[clip_count+1])
  }
  if (clip_count < 0) {
    if (clip_count == -1) {
      return(0)
    }
    return(thresholds[abs(clip_count)-1])
  }
}

#' @export
short.entry.threshold = function(clip_count, thresholds) {
  stopifnot(abs(clip_count) <= length(thresholds))
  if (clip_count <= 0) {
    if (abs(clip_count) == length(thresholds)) {
      return(NA)
    }
    return(thresholds[abs(clip_count)+1])
  }
  if (clip_count > 0) {
    if (clip_count == 1) {
      return(0)
    }
    return(-thresholds[clip_count-1])
  }
}

#' @export
bandwidth.thresholds = function(bandwidth, max_clip_count, N) {
  result = matrix(nrow=N, ncol = max_clip_count)

  for (i in 1:nrow(result)) {
    result[i,] = c(1:max_clip_count)*bandwidth
  }

  return(result)
}

#' @export
stdev.thresholds = function(stdev, max_clip_count) {
  result = matrix(nrow=length(stdev), ncol = max_clip_count)

  for (i in 1:nrow(result)) {
    result[i,] = c(1:max_clip_count)*stdev[i]
  }

  return(result)
}

#' Execute a pairs strategy.
#'
#' The pricing_data table must have the following
#'
#'     . x
#'     . y
#'     . residual
#'     . slope
#'
#' The thresholds matrix must have the same number of rows as
#' the pricing_data table. The number of columns represents the
#' number of clips. The values in each column represent the residual
#' value that triggers entry or exit from that clip
#'
#' @export execute.pairs.strategy
execute.pairs.strategy = function(pricing_data, thresholds) {
  N = nrow(pricing_data)
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

  time = pricing_data$time
  max_clip_count = ncol(thresholds)

  short_threshold[1] = thresholds[1,1]
  long_threshold[1] = -thresholds[1,1]

  prev_clip_count = 0
  prev_position = 0
  for (i in 1:N) {
    residual[i] = pricing_data$residual[i]
    hedge_ratio[i] = pricing_data$slope[i]
    x_price[i] = pricing_data$x[i]
    y_price[i]= pricing_data$y[i]

    if (prev_clip_count > -max_clip_count && residual[i] >= short_threshold[i]) {
      clip_count[i] = clip.count(residual[i], thresholds[i,])
    } else if (prev_clip_count < max_clip_count && residual[i] <= long_threshold[i]) {
      clip_count[i] = clip.count(residual[i], thresholds[i,])
    } else {
      clip_count[i] = prev_clip_count
    }
    position[i] = clip_count[i]/max_clip_count
    position_delta[i] = position[i] - prev_position
    if (i < N) {
      short_threshold[i+1] = short.entry.threshold(clip_count[i], thresholds[i,])
      long_threshold[i+1] = long.entry.threshold(clip_count[i], thresholds[i,])
    }
    prev_clip_count = clip_count[i]
    prev_position = position[i]
  }

  # Calculate P&L for each leg
  x_account = evaluate.leg(position, x_price, rep(1, N))
  y_account = evaluate.leg(position, y_price, -hedge_ratio)

  # Calculate total P&L
  pnl = x_account$pnl + y_account$pnl
  pnl_delta = c(0, diff(pnl))

  result = data.frame(
    time=time,
    residual=residual,
    position=position,
    position_delta=position_delta,
    hedge_ratio=hedge_ratio,
    x_price=x_price,
    y_price=y_price,
    pnl=pnl,
    pnl_delta=pnl_delta
  )

  return(result)
}

###############################################################################
# Factory function for leg account
###############################################################################
build.account = function(previous_cash, realised_cash, mtm, clip_count, leg_qty, leg_entry_ratio, leg_entry_price, leg_spot_price, leg_spot_ratio) {
  return(data.frame(cash=previous_cash+realised_cash, cash_delta=realised_cash, mtm=mtm, clip_count=clip_count, leg_qty=leg_qty, leg_entry_ratio=leg_entry_ratio, leg_entry_price=leg_entry_price, leg_spot_price=leg_spot_price, leg_spot_ratio=leg_spot_ratio))
}

#' Update account
#' @export update.account
update.account = function(prev_account, clip_count, leg_price, leg_ratio) {

  # Calculate the leg entry ratio
  leg_entry_ratio = leg_ratio
  leg_qty = clip_count * leg_entry_ratio

  previous_cash = prev_account$cash

  if (leg_qty*prev_account$leg_qty < 0) {
    # There has been a switch from one side to the other. This means a change in the cash position as
    # the old position is closed and the new one opened. Here, we work out the realised P&L. We also
    # set prev_leg_qty to 0 so that, below, it appears that we're entering a new position from flat.
    realised_cash = prev_account$leg_qty*(leg_price-prev_account$leg_entry_price)
    prev_leg_qty = 0
  } else {
    realised_cash = 0
    prev_leg_qty = prev_account$leg_qty
  }
  leg_qty_delta = leg_qty - prev_leg_qty

  stopifnot(!is.na(leg_qty_delta))

  if (leg_qty_delta == 0) {
    result = prev_account
    result$leg_spot_ratio = leg_ratio
    result$leg_spot_price = leg_price
    result$cash = previous_cash + realised_cash
    result$cash_delta = realised_cash
    result$leg_qty = leg_qty
    if (prev_leg_qty != 0) {
      # Have to mark-to-market current holding
      result$mtm = leg_qty*(leg_price-prev_account$leg_entry_price)
    }
    return(result)
  }
  stopifnot(leg_qty_delta != 0)
  stopifnot(!is.na(leg_price))

  if (leg_qty == 0) {
    stopifnot(clip_count == 0)
    stopifnot(realised_cash == 0)
    realised_cash = prev_leg_qty*(leg_price-prev_account$leg_entry_price)
    return(build.account(previous_cash, realised_cash, mtm=0, clip_count, leg_qty, leg_entry_ratio=NA, leg_entry_price=NA, leg_price, leg_ratio))
  }

  stopifnot(leg_qty != 0 && clip_count != 0)

  if (prev_leg_qty == 0) {
    # Entering a position from flat
    # There may be realised cash from switching from one side to the other.
    leg_entry_price = leg_price
    return(build.account(previous_cash, realised_cash, mtm=0, clip_count, leg_qty, leg_entry_ratio, leg_entry_price, leg_price, leg_ratio))
  } else {

    leg_side = sign(leg_qty)
    stopifnot(leg_side != 0)

    if (leg_side*leg_qty_delta > 0) {
      # Increasing an existing actual leg position
      # Average entry price changes
      # Cash position stays the same
      leg_entry_price = (abs(prev_leg_qty) * prev_account$leg_entry_price + abs(leg_qty_delta) * leg_price)/abs(leg_qty)
      mtm = (leg_price-leg_entry_price)*leg_qty
      return(build.account(previous_cash, realised_cash, mtm, clip_count, leg_qty, leg_entry_ratio, leg_entry_price, leg_price, leg_ratio))
    } else {
      # Reducing an existing actual leg position
      # Average entry price stays the same
      # Cash is realised due to the reduction in the position
      stopifnot(leg_side*leg_qty_delta < 0)
      stopifnot(realised_cash == 0)
      realised_cash = leg_side * abs(leg_qty_delta)*(leg_price-prev_account$leg_entry_price)
      if (leg_qty == 0) {
        leg_entry_price = NA
        leg_entry_ratio = NA
        mtm = 0
      } else {
        leg_entry_price = prev_account$leg_entry_price
        mtm = (leg_price-leg_entry_price)*leg_qty
      }
      return(build.account(previous_cash, realised_cash, mtm, clip_count, leg_qty, leg_entry_ratio, leg_entry_price, leg_price, leg_ratio))
    }
  }
  stopifnot(False) #Unreached
}

#' Evaluate one leg of a trade
#' @export
evaluate.leg = function(clip_count, leg_price, leg_ratio=NULL) {
  N = length(clip_count)

  if (is.null(leg_ratio)) {
    leg_ratio = rep(1, N)
  }

  stopifnot(length(leg_price) == N)
  stopifnot(length(leg_ratio) == N)

  cash = rep(NA,N)
  cash_delta = rep(NA,N)
  mtm = rep(NA,N)
  leg_qty = rep(NA,N)
  leg_entry_ratio = rep(NA,N)
  leg_entry_price = rep(NA,N)
  leg_spot_price = rep(NA,N)
  leg_spot_ratio = rep(NA,N)

  prev_account = list(cash=0, cash_delta=0, clip_count=0, mtm=0, leg_qty=0, leg_entry_ratio=NA, leg_entry_price=NA, leg_spot_price=NA, leg_spot_ratio=NA)
  for (i in 1:N) {
    prev_account = update.account(prev_account, clip_count[i], leg_price[i], leg_ratio[i])
    cash[i] = prev_account$cash
    cash_delta[i] = prev_account$cash_delta
    mtm[i] = prev_account$mtm
    leg_qty[i] = prev_account$leg_qty
    leg_entry_ratio[i] = prev_account$leg_entry_ratio
    leg_entry_price[i] = prev_account$leg_entry_price
    leg_spot_price[i] = prev_account$leg_spot_price
    leg_spot_ratio[i] = prev_account$leg_spot_ratio
  }

  return(data.frame(
    cash=cash,
    cash_delta=cash_delta,
    mtm=mtm,
    pnl=cash+mtm,
    clip_count=clip_count,
    leg_qty=leg_qty,
    leg_entry_ratio=leg_entry_ratio,
    leg_entry_price=leg_entry_price,
    leg_spot_price=leg_spot_price,
    leg_spot_ratio=leg_spot_ratio
  ))
}

#' Extract trade boundaries from a position series
#' @export extract.trade.boundaries
extract.trade.boundaries = function(position) {

  trade_start = rep(FALSE, length(position))
  trade_end = rep(FALSE, length(position))

  prev_position = 0
  prev_start = 0
  for (i in 1:length(position)) {
    current_position = position[i]
    if (current_position > 0) {
      if (prev_position <= 0) {
        trade_start[i] = TRUE
        prev_start = i
      }
      if (prev_position < 0) {
        trade_end[i] = TRUE
      }
    } else if (current_position < 0) {
      if (prev_position >= 0) {
        trade_start[i] = TRUE
        prev_start = i
      }
      if (prev_position > 0) {
        trade_end[i] = TRUE
      }
    } else {
      stopifnot(current_position == 0)
      if (prev_position != 0) {
        trade_end[i] = TRUE
        prev_start = 0
      }
    }
    prev_position = current_position
  }

  if (prev_start != 0) {
    # Incomplete trade
    trade_start[prev_start] = FALSE
  }

  return(data.frame(start_idx=which(trade_start), end_idx=which(trade_end)))
}

#' Sharpe ratio
#' @export
sharpe.ratio = function(pnl, years) {
  annualisation_factor = sqrt(length(pnl)/years)
  return(mean(pnl)*annualisation_factor/sd(pnl))
}

#' summarise.pnl
#' @export
summarise.pnl = function(pnl, start_time, end_time) {

  trade_count = length(pnl)

  winners = pnl[pnl >= 0]
  losers = pnl[pnl < 0]

  trade_count = length(pnl)
  winner_count = length(winners)
  loser_count = length(losers)
  win_ratio = winner_count/trade_count

  total_pnl = sum(pnl)
  mean_pnl = mean(pnl)
  sd_pnl = sd(pnl)
  stderr_pnl = sd_pnl/sqrt(trade_count)
  t_pnl = mean_pnl/stderr_pnl
  p_pnl = 2*dt(abs(t_pnl), df=trade_count-1)

  stopifnot(FALSE, "times are POSIXct now not integers, this needs to be fixed")
  years = as.integer(max(end_time) - min(start_time))/365
  sharpe_ratio = sharpe.ratio(pnl, years)

  worst_dollar_drawdown = worst.drawdown(cumsum(pnl), percentage = FALSE)
  worst_pct_drawdown = worst.drawdown(cumsum(pnl), percentage = TRUE)

  return(list(
    trade_count = trade_count,
    winner_count = winner_count,
    loser_count = loser_count,
    win_ratio = win_ratio,
    total_pnl = total_pnl,
    mean_pnl = mean_pnl,
    sd_pnl = sd_pnl,
    sharpe_ratio = sharpe_ratio,
    stderr_pnl = stderr_pnl,
    t_pnl = t_pnl,
    p_pnl = p_pnl,
    worst_dollar_drawdown = worst_dollar_drawdown,
    worst_pct_drawdown = worst_pct_drawdown
  ))
}

summarise.trades = function(trades) {
  unfiltered_summary = summarise.pnl(trades$pnl, trades$start_time, trades$end_time)
  filtered_summary = summarise.pnl(trades$filtered_pnl, trades$start_time, trades$end_time)

  return(t(rbind(unfiltered_summary, filtered_summary)))
}

extract.trade.excursion = function(execution, x_account, y_account, start_idx, end_idx) {
  result = execution[start_idx:end_idx,c("time","residual","position","hedge_ratio")]

  result$x = x_account[start_idx:end_idx, c("leg_entry_price", "leg_spot_price", "leg_qty", "cash", "cash_delta", "mtm")]
  result$y = y_account[start_idx:end_idx, c("leg_entry_price", "leg_spot_price", "leg_qty", "cash", "cash_delta", "mtm")]

  result$x$cash = result$x$cash-result$x$cash[1]
  result$y$cash = result$y$cash-result$y$cash[1]

  result$x$cash_delta[1] = 0
  result$y$cash_delta[1] = 0

  stopifnot(all.equal(sum(result$x$cash_delta), tail(result$x$cash,n=1)))

  result$cash_delta = result$x$cash_delta + result$y$cash_delta
  result$cash = result$x$cash + result$y$cash
  result$mtm = result$x$mtm + result$y$mtm
  result$pnl = result$cash + result$mtm

  N = end_idx-start_idx+1

  result$position[N] = 0
  result$hedge_ratio[N] = NA

  side = sign(result$position[1])
  if (!(side != 0 && all(sign(result$position[1:N-1]) == side))) {
    stopifnot(FALSE)
  }

  result$x$leg_qty[N] = 0
  result$x$leg_entry_price[N] = NA

  result$y$leg_qty[N] = 0
  result$y$leg_entry_price[N] = NA

  return(result)
}

#' max.loss.filter
#' @export max.loss.filter
max.loss.filter = function(execution, max_loss) {

  stopifnot(max_loss > 0)

  filter = function(trade) {

    start_idx = trade$start_idx
    end_idx = trade$end_idx

    excursion = execution[start_idx:end_idx,,drop=FALSE]
    excursion$pnl = excursion$pnl - excursion$pnl[1]

    filtered_holding_period = trade$holding_period
    filtered_pnl = trade$pnl

    condition = excursion$pnl <= -max_loss
    if (any(condition)) {
      # Where did the stop-loss trigger?
      stop_idx = which.max(condition)
      # What is the P&L after the stop loss?
      filtered_pnl = excursion$pnl[stop_idx]
      # What is the modified holding period?
      filtered_holding_period = stop_idx-1
    }

    filtered_end_idx = trade$start_idx+filtered_holding_period
    end_time = excursion$time[filtered_holding_period+1]

    result = list(end_idx=filtered_end_idx, end_time=end_time, pnl=filtered_pnl)
    return(result)
  }
  return(filter)
}

# holding.period.filter = function(trade, max_holding_period) {
#   filtered_holding_period = min(trade$holding_period, max_holding_period)
#   filtered_end_idx = trade$start_idx+filtered_holding_period
#   filtered_pnl = trade$excursion[[1]]$pnl[filtered_holding_period+1]
#   result = list(end_idx=filtered_end_idx, pnl=filtered_pnl)
#   return(result)
# }

#' apply.stop.filter
#' @export apply.stop.filter
apply.stop.filter = function(trades, stop_filter) {
  # First, work out the new end_idx of each trade
  trades$filtered_end_idx = trades$end_idx
  trades$filtered_end_time = trades$end_time
  trades$filtered_pnl = trades$pnl
  for (i in 1:nrow(trades)) {
    trade = trades[i,]
    filtered_result = stop_filter(trade)
    trades$filtered_end_idx[i] = filtered_result$end_idx
    trades$filtered_end_time[i] = filtered_result$end_time
    trades$filtered_pnl[i] = filtered_result$pnl
  }
  trades$filtered_holding_period = trades$filtered_end_idx - trades$start_idx
  return(trades)
}

#' Extract trades from an execution report
#' @export extract.trades
extract.trades = function(execution) {

  trade_boundaries = extract.trade.boundaries(execution$position)

  trades = data.frame(start_idx = trade_boundaries$start_idx, end_idx = trade_boundaries$end_idx, filtered_end_idx = trade_boundaries$end_idx)

  trades$pnl = execution$pnl[trades$end_idx] - execution$pnl[trades$start_idx]
  trades$filtered_pnl = trades$pnl

  trades$holding_period = trades$end_idx-trades$start_idx
  trades$filtered_holding_period = trades$holding_period

  trades$start_time = execution$time[trades$start_idx]
  trades$end_time = execution$time[trades$end_idx]

  max_adverse_excursion = vector(mode="numeric", length=nrow(trades))

  if (nrow(trades) > 0) {
    for (i in 1:nrow(trades)) {
      trade_excursion = execution$pnl[trades$start_idx[i]:trades$end_idx[i]]
      trade_excursion = trade_excursion - trade_excursion[1]
      max_adverse_excursion[i] = min(trade_excursion)
    }
  }

  trades$side = sign(execution$position[trades$start_idx])
  trades$mae = max_adverse_excursion

  stopifnot(all(trades$side != 0))

  return(trades)
}

trades.execution = function(trades) {
  return(attr(trades, "trades.execution"))
}

#' @export plot.trades.equity.curve
plot.trades.equity.curve = function(start_time, end_time, trade_pnl, trade_filtered_pnl) {

  time = c(start_time[1], end_time)

  unfiltered_equity = c(0, cumsum(trade_pnl))
  filtered_equity = c(0, cumsum(trade_filtered_pnl))

  ylim = c(min(0, unfiltered_equity, filtered_equity), max(c(0, unfiltered_equity, filtered_equity)))

  plot(time, unfiltered_equity, type="n", ylim=ylim)
  lines(time, filtered_equity, type='o', col='green')
  lines(time, unfiltered_equity, type='o', col='blue')
  abline(0, 0, col="red")
}

#' @export plot.trades.histogram
plot.trades.histogram = function(pnl, filtered_pnl) {
  #----------------------------------------------------------------------------
  # Histogram of P&L
  # layout(1:2)

  xlim = c(min(pnl), max(pnl))

  hist(pnl, xlim=xlim, freq=FALSE)
  hist(filtered_pnl, xlim=xlim, freq=FALSE)
}

#' @export plot.trades.density
plot.trades.density = function(trades) {
  #----------------------------------------------------------------------------
  # Histogram of P&L
  layout(1:2)
  plot(density(trades$pnl))
  plot(density(trades$filtered_pnl))
}

#' @export start.idx
start.idx = function(start_idx, N) {
  if (is.na(start_idx)) {
    start_idx = 1
  }
  if (start_idx < 0) {
    start_idx = max(1, N+start_idx+1)
  }
  return(start_idx)
}

#' @export end.idx
end.idx = function(end_idx, N) {
  if (is.na(end_idx)) {
    end_idx = N
  }
  if (end_idx < 0) {
    end_idx = max(1, N+end_idx)
  }
  return(end_idx)
}

#' @export plot.trades.entry.exit
plot.trades.entry.exit = function(trades, start_idx=1, end_idx=nrow(trades)) {
  execution = trades.execution(trades)
  kalman_regression = execution.kalman.regression(execution)
  bandwidth = execution.bandwidth(execution)

  start_idx = start.idx(start_idx, nrow(trades))
  end_idx = end.idx(end_idx, nrow(trades))

  trades = trades[start_idx:end_idx,]

  residuals_start_idx = max(1, trades$start_idx[1] - 5)
  residuals_end_idx = tail(trades$end_idx, 1) + 5

  x_label = kalman_regression$time[residuals_start_idx:residuals_end_idx]

  plot(x_label, kalman_regression$residual[residuals_start_idx:residuals_end_idx], type="o", ylab="Residuals", xlab="Time")
  abline(0,0)

  for (i in -2:2) {
    abline(i*bandwidth,0,col="blue", lty=2)
  }

  for (i in 1:nrow(trades)) {
    trade = trades[i,]
    start_time = trade$start_time
    start_residual = trade$start_residual + 0.001
    points(start_time, start_residual, col="green", pch=19)

    filtered_end_time = trade$filtered_end_time
    filtered_end_residual = trade$filtered_end_residual - 0.001
    points(filtered_end_time, filtered_end_residual, col="orange", pch=19)

    end_time = trade$end_time
    end_residual = trade$end_residual - 0.001
    points(end_time, end_residual, col="red", pch=19)
  }
}

# library(ggplot2)
#
# gg.trades.density = function(trades, binwidth) {
#   pnl1 = data.frame(trade=rep("Unfiltered", nrow(trades)), pnl=trades$pnl)
#   pnl2 = data.frame(trade=rep("Filtered", nrow(trades)), pnl=trades$filtered_pnl)
#   pnl = rbind(pnl1, pnl2)
#
#   ggplot(pnl, aes(pnl, fill=trade)) + geom_histogram(position="identity", colour="grey40", binwidth = binwidth) + facet_grid(trade ~ .)
#
#   # summary1 = summarise.trades(trades1)
#   # summary2 = summarise.trades(trades2)
#
#   # summary_columns = c("mean_pnl", "sd_pnl")
#   #
#   # summary = rbind(summary1[summary_columns], summary2[summary_columns])
#   # summary = cbind(data.frame(names=c(name1, name2)), summary)
#   #
#   # return(summary)
# }

#' @export plot.holding.period
plot.holding.period = function(pnl, holding_period, xlim=NULL, ylim=NULL) {

  #----------------------------------------------------------------------------
  # Scatter plot of winners and losers versus holding period
  winners = pnl >= 0
  winners_pnl = pnl[winners]
  winners_holding_period = holding_period[winners]

  losers = pnl < 0
  losers_pnl = pnl[losers]
  losers_holding_period = holding_period[losers]

  # Set up drawing area without drawing anything
  plot(holding_period, pnl, type="n", xlim=xlim, ylim=ylim)
  # Draw winners
  points(winners_holding_period, winners_pnl, type="p", col="blue")
  # Draw losers
  points(losers_holding_period, losers_pnl, type="p", col="red")

}

#' @export plot.trades.holding.period
plot.trades.holding.period = function(trades) {
  layout(1:2)
  xlim = c(min(trades$holding_period), max(trades$holding_period))
  ylim = c(min(trades$pnl), max(trades$pnl))
  plot.holding.period(trades$pnl, trades$holding_period, xlim=xlim)
  plot.holding.period(trades$filtered_pnl, trades$filtered_holding_period, xlim=xlim, ylim=ylim)
}

#' @export plot.trades.mae
plot.trades.mae = function(trades) {

  #----------------------------------------------------------------------------
  # Scatter plot of winners and losers versus holding period
  winners = trades[trades$pnl >= 0,]
  losers = trades[trades$pnl < 0,]

  # Set up drawing area without drawing anything
  plot(trades$mae, trades$pnl, type="n")
  # Draw winners
  points(winners$mae, winners$pnl, type="p", col="blue")
  # Draw losers
  points(losers$mae, losers$pnl, type="p", col="red")
}

#' @export trades.per.day
trades.per.day = function(trades, start_time, end_time) {
  start_time = start_time[1]
  end_time = tail(end_time, 1)
  total_days = end_time-start_time
  return(nrow(trades)/as.integer(total_days))
}

#' @export trades.per.year
trades.per.year = function(trades) {
  return(trunc(trades.per.day(trades, trades$start_time, trades$end_time)*365))
}

#' @export drawdowns
drawdowns = function(cumulative_pnl) {
  result = cumulative_pnl
  previous_max = 0
  for (i in 1:length(cumulative_pnl)) {
    current_pnl = cumulative_pnl[i]
    previous_max = max(previous_max, current_pnl)
    result[i] = min(0, current_pnl-previous_max)
  }
  return(result)
}

#' @export worst.drawdown
worst.drawdown = function(cumulative_pnl, percentage) {
  all_drawdowns = drawdowns(cumulative_pnl)
  start_idx = which(diff(c(0, sign(all_drawdowns))) < 0)-1
  end_idx = which(c(0, diff(c(sign(all_drawdowns), 0))) > 0)-1
  result = vector("numeric", length(start_idx))
  stopifnot(length(start_idx) == length(end_idx))
  if (length(result) == 0) {
    return(0)
  }
  for (i in 1:length(result)) {
    result[i] = min(all_drawdowns[start_idx[i]:end_idx[i]])
    if (percentage) {
      result[i] = result[i]/cumulative_pnl[start_idx[i]]
    }
  }
  return(min(result))
}

sample.trades = function(trades, size) {
  if (is.null(size)) {
    size = nrow(trades)
  }
  rows = sample(size, replace=TRUE)
  return(trades[rows,])
}

#' Apply position sizing algorithm
#' @export apply.position.sizing
apply.position.sizing = function(pnl, stop_loss, initial_capital, bet_ratio, lot_value, dv01) {
  cum_pnl = cumsum(pnl)

  N = length(pnl)

  start_capital = rep(NA, N)
  end_capital = rep(NA, N)
  max_lots = rep(NA, N)
  bet_lots = rep(NA, N)
  bet_pnl = rep(NA, N)
  capital_used = rep(NA, N)
  max_dollar_loss = rep(NA, N)

  prev_capital = initial_capital

  for (i in 1:N) {
    start_capital[i] = prev_capital
    max_dollar_loss[i] = bet_ratio*start_capital[i]
    max_lots[i] = start_capital[i]/lot_value
    bet_lots[i] = min(max_dollar_loss[i]/(100*stop_loss*dv01), max_lots[i])
    bet_pnl[i] = bet_lots[i]*pnl[i]*dv01*100
    end_capital[i] = max(0, start_capital[i]+bet_pnl[i])
    prev_capital = end_capital[i]
  }

  return(data.frame(
    start_capital=start_capital,
    max_dollar_loss=max_dollar_loss,
    max_lots=max_lots,
    bet_lots=bet_lots,
    bet_pnl=bet_pnl,
    end_capital=end_capital
  ))
}

summarise.equity.curve = function(capital, years) {
  worst_dollar_drawdown = worst.drawdown(capital, percentage = FALSE)
  worst_pct_drawdown = worst.drawdown(capital, percentage = TRUE)
  start_capital = capital[1]
  end_capital = tail(capital, n = 1)
  sharpe_ratio = sharpe.ratio(c(diff(log(capital))), years)
  dollar_sharpe = sharpe.ratio(c(diff(capital)), years)
  return(list(
    worst_dollar_drawdown = worst_dollar_drawdown,
    worst_pct_drawdown = worst_pct_drawdown,
    start_capital = start_capital,
    end_capital = end_capital,
    sharpe_ratio = sharpe_ratio,
    dollar_sharpe = dollar_sharpe,
    final_return = (end_capital-start_capital)/start_capital
  ))
}

generate.risk.paths = function(trades, path_length, path_count, stop_loss, initial_capital, bet_ratio, lot_value, dv01, path_years) {
  # Generates a list of 'path_count' risk paths. Each path  consists of 'path_length' trades, where each trade
  # is sized according to the given 'bet_ratio'
  sample_paths = vector("list", length = path_count)
  for (i in 1:path_count) {
    random_trades = sample.trades(trades, path_length)
    sample_paths[[i]] = apply.position.sizing(random_trades$filtered_pnl, stop_loss, initial_capital, bet_ratio, lot_value, dv01)
  }

  attr(sample_paths, "risk_paths.bet_ratio") = bet_ratio
  attr(sample_paths, "risk_paths.path_length") = path_length
  attr(sample_paths, "risk_paths.initial_capital") = initial_capital
  attr(sample_paths, "risk_paths.lot_value") = lot_value
  attr(sample_paths, "risk_paths.dv01") = dv01
  attr(sample_paths, "risk_paths.path_years") = path_years

  return(sample_paths)
}

plot.risk.paths = function(paths) {
  stopifnot(length(paths) > 0)
  start_capital = paths[[1]]$start_capital[1]
  ymin = start_capital
  ymax = start_capital
  for (i in 1:length(paths)) {
    ymin = min(ymin, min(paths[[i]]$end_capital))
    ymax = max(ymax, max(paths[[i]]$end_capital))
  }
  plot(c(start_capital, paths[[1]]$end_capital), ylim = c(ymin, ymax), type='n')
  for (i in 1:length(paths)) {
    lines(c(start_capital, paths[[i]]$end_capital))
  }
}

analyse.risk.paths = function(risk_paths) {
  sample_count = length(risk_paths)
  worst_dollar_drawdown = rep(NA, sample_count)
  worst_pct_drawdown = rep(NA, sample_count)
  start_capital = rep(NA, sample_count)
  end_capital = rep(NA, sample_count)
  final_return = rep(NA, sample_count)
  dollar_sharpe = rep(NA, sample_count)
  sharpe_ratio = rep(NA, sample_count)
  for (i in 1:sample_count) {
    summary = summarise.equity.curve(risk_paths[[i]]$end_capital, attr(risk_paths, "risk_paths.path_years"))
    worst_dollar_drawdown[i] = summary$worst_dollar_drawdown
    worst_pct_drawdown[i] = summary$worst_pct_drawdown
    start_capital[i] = summary$start_capital
    end_capital[i] = summary$end_capital
    final_return[i] = summary$final_return
    dollar_sharpe[i] = summary$dollar_sharpe
    sharpe_ratio[i] = summary$sharpe_ratio
  }
  return(data.frame(
    worst_dollar_drawdown=worst_dollar_drawdown,
    worst_pct_drawdown=worst_pct_drawdown,
    end_capital=end_capital,
    sharpe_ratio=sharpe_ratio,
    dollar_sharpe=dollar_sharpe,
    final_return=final_return
  ))
}

plot.risk.paths.distribution = function(paths) {
  layout(1:2)
  analysis = analyse.risk.paths(paths)
  start_capital = analysis$start_capital[1]

  summary = summarise.risk.paths(paths)

  final_return = analysis$final_return*100
  plot(density(final_return, from=min(final_return), to=max(final_return)), main="Return (%)")
  abline(v = summary$return_05*100, col="blue", lty=2)
  abline(v = summary$return_95*100, col="green", lty=2)

  worst_pct_drawdown = analysis$worst_pct_drawdown*100

  plot(density(worst_pct_drawdown, from=min(worst_pct_drawdown), to=max(worst_pct_drawdown), n=20), main="Max Drawdown (%)")
  abline(v = summary$pdd_05*100, col="blue", lty=2)
  abline(v = summary$pdd_95*100, col="green", lty=2)
}

summarise.risk.paths = function(risk_paths) {
  analysis = analyse.risk.paths(risk_paths)
  return_min = min(analysis$final_return)
  return_mean = mean(analysis$final_return)
  return_max = max(analysis$final_return)
  return_05 = quantile(analysis$final_return, c(0.05))
  return_50 = quantile(analysis$final_return, c(0.50))
  return_95 = quantile(analysis$final_return, c(0.95))

  pdd_min = min(analysis$worst_pct_drawdown)
  pdd_mean = mean(analysis$worst_pct_drawdown)
  pdd_max = max(analysis$worst_pct_drawdown)
  pdd_05 = quantile(analysis$worst_pct_drawdown, c(0.05))
  pdd_50 = quantile(analysis$worst_pct_drawdown, c(0.50))
  pdd_95 = quantile(analysis$worst_pct_drawdown, c(0.95))

  ddd_min = min(analysis$worst_dollar_drawdown)
  ddd_mean = mean(analysis$worst_dollar_drawdown)
  ddd_max = max(analysis$worst_dollar_drawdown)
  ddd_05 = quantile(analysis$worst_dollar_drawdown, c(0.05))
  ddd_50 = quantile(analysis$worst_dollar_drawdown, c(0.50))
  ddd_95 = quantile(analysis$worst_dollar_drawdown, c(0.95))

  sharpe_min = min(analysis$sharpe_ratio)
  sharpe_mean = mean(analysis$sharpe_ratio)
  sharpe_max = max(analysis$sharpe_ratio)
  sharpe_05 = quantile(analysis$sharpe_ratio, c(0.05))
  sharpe_50 = quantile(analysis$sharpe_ratio, c(0.50))
  sharpe_95 = quantile(analysis$sharpe_ratio, c(0.95))

  return(data.frame(
    return_05=return_05, return_50=return_50, return_95=return_95, return_mean=return_mean, return_min=return_min, return_max=return_max,
    pdd_05=pdd_05, pdd_50=pdd_50, pdd_95=pdd_95, pdd_mean=pdd_mean, pdd_min=pdd_min, pdd_max=pdd_max,
    ddd_05=ddd_05, ddd_50=ddd_50, ddd_95=ddd_95, ddd_mean=ddd_mean, ddd_min=ddd_min, ddd_max=ddd_max,
    sharpe_50=sharpe_50, sharpe_05=sharpe_05, sharpe_95=sharpe_95, sharpe_mean=sharpe_mean, sharpe_min=sharpe_min, sharpe_max=sharpe_max
  ))
}

analyse.risk.of.ruin = function(risk_paths, p_ruin=NA) {

  if (is.na(p_ruin)) {
    p_ruin = attr(risk_paths, "risk.of.ruin")
    stopifnot(!is.na(p_ruin))
  }

  analysis = analyse.risk.paths(risk_paths)

  return_05 = quantile(analysis$final_return, 0.05)
  return_50 = quantile(analysis$final_return, 0.50)
  return_95 = quantile(analysis$final_return, 0.95)

  pdd_ruin = quantile(analysis$worst_pct_drawdown, p_ruin)
  pdd_min = min(analysis$worst_pct_drawdown)

  ddd_ruin = quantile(analysis$worst_dollar_drawdown, p_ruin)
  ddd_min = min(analysis$worst_dollar_drawdown)

  return(data.frame(
    return_05=return_05, return_50=return_50, return_95=return_95,
    pdd_ruin=pdd_ruin, pdd_min=pdd_min,
    ddd_ruin=ddd_ruin, ddd_min=ddd_min
  ))
}

compare.risk.path.sets = function(path_sets) {

  N = length(path_sets)
  stopifnot(N >= 2)

  result = summarise.risk.paths(path_sets[[1]])

  for (i in 2:N) {
    result = rbind(result, summarise.risk.paths(path_sets[[i]]))
  }
  rownames(result) = c(1:N)
  return(result)
}

pct.string = function(value) {
  return(sprintf("%.2f%%", 100*value))
}

generate.risk.comparison = function(model, bandwidth, lot_value, dv01, max_clip_count, bet_sizes, years=1, initial_capital=100000) {
  stopifnot(FALSE, "Code is out of date")

  # This is out of date
  execution = execute.pairs.strategy(model, bandwidth, max_clip_count)

  trades = extract.trades(execution, max.loss.filter, stop_filter = NULL)
  path_length = years*trades.per.year(trades)

  N = length(bet_sizes)
  result = vector("list", N)
  for (i in 1:N) {
    result[[i]] = generate.risk.paths(trades, path_length, 1000, stop_loss, initial_capital, bet_sizes[i], lot_value, dv01, 1)
  }

  attr(result, "risk.comparison.model") = model
  attr(result, "risk.comparison.execution") = execution
  attr(result, "risk.comparison.trades") = trades
  attr(result, "risk.comparison.years") = years
  return(result)
}

#' Evaluate a leg expression
#' @export
evaluate.leg.expr = function(expr, data) {
  result = rep(0, nrow(data))
  for (name in names(expr)) {
    result = result + data[,name]*expr[[name]]
  }
  return(result)
}

#' Build decision data used for trading Timmy
#' @export
build.rudolf.trading.data = function(futures_data, x_leg_expr, y_leg_expr, intercept_var, slope_var, sd_lookback, start_time=NULL) {

  x_data = evaluate.leg.expr(x_leg_expr, futures_data)
  y_data = evaluate.leg.expr(y_leg_expr, futures_data)

  regression_data = data.frame(time = futures_data$time, x = x_data, y = y_data)
  regression = kalman.regression(
    regression_data,
    1,
    1,
    intercept_var,
    slope_var,
    1
  )

  # Have to lag the model by one day. We use yesterday's model for today

  # Have to start from second time to lag
  time = regression$time[-1]
  x = regression$x[-1]
  y = regression$y[-1]
  intercept = regression$intercept.mean[-nrow(regression)]
  slope = regression$slope.mean[-nrow(regression)]

  y_hat = intercept + slope*x
  residual = y - y_hat

  residual_stdev = msd(sd_lookback, n=2, t=futures_data$numeric.time, z=residual)

  result = data.frame(
    time=time,
    intercept=intercept,
    slope=slope,
    x=x, y=y, y_hat=y_hat,
    residual=residual,
    residual.stdev=residual_stdev
  )

  if (!is.null(start_time)) {
    result = subset(result, time >= start_time)
  }

  return(result)
}

# build.timmy.regression.model = function(futures_data, window_size, start_time=NULL) {
#   timmy_data = data.frame(time = futures_data$time, x = futures_data$IR03 - futures_data$IR06, y = futures_data$XT01)
#   regression = rolling.regression(
#     timmy_data,
#     window_size
#   )
#   if (!is.null(start_time)) {
#     regression = subset(regression, time >= start_time)
#   }
#
#   result = data.frame(time=regression$time, x=regression$x, y=regression$y, residual=regression$residual, slope=regression$slope)
#   attr(result, "regression.model") = regression
#   return(result)
# }

timmy.trade.analysis = function(
  futures_prices,
  start_time,
  intercept_var,
  slope_var,
  bandwidth,
  stop_loss,
  max_clip_count,
  lot_value,
  dv01,
  bet_sizes=c(1:5)*0.01,
  years=1,
  initial_capital=100000,
  print_summary=TRUE
) {
  stopifnot(FALSE, "Following code is out of date")
  model = build.timmy.trading.data(futures_prices, intercept_var, slope_var, start_time)
  result = generate.risk.comparison(model, bandwidth, stop_loss, lot_value, dv01, max_clip_count, bet_sizes, years=years, initial_capital=initial_capital)
  timmy_params = data.frame(
    start_time=start_time,
    intercept_var=intercept_var,
    slope_var=slope_var,
    bandwidth=bandwidth,
    stop_loss=stop_loss,
    max_clip_count=max_clip_count,
    lot_value=lot_value,
    dv01=dv01,
    years=years
  )
  attr(result, "strategy.params") = timmy_params
  if (print_summary) {
    print(summarise.risk.comparison(result))
  }
  return(result)
}

timmy.trade.solve.for.risk = function(
  futures_prices,
  start_time,
  intercept_var,
  slope_var,
  bandwidth,
  stop_loss,
  max_clip_count,
  lot_value,
  dv01,
  initial_capital,
  years,
  max_loss,
  risk_of_ruin,
  stopping_tolerance=0.001,
  path_count=10000,
  print_result=TRUE
) {

  stopifnot(FALSE, "Following code is out of date")

  stopifnot(max_loss > 0)
  dd_worst = -max_loss

  model = build.timmy.trading.data(futures_prices, intercept_var, slope_var, start_time)

  # This is out of date
  execution = execute.pairs.strategy(model, bandwidth, max_clip_count)

  trades = extract.trades(execution, max.loss.filter, stop_loss)
  trades_per_year = trades.per.year(trades)
  path_length = round(years*trades.per.year(trades))
  stopifnot(path_length > 0)

  risk.ratio.drawdown = function(risk_ratio) {
    risk_paths = generate.risk.paths(trades, path_length, path_count, stop_loss, initial_capital, risk_ratio, lot_value, dv01, 1)
    analysis = analyse.risk.paths(risk_paths)
    dd = quantile(analysis$worst_dollar_drawdown, risk_of_ruin)
    return(dd)
  }

  rr_high = 1
  rr_low = 0

  cat("Getting initial DD boundaries\n")

  dd_high = risk.ratio.drawdown(rr_high)
  dd_low = risk.ratio.drawdown(rr_low)

  iteration = 0
  repeat {
    iteration = iteration + 1
    cat(sprintf("Iteration=%d\n", iteration))
    cat(sprintf("rr_high=%f, rr_low=%f, dd_high=%f, dd_low=%f\n", rr_high, rr_low, dd_high, dd_low))
    if (rr_high - rr_low < stopping_tolerance) {
      break
    }
    stopifnot(dd_high <= dd_worst && dd_worst <= dd_low)
    rr_mid = (rr_high+rr_low)/2
    dd_mid = risk.ratio.drawdown(rr_mid)
    if (dd_mid <= dd_high) {
      cat("Widening dd_high\n")
      dd_high = dd_mid
      next
    }
    if (dd_mid >= dd_low) {
      cat("Widening dd_low\n")
      dd_low = dd_mid
      next
    }
    stopifnot(dd_high < dd_mid && dd_mid < dd_low)
    if (dd_mid < dd_worst) {
      cat("Reducing rr_high\n")
      rr_high = rr_mid
      dd_high = dd_mid
    } else if (dd_mid > dd_worst) {
      cat("Increasing rr_low\n")
      rr_low = rr_mid
      dd_low = dd_mid
    } else {
      stopifnot(FALSE)
    }
  }
  rr_optimal = (rr_high+rr_low)/2
  paths = generate.risk.paths(trades, path_length, path_count, stop_loss, initial_capital, rr_optimal, lot_value, dv01, 1)

  timmy_params = data.frame(
    start_time=start_time,
    intercept_var=intercept_var,
    slope_var=slope_var,
    bandwidth=bandwidth,
    stop_loss=stop_loss,
    max_clip_count=max_clip_count,
    lot_value=lot_value,
    dv01=dv01,
    years=years
  )

  attr(paths, "risk_paths.model") = model
  attr(paths, "risk_paths.execution") = execution
  attr(paths, "strategy.params") = timmy_params

  if (print_result) {
    cat(sprintf("Optimal risk ratio: %f\n", rr_optimal))
  }

  return(paths)
}

summarise.risk.comparison = function(risk_comparison) {

  comparison = compare.risk.path.sets(risk_comparison)
  row.names(comparison) = pct.string(sapply(risk_comparison, attr, "risk_paths.bet_ratio"))

  comparison$return_50 = pct.string(comparison$return_50)
  comparison$return_05 = pct.string(comparison$return_05)
  comparison$return_95 = pct.string(comparison$return_95)
  comparison$return_mean = pct.string(comparison$return_mean)
  comparison$return_min = pct.string(comparison$return_min)
  comparison$return_max = pct.string(comparison$return_max)

  comparison$pdd_50 = pct.string(comparison$pdd_50)
  comparison$pdd_05 = pct.string(comparison$pdd_05)
  comparison$pdd_95 = pct.string(comparison$pdd_95)
  comparison$pdd_mean = pct.string(comparison$pdd_mean)
  comparison$pdd_min = pct.string(comparison$pdd_min)
  comparison$pdd_max = pct.string(comparison$pdd_max)

  model_summary = summarise.kalman.regression(risk.comparison.model(risk_comparison))
  return(list(comparison = comparison, model=model_summary, params=attr(risk_comparison, "risk.comparison.params")))
}

risk.comparison.trades = function(risk_comparison) {
  return(attr(risk_comparison, "risk.comparison.trades"))
}

risk.comparison.model = function(risk_comparison) {
  return(attr(risk_comparison, "risk.comparison.model"))
}

risk.comparison.execution = function(risk_comparison) {
  return(attr(risk_comparison, "risk.comparison.execution"))
}


plot.pca.factor = function(pca, i, start_idx=1) {
  layout(1:2)

  factor_loadings = pca$rotation[,i]

  start_idx = start.idx(start_idx, nrow(pca$x))

  ylim = c(min(0, min(factor_loadings)), max(0, max(factor_loadings))*1.1)
  plot(pca$rotation[,i], type='o', ylim=ylim)
  abline(0,0)
  plot(pca$x[start_idx:nrow(pca$x),i], type='o')
  abline(0,0)
}

rolling.pca = function(input_matrix, window_length) {
  N = trunc(nrow(input_matrix)/window_length)

  result = matrix(nrow=N, ncol = ncol(input_matrix))
  for (i in seq(1, N)) {
    start_idx = (i-1)*window_length + 1
    end_idx = start_idx + window_length - 1
    window_data = input_matrix[start_idx:end_idx,]
    pca = prcomp(window_data)
    result[i,] = (pca$sdev^2)/sum(pca$sdev^2)
  }
  return(result)
}

#' Unscaling of scaled data
#' @export unscale
unscale = function(data, center=NULL, scale=NULL) {
  stopifnot(length(dim(data)) == 2)

  if (is.null(center)) {
    center = attr(data, "scaled:center")
  }
  if (is.null(scale)) {
    scale = attr(data, "scaled:scale")
  }

  unscaled_data = t(apply(data, 1, function(row) row*scale+center))

  # Removing scaling attributes, if they exist. It's harmless
  # if they don't.
  attr(unscaled_data, "scaled:center") = NULL
  attr(unscaled_data, "scaled:scale") = NULL

  dim(unscaled_data) = dim(data)

  return(unscaled_data)
}

#' build.curve.model
#' @export
build.curve.model = function(curve_data) {
  contract_data = curve_data[,contract.names(curve_data)]
  pca = prcomp(contract_data, center=TRUE, scale=TRUE, retx=TRUE)
  attr(pca, "contract.names") = contract.names(curve_data)
  attr(pca, "times") = curve_data$time
  return(pca)
}

curve.model.nrow = function(curve_model) {
  return(nrow(curve_model$x))
}

curve.model.leg.count = function(curve_model) {
  return(length(contract.names(curve_model)))
}

#' curve.model.component.score
#' @export
curve.model.component.score = function(curve_model, component=-1) {
  # -1 means last component

  if (component < 0) {
    component = curve.model.leg.count(curve_model)+component+1
  }
  stopifnot(1 <= component && component <= curve.model.leg.count(curve_model))

  # If the component loading for the first contract is negative, the component
  # score will be a mirror image of what you'd expect if you bought the
  # first contract because the negative loading means sell not buy. Here, we
  # adjust the component value to look as if we're always buying the first
  # contract.
  component_sign = sign(curve_model$rotation[1,component])
  return(component_sign * curve_model$x[,component])
}

#' build.curve.structures
#' @export
build.curve.structure = function(curve_model, contract_names=NULL) {

  # Extract hedge ratios for a set of contracts from a curve structure
  # The contract names may be a subset of the contract names in the curve struture

  # The edge ratios are calculated to make the portfolio of contracts
  # insensitive to the first N-1 components of the curve structure where N
  # is the number of contract in the portfolio

  if (is.null(contract_names)) {
    contract_names = contract.names(curve_model)
  }
  stopifnot(!is.null(contract_names))

  N = length(contract_names)

  scale = curve_model$scale[contract_names]

  rotation = curve_model$rotation[contract_names,]

  # Here we compute hedge ratios so that the portfolio
  # is insensitive to factors 1 to (N-1), thus only
  # exposed to factor N.
  #
  # By convention, we set the ratio of the first
  # instrument to 1 and then solve for ratios for
  # instruments 2 to N.
  #
  # For instrument i, the sensitivity to component j
  # is rotation[i,j]*scale[i]. We have to find hedge
  # ratios so that the total sensitivity to component
  # 1 is 0, the sensitivity to component 2 is 0 etc.
  # all the way to component N-1.
  #
  # If you do the algebra you get a system of linear
  # equations that must be solved. The following solves
  # those equations.

  lhs = t(rotation[2:N, 1:(N-1)] * scale[2:N])
  rhs = t(-rotation[1, 1:(N-1), drop=FALSE] * scale[1])

  # Note that the hedge ratio for the first instrument
  # is set to 1
  curve_structure = c(1, solve(lhs, rhs))

  stopifnot(dim(curve_structure) == c(N, 1))
  names(curve_structure) = contract_names

  attr(curve_structure, "times") = attr(curve_model, "times")
  attr(curve_structure, "contract.names") = contract_names

  return(curve_structure)
}

#' plot.bands
#' @export plot.bands
plot.bands = function(time, x, start_idx=1, end_idx=length(time)) {

  time = time[!is.na(x)]
  x = x[!is.na(x)]

  stopifnot(length(time) == length(x))

  midline = mean(x)
  stdev = sd(x)

  start_idx = start.idx(start_idx, length(time))
  end_idx = end.idx(end_idx, length(time))

  plot(time[start_idx:end_idx], x[start_idx:end_idx], type='o', ylab = "X", xlab = "Time")

  abline(midline, 0, col="black", lty=2)
  abline(midline+stdev, 0, col="blue", lty=2)
  abline(midline-stdev, 0, col="blue", lty=2)
  abline(midline+2*stdev, 0, col="red", lty=2)
  abline(midline-2*stdev, 0, col="red", lty=2)

  half_life = half.life(x)
  title(sprintf("stdev: %f, Half-life: %f", stdev, half_life))
}

#' curve.model.plot
#' @export
curve.model.plot = function(curve_model, component=-1, start_idx=1, end_idx=nrows(curve_model)) {
  nrows = curve.model.nrow(curve_model)

  start_idx = start.idx(start_idx, nrows)
  end_idx = end.idx(end_idx, nrows)
  stopifnot(start_idx <= end_idx)

  component_score = curve.model.component.score(curve_model, component)

  times = attr(curve_model, "times")

  plot.bands(times, component_score, start_idx, end_idx)
}

#' curve.data.subset
#' @export
curve.data.subset = function(curve_data, start_time=NA, end_time=NA, contract_names=NULL, lookback=NA) {

  if (is.null(contract_names)) {
    contract_names = contract.names(curve_data)
  }

  if (!is.na(lookback)) {
    lookback_start = tail(curve_data$time, 1) - lookback
    curve_data = subset(curve_data, time >= lookback_start)
  }

  if (!is.na(start_time)) {
    curve_data = subset(curve_data, time >= start_time)
  }

  if (!is.na(end_time)) {
    curve_data = subset(curve_data, time <= end_time)
  }

  if (!is.null(contract_names)) {
    curve_data = curve_data[,c("time", "numeric.time", contract_names)]
  }
  attr(curve_data, "contract.names") = contract_names
  stopifnot(!is.null(contract.names(curve_data)))

  return(curve_data)
}

#' curve.data.as.matrix
#' @export
curve.data.as.matrix = function(curve_data) {
  contract_names = contract.names(curve_data)
  stopifnot(!is.null(contract_names))
  return(as.matrix(curve_data[,contract_names]))
}

#' curve.structure.first.time
#' @export
curve.structure.first.time = function(curve_structure) {
  times = attr(curve_structure, "times")
  stopifnot(!is.null(times))
  return(times[1])
}

#' curve.structure.last.time
#' @export
curve.structure.last.time = function(curve_structure) {
  times = attr(curve_structure, "times")
  stopifnot(!is.null(times))
  return(tail(times, 1))
}

#' curve.structure.contract.names
#' @export
contract.names = function(object) {
  result = attr(object, "contract.names")
  stopifnot(!is.null(result))
  return(result)
}

#' curve.structure.value
#' @export
curve.structure.value = function(curve_structure, curve_data) {
  data_subset = curve.data.subset(
    curve_data,
    start_time = curve.structure.first.time(curve_structure),
    contract_names = contract.names(curve_structure)
  )
  value = curve.data.as.matrix(data_subset) %*% as.matrix(curve_structure)
  return(data.frame(time = data_subset$time, value = value))
}

#' curve.structure.plot
#' @export
curve.structure.plot = function(curve_structure, curve_data, start_idx=NA, end_idx=NA) {
  value = curve.structure.value(curve_structure, curve_data)
  plot.bands(value$time, value$value, start_idx, end_idx)
}

#' fair.value
#' @export
fair.value = function(curve_model, curve_data, hedged_factors) {

  curve_matrix = curve.data.as.matrix(curve_data)
  scaled_input = scale(curve_matrix, curve_model$center, curve_model$scale)
  rotated = scaled_input %*% curve_model$rotation
  scaled_output = rotated[,1:hedged_factors] %*% t(curve_model$rotation[,1:hedged_factors])

  fair_value = unscale(scaled_output, curve_model$center, curve_model$scale)
  fair_value = as.data.frame(fair_value)
  fair_value = cbind(curve_data$time, fair_value)

  # delta = curve_data-fair_value

  colnames(fair_value) = c('time', contract.names(curve_model))

  attr(fair_value, "contract.names") = contract.names(curve_model)
  attr(fair_value, "scale.mean") = curve_model$center
  attr(fair_value, "scale.stdev") = curve_model$scale
  attr(fair_value, "z.stdev") = attr(scaled_input, "scaled:scale")
  attr(fair_value, "component.loadings") = curve_model$rotation

  return(fair_value)
}

#' fair.value.z
#' @export
fair.value.z = function(curve_data, fair_value) {
  stopifnot(colnames(curve_data) == colnames(fair_value))
  stopifnot(all(curve_data$time == fair_value$time))
  delta = curve_data[,contract.names(curve_data)] - fair_value[,contract.names(curve_data)]
  scaled_delta = scale(delta)
  z = as.data.frame(scaled_delta)
  z$time = curve_data$time
  attr(z, "contract.names") = contract.names(curve_data)
  attr(z, "scaled:center") = attr(scaled_delta, "scaled:center")
  attr(z, "scaled:scale") = attr(scaled_delta, "scaled:scale")
  return(z)
}

#' plot.z
#' @export plot.z
plot.z = function(values) {
  m = mean(values)
  s = sd(values)
  plot((values-m)/s, type='o', ylab = "Z")
  abline(m, 0, col="black", lty=2)
  abline(1, 0, col="blue", lty=2)
  abline(-1, 0, col="blue", lty=2)
  abline(2, 0, col="red", lty=2)
  abline(-2, 0, col="red", lty=2)
}

#' Return the indexes of the given column names in a data frame
#'
#' @export
#'
colnames.index = function(data_frame, colnames) {
  result = rep(0, length(colnames))
  for (i in 1:length(result)) {
    index = which(colnames(data_frame) == colnames[i])
    stopifnot(length(index) > 0)
    result[i] = index
  }
  return(result)
}

#' Load data from a file downloaded from Reuters Tick History v2
#' @export
read.sfe.rates.settlement.data = function(file_name) {
  long_data = read.csv(file_name, colClasses = c("character", NA, NA))
  long_data = long_data[,c("RIC", "Trade.Date", "Settlement.Price")]
  wide_data = tidyr::spread(long_data, key = "RIC", value = "Settlement.Price")

  # Map the column names in the file to internal names
  current_colnames = c("Trade.Date", "YBAc1", "YBAc2", "YBAc3", "YBAc4", "YBAc5", "YBAc6", "YBAc7", "YBAc8", "YTTc1", "YTCc1")
  contract_names = c("IR01",  "IR02",  "IR03",  "IR04",  "IR05",  "IR06",  "IR07",  "IR08",  "YT01",  "XT01")
  target_colnames = c("time", contract_names)

  # This will fail if current_colnames are not present in wide_data
  colname_index = colnames.index(wide_data, current_colnames)
  result = wide_data[colname_index]
  colnames(result) = target_colnames

  result$time = as.POSIXct(result$time, format="%Y-%m-%d")
  result$numeric.time = as.integer(as.Date(result$time))

  attr(result, "contract.names") = contract_names

  # Rename row to time
  rownames(result) = result$time

  # Remove incomplete cases
  return(result[complete.cases(result),])
}


#' Load data from a file downloaded from Reuters Tick History v2
#' @export
read.tmx.rates.settlement.data = function(file_name) {
  long_data = read.csv(file_name, colClasses = c("character", NA, NA))
  long_data = long_data[,c("RIC", "Trade.Date", "Settlement.Price")]
  wide_data = tidyr::spread(long_data, key = "RIC", value = "Settlement.Price")

  # Map the column names in the file to internal names
  current_colnames = c("Trade.Date", "BAXcm3", "BAXcm6", "CGBc1")
  contract_names = c("BAX03",  "BAX06",  "CGB01")
  target_colnames = c("time", contract_names)

  # This will fail if current_colnames are not present in wide_data
  colname_index = colnames.index(wide_data, current_colnames)
  result = wide_data[colname_index]
  colnames(result) = target_colnames

  result$time = as.POSIXct(result$time, format="%Y-%m-%d")
  result$numeric.time = as.integer(as.Date(result$time))

  attr(result, "contract.names") = contract_names

  # Rename row to time
  rownames(result) = result$time

  # Remove incomplete cases
  return(result[complete.cases(result),])
}

#' Load data from a file downloaded from Reuters Tick History v2
#' @export
read.timmy.intraday.data = function(file_name) {
  file_data = read.csv(file_name, colClasses = c(
    "character", "character", "character", "character", "integer", "character",
    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
  ))

  # Parse Date.Time. There may be a way to do this in read.csv but I don't know how
  file_data$Date.Time = as.POSIXct(file_data$Date.Time, format="%Y-%m-%dT%H:%M:%S.000000000Z", tz="UTC")

  file_data$Local.Time = file_data$Date.Time
  attr(file_data$Local.Time, "tzone") = "Australia/Sydney"

  # Older data has GMT.Offset of "+0", which is wrong. It should either be "+11" or "+10"
  long_data = file_data[file_data$GMT.Offset != 0 & file_data$Alias.Underlying.RIC != "", c("X.RIC", "Date.Time", "GMT.Offset", "Local.Time", "Close.Ask")]

  stopifnot(all(long_data$GMT.Offset != 0 & long_data$Alias.Underlying.RIC != ""))

  wide_data = tidyr::spread(long_data, key = "X.RIC", value = "Close.Ask")

  #
  # # Map the column names in the file to internal names
  current_colnames = c("Date.Time", "GMT.Offset", "Local.Time", "YBAc3", "YBAc6", "YTCc1")
  contract_names = c("IR03",  "IR06", "XT01")
  target_colnames = c("time", "tz.offset", "local.time", contract_names)

  #
  # This will fail if current_colnames are not present in wide_data
  colname_index = colnames.index(wide_data, current_colnames)
  result = wide_data[colname_index]
  colnames(result) = target_colnames


  stopifnot(all(!is.na(result$time) & !is.na(result$local.time)))
  stopifnot(all(result$time == unique(result$time)))

  attr(result, "contract.names") = target_colnames

  # Remove incomplete cases
  return(na.omit(result))
}


#' write.fair.value.config
#' @export
write.fair.value.config = function(curve_data, curve_name, time_period, hedged_factors, file) {

  curve_model = build.curve.model(curve_data)
  curve_structure = build.curve.structure(curve_model)
  hedge_ratios = unname(curve_structure)

  fair_value = fair.value(curve_model, curve_data, hedged_factors)
  z = fair.value.z(curve_data, fair_value)

  component_loadings=unname(as.matrix(attr(fair_value, "component.loadings")))

  z_mean = attr(z, "scaled:center")
  z_stdev = attr(z, "scaled:scale")

  stopifnot(!is.null(z_mean))
  stopifnot(!is.null(z_stdev))

  mamba_contract_names = paste('$', contract.names(curve_model), sep = "")

  output_list = list(
    curve_name=curve_name,
    time_period=time_period,
    hedged_factors=hedged_factors,
    contract_names=mamba_contract_names,
    hedge_ratios=hedge_ratios,
    scale_mean=unname(attr(fair_value, "scale.mean")),
    scale_stdev=unname(attr(fair_value, "scale.stdev")),
    z_mean=unname(z_mean),
    z_stdev=unname(z_stdev),
    component_loadings=component_loadings
  )
  write(jsonlite::toJSON(output_list), file=file)
}

#' Australian Government Bond future value
#' @export
gov.bond.value = function(price, face_value, coupon_rate, payments_per_year, maturity_value) {
  yld = 100 - price
  a = (1.0/(1.0 + (yld/(payments_per_year*100.0))))
  b = a^(maturity_value*payments_per_year)
  c = coupon_rate/100.0 * (1-b)/(yld/100)
  result = face_value * (b + c/100.0)
  return(round(result,2))
}

#' 10 year Australian Government Bond future value
#' @export
xt.value = function(price) {
  return(gov.bond.value(price, 100000, 600, 2, 10))
}

#' 3 year Australian Government Bond future value
#' @export
yt.value = function(price) {
  return(gov.bond.value(price, 100000, 600, 2, 3))
}

#' 90 day bank bill future value
#' @export
bank.bill.value = function(price) {
  return(1000000*365/(365+(100-price)*90/100))
}

#' DV01 for a given valuation function
#' @export
dv.01 = function(valuation_function, price) {
  return(round(valuation_function(price) - valuation_function(price-0.01), 2))
}

#' Analyse trades
#' @export
analyse.trades = function(trades, point_value=1, do_plot=FALSE) {

  # Factory function to make sure we don't miss anything by creating
  # a list directly
  create.summary = function(
    total_pnl,
    pnl_per_day,
    pnl_mean,
    pnl_sd,
    pnl_quantiles,
    mae_quantiles,
    filtered_total_pnl,
    filtered_pnl_mean,
    filtered_pnl_sd,
    trade_count,
    win_count,
    loss_count,
    win_ratio,
    holding_period_mean,
    holding_period_sd,
    holding_period_median,
    holding_period_max
  ) {
    return(list(
      total.pnl = total_pnl,
      pnl.per.day = pnl_per_day,
      pnl.mean = pnl_mean,
      pnl.sd = pnl_sd,
      pnl.quantiles = pnl_quantiles,
      mae.quantiles = mae_quantiles,
      filtered.total.pnl = filtered_total_pnl,
      filtered.pnl.mean = filtered_pnl_mean,
      filtered.pnl.sd = filtered_pnl_sd,
      trade.count = trade_count,
      win.count = win_count,
      loss.count = loss_count,
      win.ratio = win_ratio,
      holding.period.mean=holding_period_mean,
      holding.period.sd=holding_period_sd,
      holding.period.median=holding_period_median,
      holding.period.max=holding_period_max
    ))
  }

  if (nrow(trades) == 0) {
    if (do_plot) {
      print("No trades to plot")
    }

    return(create.summary(
      total_pnl = 0,
      pnl_per_day = 0,
      pnl_mean = NA,
      pnl_sd = NA,
      pnl_quantiles = NA,
      mae_quantiles = NA,
      filtered_total_pnl = 0,
      filtered_pnl_mean = NA,
      filtered_pnl_sd = NA,
      trade_count = 0,
      win_count = 0,
      loss_count = 0,
      win_ratio = NA,
      holding_period_mean = NA,
      holding_period_sd = NA,
      holding_period_median = NA,
      holding_period_max = NA
    ))
  } else {

    dollar_pnl = trades$pnl * point_value
    dollar_filtered_pnl = trades$filtered_pnl * point_value
    dollar_mae = trades$mae * point_value

    if (do_plot) {
      layout(1:3)
      plot.trades.histogram(dollar_pnl, dollar_filtered_pnl)
      plot.trades.equity.curve(trades$start_time, trades$end_time, dollar_pnl, dollar_filtered_pnl)
    }

    win_count = sum(dollar_pnl > 0)
    loss_count = sum(dollar_pnl <= 0)
    win_ratio = win_count/(win_count+loss_count)
    total_pnl = sum(dollar_pnl)

    pnl_per_day = total_pnl/sum(trades$holding_period)
    pnl_mean = mean(dollar_pnl)
    pnl_sd = sd(dollar_pnl)

    pnl_quantiles = quantile(dollar_pnl, c(0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 1))
    mae_quantiles = quantile(dollar_mae, c(0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 1))

    filtered_total_pnl = sum(dollar_filtered_pnl)
    filtered_pnl_mean = mean(dollar_filtered_pnl)
    filtered_pnl_sd = sd(dollar_filtered_pnl)

    trade_count = nrow(trades)
    win_count = win_count
    loss_count = loss_count
    win_ratio = win_ratio

    holding_period_mean = mean(trades$holding_period)
    holding_period_sd = sd(trades$holding_period)
    holding_period_median = median(trades$holding_period)
    holding_period_max = max(trades$holding_period)

    return(create.summary(
      total_pnl = total_pnl,
      pnl_per_day = pnl_per_day,
      pnl_mean = pnl_mean,
      pnl_sd = pnl_sd,
      pnl_quantiles = pnl_quantiles,
      mae_quantiles = mae_quantiles,
      filtered_total_pnl = filtered_total_pnl,
      filtered_pnl_mean = filtered_pnl_mean,
      filtered_pnl_sd = filtered_pnl_sd,
      trade_count = trade_count,
      win_count = win_count,
      loss_count = loss_count,
      win_ratio = win_ratio,
      holding_period_mean = holding_period_mean,
      holding_period_sd = holding_period_sd,
      holding_period_median = holding_period_median,
      holding_period_max = holding_period_max
    ))
  }
}

#' @export
execute.rudolf = function(
  futures_prices,
  x_leg_expr,
  y_leg_expr,
  start_time,
  end_time,
  max_boundary,
  max_clip_count,
  intercept_var,
  slope_var,
  sd_lookback
) {
  stopifnot(end_time <= tail(futures_prices$time, 1))

  trading_data = build.rudolf.trading.data(futures_prices, x_leg_expr, y_leg_expr, intercept_var, slope_var, sd_lookback)
  trading_data = subset(trading_data, start_time <= time & time <= end_time)
  stopifnot(nrow(trading_data) > 0)

  thresholds = stdev.thresholds(trading_data$residual.stdev, max_clip_count = max_clip_count)
  execution = execute.pairs.strategy(pricing_data = trading_data, thresholds = thresholds)

  return(list(trading_data=trading_data, execution=execution))
}

#' @export
get.rudolf.trades = function(
  futures_prices,
  x_leg_expr,
  y_leg_expr,
  start_time,
  end_time,
  max_boundary,
  max_clip_count,
  intercept_var,
  slope_var,
  sd_lookback,
  stop_loss=NULL
) {
  execution_result = execute.rudolf(
    futures_prices = futures_prices,
    x_leg_expr = x_leg_expr,
    y_leg_expr = y_leg_expr,
    start_time = start_time,
    end_time = end_time,
    max_boundary = max_boundary,
    max_clip_count = max_clip_count,
    intercept_var = intercept_var,
    slope_var = slope_var,
    sd_lookback = sd_lookback
  )
  trades = extract.trades(execution_result$execution)
  if (!is.null(stop_loss) && !is.na(stop_loss)) {
    trades = apply.stop.filter(trades, max.loss.filter(execution_result$execution, stop_loss))
  }
  result = list(trading_data=execution_result$trading_data, execution=execution_result$execution, trades=trades)
  return(result)
}

#' @export
analyse.rudolf.trades = function(
  futures_prices,
  x_leg_expr,
  y_leg_expr,
  start_time,
  end_time,
  max_boundary,
  max_clip_count,
  intercept_var,
  slope_var,
  sd_lookback,
  stop_loss=NULL,
  do_plot=FALSE,
  point_value=1
) {
  run_results = get.rudolf.trades(
    futures_prices,
    x_leg_expr,
    y_leg_expr,
    start_time,
    end_time,
    max_boundary,
    max_clip_count,
    intercept_var,
    slope_var,
    sd_lookback,
    stop_loss
  )
  trades_analysis = analyse.trades(trades = run_results$trades, point_value = point_value, do_plot = do_plot)
  result = append(list(run_results=run_results), trades_analysis)
  return(result)
}

#' @export
rudolf.optimisation = function(
  futures_prices,
  x_leg_expr,
  y_leg_expr,
  start_time,
  end_time,
  max_clip_count,
  max_boundary_range,
  intercept_var_range,
  slope_var_range,
  sd_lookback_range,
  pop_size,
  max_iter,
  run,
  parallel,
  significant_digits,
  data_lookback
) {

  if (!is.null(data_lookback) && !is.na(data_lookback)) {
    # Subset end_time is 7 days after specified end_time to avoid intra-day rounding issues and weekends etc
    futures_prices = curve.data.subset(futures_prices, start_time = start_time - data_lookback, end_time = end_time + lubridate::days(7))
  }

  stopifnot(!is.na(start_time) && !is.na(end_time))

  tictoc::tic(sprintf("rudolf.optimsation: start_time: %s", start_time))

  objective_function = function(individual) {

    max_boundary = individual[1]
    intercept_var = individual[2]
    slope_var = individual[3]
    sd_lookback = as.integer(round(individual[4]))

    # stop_loss = individual[5]
    # print(sprintf("individual: %s", individual))
    # print(sprintf("sd_lookback: %s", sd_lookback))
    # print(sprintf("max_clip_count: %s", max_clip_count))

    stopifnot(max_boundary_range[1] <= max_boundary && max_boundary <= max_boundary_range[2])
    stopifnot(intercept_var_range[1] <= intercept_var && intercept_var <= intercept_var_range[2])
    stopifnot(slope_var_range[1] <= slope_var && slope_var <= slope_var_range[2])
    stopifnot(sd_lookback_range[1] <= sd_lookback && sd_lookback <= sd_lookback_range[2])

    # tictoc::tic(sprintf("rudolf.optimsation: objective_function"))

    rudolf_execution = execute.rudolf(
      futures_prices = futures_prices,
      x_leg_expr = x_leg_expr,
      y_leg_expr = y_leg_expr,
      start_time = start_time,
      end_time = end_time,
      max_boundary = max_boundary,
      max_clip_count = max_clip_count,
      intercept_var = intercept_var,
      slope_var = slope_var,
      sd_lookback = sd_lookback
    )
    pnl = tail(rudolf_execution$execution$pnl, 1)
    # print(sprintf("pnl: %f", pnl))
    stopifnot(!is.na(pnl))

    # tictoc::toc()

    # Only use 3 significant digits so that the GA doesn't keep going due to
    # small insignificant improvements
    return(signif(pnl, significant_digits))
  }

  optimisation.monitor = function(state) {
    print(sprintf("P&L: %f", state@fitness))
    print(state@population)
  }

  range_minima = c(max_boundary_range[1], intercept_var_range[1], slope_var_range[1], sd_lookback_range[1])
  range_maxima = c(max_boundary_range[2], intercept_var_range[2], slope_var_range[2], sd_lookback_range[2])

  ga_result = GA::ga(
    type="real-valued",
    fitness = objective_function,
    min = range_minima,
    max = range_maxima,
    popSize = pop_size,
    maxiter = max_iter,
    run = run,
    parallel = parallel,
    # monitor = optimisation.monitor
    monitor = GA::gaMonitor2
  )

  # print("Solution:")
  # print(ga_result@solution)

  # There may be many solutions
  stopifnot(length(dim(ga_result@solution)) == 2)

  # We arbitrarily pick the first one
  max_boundary = ga_result@solution[[1,1]]
  intercept_var = ga_result@solution[[1,2]]
  slope_var = ga_result@solution[[1,3]]
  sd_lookback = ga_result@solution[[1,4]]

  stopifnot(max_boundary_range[1] <= max_boundary && max_boundary <= max_boundary_range[2])
  stopifnot(intercept_var_range[1] <= intercept_var && intercept_var <= intercept_var_range[2])
  stopifnot(slope_var_range[1] <= slope_var && slope_var <= slope_var_range[2])
  stopifnot(sd_lookback_range[1] <= sd_lookback && sd_lookback <= sd_lookback_range[2])

  optimal_parameters = list(
    x_leg_expr = x_leg_expr,
    y_leg_expr = y_leg_expr,
    max_boundary = max_boundary,
    intercept_var = intercept_var,
    slope_var = slope_var,
    sd_lookback = sd_lookback,
    stop_loss = NULL,
    start_time = start_time,
    end_time = end_time,
    max_clip_count = max_clip_count
  )

  attr(optimal_parameters, "ga.result") = ga_result

  tictoc::toc()

  return(optimal_parameters)
}

#' @export
timmy.optimisation = function(
  futures_prices,
  start_time,
  end_time,
  max_clip_count,
  pop_size=100,
  max_iter=200,
  run=20,
  parallel=TRUE,
  significant_digits=3,
  data_lookback=lubridate::years(5)
) {

  return(rudolf.optimisation(
    futures_prices,
    x_leg_expr = list(IR03=1, IR06=-1),
    y_leg_expr = list(XT01=1),
    start_time = start_time,
    end_time = end_time,
    max_clip_count = max_clip_count,
    max_boundary_range = c(1, 3),
    intercept_var_range = c(0, 1),
    slope_var_range = c(0, 1),
    sd_lookback_range = c(50, 200),
    pop_size = pop_size,
    max_iter = max_iter,
    run = run,
    parallel = parallel,
    significant_digits = significant_digits,
    data_lookback = data_lookback
  ))
}

#' @export
test.times = function(futures_prices, optimisation_result, following_period) {
  if (is.null(following_period) || is.na(following_period)) {  # in-sample
    start_time = optimisation_result$start_time
    end_time = optimisation_result$end_time
  } else { # out-of-sample
    start_time = optimisation_result$end_time + as.difftime(1, units = "days")
    end_time = start_time + following_period
    stopifnot(end_time > start_time)
  }
  stopifnot(start_time >= futures_prices$time[1], end_time <= tail(futures_prices$time, 1))
  return(list(start_time=start_time, end_time=end_time))
}

#' If following_period is NULL, the test is in-sample. Otherwise, it is an out-of-sample test. The
#' test period starts from the end of the training period and continues for the given following period.
#' @export
test.rudolf = function(futures_prices, optimisation_result, following_period=NULL, do_plot=FALSE, point_value=1) {

  time_bounds = test.times(futures_prices, optimisation_result, following_period)

  return(analyse.rudolf.trades(
    futures_prices,
    point_value,
    x_leg_expr = optimisation_result$x_leg_expr,
    y_leg_expr = optimisation_result$y_leg_expr,
    start_time = time_bounds$start_time,
    end_time = time_bounds$end_time,
    max_boundary = optimisation_result$max_boundary,
    max_clip_count = optimisation_result$max_clip_count,
    intercept_var = optimisation_result$intercept_var,
    slope_var = optimisation_result$slope_var,
    sd_lookback = optimisation_result$sd_lookback,
    stop_loss = optimisation_result$stop_loss,
    do_plot = do_plot
  ))
}

#' @export
write.rudolf.optimisation.result = function(optimsation_result, file) {

  attr(optimsation_result, "ga.result") = NULL
  text = c(jsonlite::serializeJSON(optimsation_result, pretty = TRUE, digits=5), "\n")
  write(text, file=file)
}

#' @export
read.rudolf.optimisation.result = function(file) {
  # result = as.list(jsonlite::fromJSON(file))
  # result$start_time = as.POSIXct(result$start_time, tz="Australia/Sydney")
  # result$end_time = as.POSIXct(result$end_time, tz="Australia/Sydney")
  # return(result)

  return(jsonlite::unserializeJSON(readr::read_file(file)))
}

#' @export
build.rudolf.trading.parameters = function(futures_prices, optimisation_result) {
  trading_data = build.rudolf.trading.data(
    futures_data = futures_prices,
    x_leg_expr = optimisation_result$x_leg_expr,
    y_leg_expr =optimisation_result$y_leg_expr,
    intercept_var = optimisation_result$intercept_var,
    slope_var = optimisation_result$slope_var,
    sd_lookback = optimisation_result$sd_lookback
  )
  last_row = tail(trading_data, 1)

  max_boundary = optimisation_result$max_boundary
  entry_threshold = (optimisation_result$max_boundary * last_row$residual.stdev)/optimisation_result$max_clip_count

  result = list(
    model.date = last_row$time,
    x_leg_expr = optimisation_result$x_leg_expr,
    y_leg_expr = optimisation_result$y_leg_expr,
    max.clip.count=optimisation_result$max_clip_count,
    intercept=last_row$intercept,
    slope=last_row$slope,
    residual.stdev=last_row$residual.stdev,
    max.boundary=max_boundary,
    entry.threshold=entry_threshold
  )
  return(result)
}

#' @export update.timmy.parameters
update.timmy.parameters = function(path_to_data_dir=".") {
  futures_prices = read.sfe.rates.settlement.data(file.path(path_to_data_dir, "data/sfe_rates.csv"))
  model_parameters = read.rudolf.optimisation.result(file.path(path_to_data_dir, "data/timmy_optimised_parameters.json"))
  model = build.rudolf.trading.parameters(futures_prices, model_parameters)
  text = c(jsonlite::toJSON(model, auto_unbox = TRUE, pretty = TRUE, digits=5), "\n")
  write(text, file.path(path_to_data_dir, "data/timmy_model.json"))

  format_number = function(number) {
    return(sprintf("%.5f", number))
  }

  model_row = data.frame(
    Date=model$model.date,
    Intercept=format_number(model$intercept),
    Slope=format_number(model$slope),
    Entry.Threshold=format_number(model$entry.threshold)
  )
  write.csv(model_row, row.names=FALSE)
}

# start_date
# end_date - exclusive - won't be included
# interval
#' @export
date.sequence = function(start_date, end_date, interval) {
  stopifnot(start_date < end_date)

  N = 0
  result = vector("list", 0)

  next_date = start_date
  while (next_date < end_date) {
    N = N+1
    length(result) = N
    result[[N]] = next_date
    next_date = next_date + interval
  }

  return(result)
}
