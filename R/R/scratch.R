library(tictoc)

timmy.rolling.optimisation = function(
  data,
  first_start_date,
  last_start_date, # Exclusive
  start_date_step,
  training_period,
  max_clip_count,
  pop_size=100,
  max_iter=200,
  run=20,
  parallel=TRUE
) {
  tic("rolling.optimisation")
  start_dates = date.sequence(first_start_date, last_start_date, start_date_step)
  end_dates = lapply(start_dates, "+", training_period)

  results = vector("list", length(start_dates))
  for (i in 1:length(start_dates)) {
    start_time = as.POSIXct(start_dates[[i]])
    end_time = as.POSIXct(end_dates[[i]])
    print(sprintf("start_time: %s, end_time: %s", start_time, end_time))
    results[[i]] = timmy.optimisation(
      data,
      start_time=start_time,
      end_time=end_time,
      max_clip_count=max_clip_count,
      pop_size=pop_size,
      max_iter=max_iter,
      run=run,
      parallel=parallel
    )
  }
  parameters = list(
    first.start.time=first_start_time,
    last.start.time=last_start_time,
    start.time.step=start_time_step,
    training.period=training_period,
    max.clip.count=max_clip_count,
    pop.size=pop_size,
    max.iter=max_iter,
    run=run,
    parallel=parallel
  )
  attr(results, "rolling.optimisation.parameters") = parameters
  toc()
  return(results)
}

plot.timmy.decision.state = function(futures_data, timmy_parameters, start_idx=NA, end_idx=NA, start_time=NULL, end_time=NULL) {

  trading_data = build.timmy.trading.data(
    futures_data,
    intercept_var = timmy_parameters$intercept_var,
    slope_var = timmy_parameters$slope_var,
    sd_lookback = timmy_parameters$sd_lookback
  )

  # trading_data will be shorter than futures_data by one because the Kalman filter model
  # must be lagged to the next day
  start_idx = start.idx(start_idx, nrow(trading_data))
  end_idx = end.idx(end_idx, nrow(trading_data))

  if (start_idx != 1 || end_idx != nrow(trading_data)) {
    trading_data = trading_data[start_idx:end_idx,]
  }
  if (!is.null(start_time)) {
    trading_data = subset(trading_data, time >= start_time)
  }
  if (!is.null(end_time)) {
    trading_data = subset(trading_data, time <= end_time)
  }
  layout(1:2)
  # Plot bills spread
  plot(trading_data$time, trading_data$x, type = 'o', col="black")
  grid()

  first_time = trading_data$time[1]
  last_time = tail(trading_data$time, 1)
  title_text = sprintf("%s - %s", first_time, last_time)
  title(title_text)

  # Plot tens
  ylim=c(min(trading_data$y, trading_data$y_hat), max(trading_data$y, trading_data$y_hat))
  plot(trading_data$time, trading_data$y, type='o', ylim=ylim, col="red")
  lines(trading_data$time, trading_data$y_hat, type='o', col="black")
  grid()
}

final.model.drift = function(run_results) {

  trading_data = run_results$trading_data
  trades = run_results$trades

  start_slope = trading_data$slope[trades$start_idx]
  end_slope = trading_data$slope[trades$end_idx]
  start_intercept = trading_data$intercept[trades$start_idx]
  end_intercept = trading_data$intercept[trades$end_idx]
  model_drift = sqrt(((end_slope-start_slope)/start_slope)^2 + ((end_intercept - start_intercept)/start_intercept)^2)
  return(model_drift)
}

intra.trade.model.drift = function(run_results, trade_idx) {

  start_idx = run_results$trades$start_idx[trade_idx]
  end_idx = run_results$trades$end_idx[trade_idx]

  trading_data = run_results$trading_data
  trades = run_results$trades

  intercept = trading_data$intercept[start_idx:end_idx]
  slope = trading_data$slope[start_idx:end_idx]

  intercept_delta = (intercept - intercept[1])/intercept[1]
  slope_delta = (slope - slope[1])/slope[1]

  model_drift = sqrt((intercept_delta)^2 + (slope_delta)^2)

  return(model_drift)
}

intra.trade.pnl = function(run_results, trade_idx) {

  start_idx = run_results$trades$start_idx[trade_idx]
  end_idx = run_results$trades$end_idx[trade_idx]

  execution = run_results$execution
  trades = run_results$trades

  total_pnl = execution$pnl[start_idx:end_idx]

  return(total_pnl-total_pnl[1])
}

#' @export plot.trades.mae
plot.pnl.versus.feature = function(pnl, feature) {

  #----------------------------------------------------------------------------
  # Scatter plot of winners and losers versus holding period
  winners = pnl >= 0
  losers = pnl < 0

  # Set up drawing area without drawing anything
  plot(feature, pnl, type="n")
  # Draw winners
  points(feature[winners], pnl[winners], type="p", col="blue")
  # Draw losers
  points(feature[losers], pnl[losers], type="p", col="red")
}

analyse.pnl = function(pnl) {

  win_count = sum(pnl > 0)
  loss_count = sum(pnl <= 0)
  win_ratio = win_count/(win_count+loss_count)

  return(list(
    pnl.mean=mean(pnl),
    pnl.sd=sd(pnl),
    pnl.quantiles=quantile(pnl, c(0, 0.01, 0.05, 0.5, 0.95, 0.99, 1)),
    trade.count=length(pnl),
    win.count=win_count,
    loss.count=loss_count,
    win.ratio=win_ratio
  ))
}

#' @export
ike.optimisation = function(futures_prices, start_time, end_time, max_clip_count, pop_size=100, max_iter=200, run=20, parallel=TRUE) {

  return(rudolf.optimisation(
    futures_prices,
    x_leg_expr = list(BAX03=1, BAX06=-1),
    y_leg_expr = list(CGB01=1),
    start_time = start_time,
    end_time = end_time,
    max_clip_count = max_clip_count,
    max_boundary_range = c(1, 3),
    intercept_var_range = c(0, 50),
    slope_var_range = c(0, 50),
    sd_lookback_range = c(50, 200),
    pop_size = pop_size,
    max_iter = max_iter,
    run = run,
    parallel = parallel
  ))
}

#' @export
kyle.optimisation = function(futures_prices, start_time, end_time, max_clip_count, pop_size=100, max_iter=200, run=20, parallel=TRUE) {

  return(rudolf.optimisation(
    futures_prices,
    x_leg_expr = list(ZN01=1),
    y_leg_expr = list(XT01=1),
    start_time = start_time,
    end_time = end_time,
    max_clip_count = max_clip_count,
    max_boundary_range = c(1, 3),
    intercept_var_range = c(0, 0.001),
    slope_var_range = c(0, 0.0001),
    sd_lookback_range = c(10, 300),
    pop_size = pop_size,
    max_iter = max_iter,
    run = run,
    parallel = parallel
  ))
}

#' @export
plot.rudolf.analysis = function(analysis) {
  layout(1:3)
  plot.trades.histogram(analysis$run_result$trades)
  plot.trades.equity.curve(analysis$run_result$trades)
}

#' Load data from a file downloaded from Reuters Tick History v2
#' @export
read.curve.curve.settlement.data = function(file_name) {
  long_data = read.csv(file_name, colClasses = c("character", NA, NA))
  long_data = long_data[,c("RIC", "Trade.Date", "Settlement.Price")]
  wide_data = tidyr::spread(long_data, key = "RIC", value = "Settlement.Price")

  # Map the column names in the file to internal names
  current_colnames = c("Trade.Date", "YTCc1", "YTTc1", "FVcm1", "TYcm1")
  contract_names = c("XT01", "YT01", "ZF01", "ZN01")
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

execute.multiple.rudolf.models = function(futures_prices, models, following_period=NULL) {
  executions = vector("list", length(models))

  for (i in 1:length(models)) {
    model = models[[i]]
    time_bounds = test.times(futures_prices, model, following_period)
    execution = execute.rudolf(
      futures_prices,
      x_leg_expr = model$x_leg_expr,
      y_leg_expr = model$y_leg_expr,
      start_time = time_bounds$start_time,
      end_time = time_bounds$end_time,
      max_boundary = model$max_boundary,
      max_clip_count = model$max_clip_count,
      intercept_var = model$intercept_var,
      slope_var = model$slope_var,
      sd_lookback = model$sd_lookback
    )
    executions[[i]] = execution$execution
  }

  combined_execution = do.call("rbind", executions)
  combined_execution$pnl = cumsum(combined_execution$pnl_delta)

  return(combined_execution)
}

analyse.execution.trades = function(execution, stop_loss=NULL, do_plot=FALSE, point_value=1) {
  trades = extract.trades(execution)
  if (!is.null(stop_loss) && !is.na(stop_loss)) {
    trades = apply.stop.filter(trades, max.loss.filter(execution, stop_loss))
  }
  trades_analysis = analyse.trades(trades = trades, point_value = point_value, do_plot = do_plot)
  return(trades_analysis)
}

execution.sharpe = function(execution) {

  daily_mean = mean(execution$pnl_delta)
  daily_sd = sd(execution$pnl_delta)

  # Business days
  annualised_mean = daily_mean * 256
  annualised_sd = daily_sd * sqrt(256)

  annualised_sharpe = annualised_mean / annualised_sd

  return(annualised_sharpe)
}

analyse.execution.pnl = function(execution, point_value, do_plot=FALSE) {
  sharpe = execution.sharpe(execution)

  dollar_pnl = execution$pnl * point_value
  dollar_pnl_delta = execution$pnl_delta * point_value

  pnl_quantiles = quantile(dollar_pnl_delta, c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
  pnl_mean = mean(dollar_pnl_delta)
  pnl_drawdowns = drawdowns(dollar_pnl)

  drawdown_quantiles = quantile(pnl_drawdowns, c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))

  if (do_plot) {
    layout(1:3)
    hist(dollar_pnl_delta)
    abline(v=pnl_mean, col="red")
    plot(execution$time, dollar_pnl, type='l')
    plot(execution$time, pnl_drawdowns, type='l')
    abline(0, 0, col="red")
  }

  return(list(
    sharpe = sharpe,
    pnl_quantiles = pnl_quantiles,
    pnl_mean = pnl_mean,
    drawdown_quantiles = drawdown_quantiles
  ))
}
