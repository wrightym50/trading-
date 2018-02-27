
library(t0)

#' Plot the Kalman regression result
#' @export plot.kalman.regression
plot.kalman.regression.trading.data = function(data, plot_input_data=FALSE) {
  
  graph_count = 3
  if (plot_input_data) {
    graph_count = 5
  }
  
  layout(1:graph_count)
  
  time = data$time
  
  if (plot_input_data) {
    plot(time, data$x, type="l", ylab="X")
    plot(time, data$y, type="l", ylab="Y")
  }
  
  plot(time, data$intercept, type="l", ylab="Intercept", xlab="Time")
  abline(0,0)
  
  # title_text = sprintf(
  #   "P1: %f, P2: %f, Q1: %f, Q2: %f, R: %f",
  #   attr(data, "kalman.regression.p1"),
  #   attr(data, "kalman.regression.p2"),
  #   attr(data, "kalman.regression.q1"),
  #   attr(data, "kalman.regression.q2"),
  #   attr(data, "kalman.regression.r")
  # )
  
  # title(title_text)
  
  plot(time, data$slope, type="l", ylab="Slope", xlab="Time")
  abline(0,0)
  
  plot.bands(time, data$residual)
}