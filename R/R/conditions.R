#!/usr/bin/env Rscript
# tryCatch.Rscript -- experiments with tryCatch

# Get any arguments
a <- "error"

# Define a division function that can issue warnings and errors
myDivide <- function(d, a) {
  if (a == 'warning') {
    return_value <- 'myDivide warning result'
    warning("myDivide warning message")
  } else if (a == 'error') {
    return_value <- 'myDivide error result'
    stop("myDivide error message")
  } else {
    return_value = d / as.numeric(a)
  }
  return(return_value)
}

# try.catch.test = function() {
#   do.sleep = {
#     warning("Warning")
#     return(-1)
#   }
#   result = tryCatch(
#     do.sleep(),
#     warning = function(warning) {
#       cat("Caught Warning")
#       return(NA)
#     }
#   )
#   return(result)
# }

do.sleep = function() {
  # Sys.sleep(5)
  # warning("Warning")
  while(TRUE) {
  }
  stop("Stop")
  return(-1)
}

try.catch.test = function() {
  # Evalute the desired series of expressions inside of tryCatch
  result <- tryCatch({

    do.sleep()

  }, warning = function(war) {

    # warning handler picks up where error was generated
    print(paste("MY_WARNING:  ",war))
    return(100)

  }, error = function(err) {

    # error handler picks up where error was generated
    print(paste("MY_ERROR:  ",err))
    return(200)

  }, finally = {


  }) # END tryCatch
}

# print(paste("result =", try.catch.test()))
