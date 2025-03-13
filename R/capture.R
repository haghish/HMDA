

capture <- function(expr) {
  # Capture the expression as code
  expr <- substitute(expr)

  # Initialize storage for console outputs, messages, warnings, and errors
  output_text <- character(0)
  message_text <- character(0)
  warning_text <- character(0)
  error_text <- NULL

  # Evaluate the expression while capturing outputs, messages, and warnings
  result <- withCallingHandlers(
    tryCatch({
      # capture.print suppresses all printed output
      output_text <- capture.output({
        res <- eval(expr, envir = parent.frame())
      })
      res
    },
    error = function(e) {
      error_text <<- conditionMessage(e)
      NULL
    }),
    warning = function(w) {
      warning_text <<- c(warning_text, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      message_text <<- c(message_text, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  # Return a list containing the evaluated value and all captured text
  list(
    value = result,
    output = output_text,
    messages = message_text,
    warnings = warning_text,
    error = error_text
  )
}

# # Example usage:
# captured <- capture({
#   print("Hello, world!")
#   message("This is a message.")
#   warning("This is a warning.")
#   cat(42)  # final value returned
# })
#
# # To print the captured output:
# print(captured$output)
# print(captured$messages)
# print(captured$warnings)
# print(captured$error)
# print(captured$value)
