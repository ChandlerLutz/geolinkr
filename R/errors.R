#' Create and Throw a Custom Error
#'
#' This function creates a custom error object and immediately throws it using `stop()`.
#' It allows you to define your own error classes for better error handling.
#'
#' @param .subclass The subclass of the error. This should be a string that
#'   uniquely identifies your custom error type (e.g., "my_package_data_error").
#' @param message The error message to be displayed.
#' @param call The call that triggered the error. Defaults to the calling function.
#'   You can use `sys.call(-1)` to capture the call automatically. If `NULL`, no
#'   call will be included in the error object.
#' @param ... Additional named arguments to be included in the error object.
#'   These can be any data that might be helpful for debugging or handling
#'   the error.
#'
#' @return This function does not return a value. It throws an error.
#'
#' @seealso [base::stop()] for the base R error function, and
#'   <https://adv-r.hadley.nz/conditions.html> for more information on conditions and error handling
#'   in R.
#'
#' @noRd
stop_custom <- function(.subclass, message, call = NULL, ...) {
  err <- structure(
    list(
      message = message,
      call = call,
      ...
    ),
    class = c(.subclass, "error", "condition")
  )
  stop(err)
}

