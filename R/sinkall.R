#'
#' Divert stdout and stderr to a File
#'
#' This is a simple utility function to direct the output of stdout and stderr
#' to a file.  stdout is the information normally printed in the console, for instance
#' the results of \code{print(rnorm(5))}.  stderr is the output created by functions
#' \code{message}, \code{warning} and \code{stop}.  The purpose of this function is to allow
#' one to direct all this output into a single file where the results can be studied, for
#' instance, for troubleshooting purposes.  Works exactly like the base \code{sink()} function:
#' you have to call it a second time with no arguments to close the file.
#'
#' @param filename Character.  A path to a filename where the results will be captured.
#'
#' @return \code{NULL}, invisibly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tf <- tempfile(pattern = "SinkDemo", fileext = "txt")
#' sinkall(tf)
#' print("Hello")
#' print(rnorm(2))
#' print(normr(2)) # typo, so it errors
#' message("A message from message()")
#' warning("A warning from warning()")
#' cat("Information via cat\\n")
#' sinkall() # one must close the file connection
#' file.show(tf)
#' }
#'
sinkall <- function(filename = NULL) {
  if (!is.null(filename)) {
    con <- file(filename) # opens the connection
    sink(con, type = "output") # overwriting
    sink(con, type = "message", append = TRUE)
    # close(con)
  }

  if (is.null(filename)) {
    sink(type = "output")
    sink(type = "message")
  }

  invisible()
}
