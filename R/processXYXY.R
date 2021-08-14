#'
#' Extract the values in JCAMP-DX file with an PEAK_TABLE variable list.
#'
#' This function is NOT EXPORTED.
#' Users would not normally call this function.  See \code{\link{readJDX}}.
#' Documentation is provided for developers wishing to contribute to the package.
#'
#' @param VL Character.  The variable list to be processed as a character vector. Includes one pre-pended
#' line giving the format of data (e.g. XYY, XRR, XII, PEAK_TABLE).
#'
#' @param params Numeric. Vector of parameters extracted from file header.
#'
#' @param mode Character. One of c("XY_data", "NMR", "NMR2D", "PEAK_TABLE")
#'
#' @param SOFC Logical.  See \code{\link{readJDX}} for details.
#'
#' @param debug Integer.  See \code{\link{readJDX}} for details.
#'
#' @return A data frame with elements \code{x} and \code{y}.
#'
#' @noRd
#'

processXYXY <- function(VL, params, mode, SOFC, debug = 0) {
  fmt <- VL[1]
  VL <- VL[-1] # Remove the pre-pended format string
  if (debug >= 1) cat("\nProcessing variable list...\n")

  # This format is typical of a single mass spectrum is also used in LC_MS for
  # each time point

  ### Step 1. Convert to numeric values
  xValues <- charXYnumXY(VL)$x
  yValues <- charXYnumXY(VL)$y

  ### Step 2. Check the integrity of the results
  # Check that we got the right number of data points

  npoints <- as.integer(params[1])
  if (debug == 2) cat("\nNPOINTS =", npoints, "\n")
  if (debug == 2) cat("Actual no. data points found  =", length(xValues), "\n")
  if (!npoints == length(xValues)) stop("NPOINTS and length of parsed data don't match")

  ### And we're done...

  xydata <- data.frame(x = xValues, y = yValues)
  return(xydata)
}
