#'
#' Extract the values in JCAMP-DX file with an "Data Table" variable list.
#'
#' This function is NOT EXPORTED.
#' Users would not normally call this function.  See \code{\link{readJDX}}.
#' Documentation is provided for developers wishing to contribute to the package.
#'
#' @param VL Character.  The variable list to be processed as a character vector. Includes one pre-pended
#' line giving the format of data (e.g. XYY, XRR, XII, PEAK_TABLE, etc).
#'
#' @param params Numeric. Vector of parameters extracted from file header.
#'
#' @param mode Character. One of c("XY_data", "NMR", "NMR2D", "PEAK_TABLE", "LC_MS")
#'
#' @param SOFC Logical.  See \code{\link{readJDX}} for details.
#'
#' @param debug Integer.  See \code{\link{readJDX}} for details.
#'
#' @return A data frame with elements \code{mz} and \code{int}.
#'
#' @importFrom stringr str_locate str_trim
#'
#' @noRd
#'

processDT <- function(VL, params, mode, SOFC, debug = 0) {
  fmt <- VL[1]
  VL <- VL[-1] # Remove the pre-pended format string
  if (debug >= 1) {
    cat("\nProcessing retention time...", VL[1], "\n")
  }
  VL <- VL[-1] # Remove e.g. ##PAGE= T=  now that we have used it for debugging.

  # Get the number points for this page
  npoints <- VL[1]
  npoints <- sub("^\\s*##NPOINTS\\s*=", replacement = "", npoints)
  npoints <- as.integer(npoints)
  if (!is.integer(npoints)) stop("Couldn't find NPOINTS")
  VL <- VL[-1] # remove the NPOINTS line

  # Remove comment only lines entirely
  comOnly <- grepl("^\\$\\$", VL)
  VL <- VL[!comOnly]

  # Remove any comments that follow numbers etc
  VL <- gsub("\\$\\$.*", "", VL)

  ### Step 1. Convert to numeric values
  # TODO this is special for the Waters QDA data sets, check that it complies with AFFN
  # Typical line: 123.45 6.789 separated by space
  xValues <- as.numeric(sub("\\s{1}[0-9]+\\.{0,1}[0-9]*$", "", VL))
  yValues <- as.numeric(sub("^[0-9]+\\.{0,1}[0-9]*\\s{1}", "", VL))

  ### Step 2. Check the integrity of the results
  # Check that we got the right number of data points

  if (debug == 2) cat("\nNPOINTS =", npoints, "\n")
  if (debug == 2) cat("Actual no. data points found  =", length(xValues), "\n")
  if (!npoints == length(xValues)) stop("NPOINTS and length of parsed data don't match")

  ### And we're done...

  xydata <- data.frame(mz = xValues, int = yValues)
  rownames(xydata) <- names(VL)
  return(xydata)
}
