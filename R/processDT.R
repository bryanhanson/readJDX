#'
#' Extract the values in JCAMP-DX file with a "Data Table" variable list with X, Y Values
#'
#' This function is NOT EXPORTED.
#' Users would not normally call this function.  See \code{\link{readJDX}}.
#' Documentation is provided for developers wishing to contribute to the package.
#'
#' @param VL Character.  The variable list to be processed as a character vector. Includes one pre-pended
#' line giving the format of data (e.g. XYY, XRR, XII, PEAK_TABLE, etc).
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

processDT <- function(VL, SOFC, debug = 0) {

  # This function handles ##DATA TABLE= (XI..XI), PEAKS format, which is not clearly 
  # documented in the standard, but at least is consistent.
  # Found in Waters QDA exported files, where the data lines are like 123.45 6.789

  if (debug >= 1) {
    cat("\nProcessing retention time...", VL[2], "\n")
  }
  # Remove the pre-pended format string & ##PAGE= T=  now that we have used it for debugging.
  VL <- VL[-c(1, 2)] 

  # Get the number of data points for this page
  # I'm not certain if we can get the far if it is not... we may have wrong lines in that case
  np <- grepl("^\\s*##NPOINTS\\s*=",, VL[1]) # see if NPOINTS is present
  if (!np) warning("Could not find NPOINTS, continuing")
  if (np) {
    npoints <- VL[1]
    npoints <- sub("^\\s*##NPOINTS\\s*=", replacement = "", npoints)
    npoints <- as.integer(npoints)
    if (SOFC) if (!is.integer(npoints)) stop("Couldn't find NPOINTS")
    VL <- VL[-1] # remove the NPOINTS line
  }

  # Remove comment only lines entirely (single line comments; not checking for multiline comments)
  comOnly <- grepl("^\\$\\$", VL)
  VL <- VL[!comOnly]

  # Remove any comments at the end of a line
  VL <- gsub("\\$\\$.*", "", VL)

  ### Step 1. Convert to numeric values
  # Typical line: 123.45 6.789 separated by space so AFFN
  # processPT has a somewhat more standard compliant bit of code
  xValues <- as.numeric(sub("\\s{1}[0-9]+\\.{0,1}[0-9]*$", "", VL))
  yValues <- as.numeric(sub("^[0-9]+\\.{0,1}[0-9]*\\s{1}", "", VL))

  ### Step 2. Check the integrity of the results
  # Check that we got the right number of data points
  if (np) {
    if (debug == 2) cat("\nNPOINTS =", npoints, "\n")
    if (debug == 2) cat("Actual no. data points found  =", length(xValues), "\n")
    if (!npoints == length(xValues)) stop("NPOINTS and length of parsed data don't match")
  }

  ### And we're done...

  xydata <- data.frame(mz = xValues, int = yValues)
  rownames(xydata) <- names(VL)
  return(xydata)
}
