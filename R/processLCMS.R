#'
#' Extract the x, y Values in a LC-MS Variable List
#'
#' This function is NOT EXPORTED.
#' Users would not normally call this function.  See \code{\link{readJDX}}.
#' Documentation is provided for developers wishing to contribute to the package.
#'
#' @param VL Character.  The variable list to be processed as a character vector. Includes one pre-pended
#' line giving the format of data (e.g. XYY, XRR, XII, PEAK_TABLE, etc).
#'
#' @template SOFC-arg
#' @template debug-arg
#'
#' @return A data frame with elements \code{mz} and \code{int}.
#'
#' @importFrom stringr str_locate str_trim
#'
#' @noRd
#'

processLCMS <- function(VL, SOFC, debug = 0) {

  # This function handles ##DATA TABLE= (XY..XY), PEAKS format
  # This is one time point/one page in a spectral series (Lampen 1994 sec 5.5.3)
  # Data are required to be in AFFN as x, y pairs, one or more to line
  # Found in Waters QDA exported files as (XI..XI) meaning mass and intensity

  if (debug >= 1) {
    cat("\nProcessing retention time...", VL[2], "\n")
  }
  # Remove the pre-pended format string & ##PAGE= T=  now that we have used it for debugging.
  VL <- VL[-c(1, 2)] 

  # Get the number of data points for this page; this is required
  np <- grepl("^\\s*##NPOINTS\\s*=", VL[1]) # verify NPOINTS is present
  if (!np) stop("Could not find NPOINTS, continuing")
  npoints <- VL[1]
  npoints <- sub("^\\s*##NPOINTS\\s*=", replacement = "", npoints)
  npoints <- as.integer(npoints)
  if (SOFC) if (!is.integer(npoints)) stop("Found NPOINTS but could not convert to integer")
  VL <- VL[-1] # remove the NPOINTS line

  # Remove comment only lines entirely (single line comments; not checking for multiline comments)
  comOnly <- grepl("^\\$\\$", VL)
  VL <- VL[!comOnly]

  # Remove any comments at the end of a line
  VL <- gsub("\\$\\$.*", "", VL)

  # Verify expected numerical format
  comp <- getComp(VL)
  if (length(comp) != 1L) {
    cat("\nCompression found:", comp, "\n")
    stop("For LC-MS data is expected to be in AFFN format")
  }
  if (comp != "AFFN") {
    cat("\nCompression found:", comp, "\n")
    stop("For LC-MS data is expected to be in AFFN format")
  }

  ### Step 1. Convert to numeric values
  xValues <- charXYnumXY(VL)$x
  yValues <- charXYnumXY(VL)$y

  ### Step 2. Check the integrity of the results
  # Check that we got the right number of data points
  if (debug == 2) {
    cat("\nNPOINTS =", npoints, "\n")
    cat("Actual no. data points found  =", length(xValues), "\n")
  }
  if (!npoints == length(xValues)) stop("NPOINTS and length of parsed x-values don't match")
  if (!npoints == length(yValues)) stop("NPOINTS and length of parsed y-values don't match")

  ### And we're done...

  xydata <- data.frame(mz = xValues, int = yValues)
  rownames(xydata) <- names(VL)
  return(xydata)
}
