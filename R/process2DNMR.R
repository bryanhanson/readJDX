#'
#' Extract the x, y Values in a 2D NMR Variable List
#'
#' This function is NOT EXPORTED.
#' Users would not normally call this function.  See \code{\link{readJDX}}.
#' Documentation is provided for developers wishing to contribute to the package.
#'
#' @section Note:
#' This function is analogous to \code{\link{processLCMS}} but differs in detail.
#'
#' @template VL-arg
#' @template SOFC-arg
#' @template debug-arg
#' @template params-arg
#' @template mode-arg
#'
#' @return A data frame with elements \code{x} and \code{y}.
#'
#' @importFrom stringr str_locate str_trim
#'
#' @noRd
#'

process2DNMR <- function(VL, params, mode, SOFC, debug = 0) {

  # This function handles ##DATA TABLE= (XY..XY), PEAKS format
  # This is one time point/one page in a spectral series (Lampen 1994 sec 5.5.3)
  # Data are required to be in AFFN as x, y pairs, one or more to line
  # Found in Waters QDA exported files as (XI..XI) meaning mass and intensity

  # arguments params, mode is here simply to be passed on to processXYY
  
  if (mode != "NMR_2D") stop("You have landed in the 2D NMR processing function but the mode is wrong")

  if (debug >= 1) {
    cat("\nProcessing F2 spectra...", VL[2], "\n")
  }
  # Remove the ##PAGE= F1=  now that we have used it for debugging (Line 2)
  # Leave the fmt which is in position 1, it will be removed by processXYY
  # Remove the next line, Line 3, (for now) e.g. ##FIRST=     26731.4038939631, 8409.28819444445, -7112
  # but in the future we might use this for checking in this function or another one

  VL <- VL[-c(2, 3)] 

  # Verify expected numerical format
  # So far, the only 2D NMR files we've seen are always ASDF; check to make sure it's not AFFN
  comp <- getComp(VL)
  if (length(comp) == 1L) {
    if (comp == "AFFN") stop("readJDX cannot process 2D NMR files that use AFFN numerical format")
  }

  ### Step 1. Convert to numeric values
  xydata <- processXYY(VL, params, mode, SOFC, debug = debug)

  ### Step 2. Check the integrity of the results
  # TODO: this is where we can possibly run a check

  ### And we're done...

  return(xydata)
}
