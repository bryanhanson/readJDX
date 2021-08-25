#'
#' Process a Single Variable List from a JCAMP-DX file
#'
#' This function is NOT EXPORTED.
#' Users would not normally call this function.  See \code{\link{readJDX}}.
#' Documentation is provided for developers wishing to contribute to the package.
#'
#' @template VL-arg
#' @template SOFC-arg
#' @template debug-arg
#' @template mode-arg
#' @template params-arg
#'
#' @return A data frame with elements \code{x} and \code{y}, unless the file contains 2D NMR, in which case a matrix.
#'
#' @noRd
#'
processVariableList <- function(VL, params, mode, SOFC, debug = 0) {

  fmt <- VL[1]

  # For each mode:
  #
  # Step 1. Strip off non-numerical lines at beginning and end.
  # After this, VL should consist only of numerical values or their surrogates,
  # but it is possible there are still some comments embedded within VL.
  # These will be handled later.
  #
  # Step 2. Dispatch to process* functions
  # As currently implemented, this is not completely universal.  For instance, there is nothing
  # (I can find) that says a 2D NMR, XII, XRR could not be in AFFN format, but I've never seen it.
  # TODO: In the future it may be necessary to peek at the VL and determine the internal format
  # and dispatch on that.

  if (mode == "XYY") {
    # Keep line 1 of VL; it has the format
    VL <- VL[-c(2, length(VL))]

    xydata <- processXYY(VL, params, mode, SOFC, debug = debug)
    return(xydata)
  }

  if (mode == "XYXY") {
    # Keep line 1 of VL; it has the format
    VL <- VL[-c(2, length(VL))]

    xydata <- processXYXY(VL, params, mode, SOFC, debug = debug)
    return(xydata)
  }

  if (mode == "NMR_1D") {

    if (fmt == "XRR") {
      # Keep line 1 of VL; it has the format
      VL <- VL[-c(2, 3)]
    }

    if (fmt == "XII") {
      # Keep line 1 of VL; it has the format
      VL <- VL[-c(2, 3, length(VL))]
    }

    xydata <- processXYY(VL, params, mode, SOFC, debug = debug)
    return(xydata)
  }

  if (mode == "NMR_2D") {
    # Keep line 1 of VL; it has the format
    # Keep line 2 of VL for debug reporting during decompression (e.g. ##PAGE= F1= 4.7865152724775)
    # Keep line 3 for possible checking
    VL <- VL[-4]

    xydata <- process2DNMR(VL, params, mode, SOFC, debug = debug)
    return(xydata)
  }


  if (mode == "LC_MS") {
    # Keep line 1 of VL; it has the format
    # Keep line 2 of VL for debug reporting during decompression (e.g. ##PAGE= T= )
    # Keep line 3 for checking results: NPOINTS
    # Remove line 4: ##DATA TABLE= (XI..XI), PEAKS or similar
    VL <- VL[-4]

    xydata <- processLCMS(VL, SOFC, debug = debug) # does not use params currently
    return(xydata)
  }

}
