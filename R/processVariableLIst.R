#'
#' Process (in most cases decompress) a Single Variable List from a JCAMP-DX file
#'
#' This function is NOT EXPORTED.
#' Users would not normally call this function.  See \code{\link{readJDX}}.
#' Documentation is provided for developers wishing to contribute to the package.
#'
#' @param VL Character.  A vector of character strings which contains
#' the variable list.  First line should be the format code (an
#' extra line inserted by this package).  Depending upon mode, there
#' may be some other stuff that still needs to be stripped off to get to just numbers.
#'
#' @param params Numeric. Vector of parameters from file header.
#'
#' @param  lineNos Two integers giving the first and last lines
#'         of the variable list in the original file. Used for debugging responses.
#'
#' @param mode Character. One of c("XY_data", "NMR_1D", "NMR_2D", "LC_MS", "PEAK_TABLE")
#'
#' @param debug Integer.  See \code{\link{readJDX}} for details.
#'
#' @param SOFC Logical.  See \code{\link{readJDX}} for details.
#'
#' @return A data frame with elements \code{x} and \code{y}, unless the file contains 2D NMR, in which case a matrix.
#'
#' @noRd
#'
processVariableList <- function(VL, params, mode, lineNos, SOFC, debug = 0) {

  # Strip off non-numerical lines at beginning and end,
  # and adjust lineNos accordingly.
  # After this, VL should consist only of numerical values or their surrogates,
  # but it is possible there are still some comments embedded within VL.
  # These will be handled during decompression.

  lineNos <- unlist(lineNos)
  fmt <- VL[1]

  if (mode == "XY_data") {
    # Keep line 1 of VL; it has the format
    VL <- VL[-c(2, length(VL))]
    st <- lineNos[1] + 1
    end <- lineNos[2] - 1
    lineNos <- c(NA_integer_, st:end)
  }

  if (mode == "PEAK_TABLE") {
    # Keep line 1 of VL; it has the format
    VL <- VL[-c(2, length(VL))]
    st <- lineNos[1] + 1
    end <- lineNos[2] - 1
    lineNos <- c(NA_integer_, st:end)
  }

  if (mode == "NMR_1D") {
    if (fmt == "XRR") {
      # Keep line 1 of VL; it has the format
      VL <- VL[-c(2, 3)]
      st <- lineNos[1] + 2
      end <- lineNos[2]
      lineNos <- c(NA_integer_, st:end)
    }

    if (fmt == "XII") {
      # Keep line 1 of VL; it has the format
      VL <- VL[-c(2, 3, length(VL))]
      st <- lineNos[1] + 2
      end <- lineNos[2] - 1
      lineNos <- c(NA_integer_, st:end)
    }
  }

  if (mode == "NMR_2D") {
    # Keep line 1 of VL; it has the format
    # Keep line 2 of VL for debug reporting during decompression (e.g. ##PAGE= F1= 4.7865152724775)
    VL <- VL[-c(3, 4)]
    st <- lineNos[1]
    end <- lineNos[2]
    lineNos <- c(NA_integer_, st, (st + 3L):end)
  }


  if (mode == "LC_MS") {
    # Keep line 1 of VL; it has the format
    # Keep line 2 of VL for debug reporting during decompression (e.g. ##PAGE= T= )
    # Keep line 3 for checking results
    # Remove line 4: ##DATA TABLE= (XI..XI), PEAKS
    VL <- VL[-4]
    st <- lineNos[1]
    end <- lineNos[2]
    lineNos <- c(NA_integer_, st, (st + 2L):end)
  }

  # Dispatch based on fmt
  # As currently implemented, this is not completely universal.  For instance, there is nothing
  # (I can find) that says a 2D NMR, XII, XRR could not be in AFFN format, but I've never seen it.
  # In the future it may be necessary to peek at the VL and determine the internal format
  # and dispatch on that.

  names(VL) <- paste("Line", lineNos, sep = "_") # name it for debugging purposes downstream

  if ((fmt == "XRR") | (fmt == "XII") | (fmt == "NMR_2D") | (fmt == "XYY")) {
    xydata <- processXYY(VL, params, mode, SOFC, debug = debug)
    return(xydata)
  }

  if (fmt == "PEAK_TABLE") {
    xydata <- processPT(VL, params, mode, SOFC, debug = debug)
    return(xydata)
  }

  if (fmt == "LC_MS") {
    xydata <- processDT(VL, params, mode, SOFC, debug = debug)
    return(xydata)
  }

  stop("Did not find a valid format") # Should never reach this line...
}
