#'
#' Convert Character x,y Data to Numeric x, y Values
#'
#' This function is NOT EXPORTED.
#' Users would not normally call this function.  See \code{\link{readJDX}}.
#' Documentation is provided for developers wishing to contribute to the package.
#'
#' @param VL Character.  The variable list to be processed as a character vector.
#'        All non-numeric data should have already been removed.
#'
#' @return A data frame with elements \code{x} and \code{y}.
#' 
#' @noRd
#'
charXYnumXY <- function(VL) {
  # From the standard (can't find the exact reference, but very common in MS)
  # "Groups are separated by a semicolon or space; components
  #  of a group are separated by commas"
  # 
  # Note 1: R automatically handles the exponent if present
  # Note 2: Many of the files are simply one x,y pair per line
  #

  # Split on any ; or space not preceeded by a , (negative lookbehind, need perl)
  VL <- unlist(strsplit(VL, ";"))
  VL <- unlist(strsplit(trimws(VL), "(?<!,)\\s+", perl = TRUE))
  xValues <- as.numeric(sub(",\\s*[+-]{0,1}[0-9]+\\.{0,1}[0-9]*E{0,1}[0-9]*", "", VL))
  yValues <- as.numeric(sub("\\s*[+-]{0,1}[0-9]+\\.{0,1}[0-9]*E{0,1}[0-9]*,", "", VL))
  data.frame(x = xValues, y = yValues)
}
