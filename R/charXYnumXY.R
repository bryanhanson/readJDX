#'
#' Convert Character x,y Data to Numeric x, y Values
#'
#' This function is NOT EXPORTED.
#' Users would not normally call this function.  See \code{\link{readJDX}}.
#' Documentation is provided for developers wishing to contribute to the package.
#'
#' @template VL-arg
#'
#' @return A data frame with elements \code{x} and \code{y}.
#' 
#'
charXYnumXY <- function(VL) {
  # From the standard (can't find the exact reference, but very common in MS)
  # "Groups are separated by a semicolon or space; components
  #  of a group are separated by commas"
  # 
  # Note 1: R automatically handles the exponent if present
  # Note 2: Many of the files are simply one x,y pair per line
  # Note 3: Some files have one x y pair per line (space separator)

  VL <- unlist(strsplit(VL, ";")) # split on any ; nc if not present
  # check to see if there are , present, if so, split on spaces not preceded by a ,
  if (any(grepl(",", VL))) {
    VL <- unlist(strsplit(trimws(VL), "(?<!,)\\s+", perl = TRUE))
    xValues <- as.numeric(sub(",\\s*[+-]{0,1}[0-9]+\\.{0,1}[0-9]*E{0,1}[0-9]*", "", VL))
    yValues <- as.numeric(sub("\\s*[+-]{0,1}[0-9]+\\.{0,1}[0-9]*E{0,1}[0-9]*,", "", VL))
  }

  # if no commas present, this is apparently x space y, one per line, don't process much further
  if (!any(grepl(",", VL))) {
    xValues <- as.numeric(sub("\\s+[+-]{0,1}[0-9]+\\.{0,1}[0-9]*E{0,1}[0-9]*$", "", VL))
    yValues <- as.numeric(sub("^[+-]{0,1}[0-9]+\\.{0,1}[0-9]*E{0,1}[0-9]*\\s+", "", VL))
  }

  data.frame(x = xValues, y = yValues)
}
