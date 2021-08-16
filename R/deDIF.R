#'
#' Process DIF Encoded Strings
#'
#' This function is NOT EXPORTED.
#' Users would not normally call this function.  See \code{\link{readJDX}}.
#' Documentation is provided for developers wishing to contribute to the package.
#'
#' @param i Integer. Essentially the index into \code{lineList}.
#'
#' @param lineList A list, named with the line numbers.  Elements are character vectors, whose entries
#' are named with the ASDF compression mode.  X values still present.
#'
#' @param debug Integer.  See \code{\link{readJDX}} for details.
#'
#' @return A numeric vector.
#'
#'
deDIF <- function(i, lineList, debug = 0) {

  # The JCAMP std states that in DIF form the first entry on a line
  # is an X value, the 2nd value is in SQZ form
  # and the subsequent values are differences between *adjacent* values:
  # X, Y1, D1, D2, D3, D4...
  # corresponding, after conversion, to:
  # X, Y1, Y2, Y3, Y4, Y5...
  # where D3 is the offset from D2 for example. So to construct
  # Y4 you must do Y1 + D1 + D2 + D3
  # However, DIF mode can be mixed with other modes, so
  # DUPs should be handled before arriving at this function,
  # and SQZ values should have been converted to numbers already.
  # This is the last step, only the DIF'ing needs to be done.

  # As received, the string is composed of numeric values and DIF characters

  # Convert the DIF characters to the corresponding numbers and fix offset

  if (debug == 6) tmp_string <- lineList[[i]] # save copy for reporting


  # A single string has arrived here, as this was called on a list via lapply
  string <- unDIF(lineList[[i]])

  if (debug == 6) {
    cat("\nUndoing DIF compression:\n")
    cat("\n  ", names(lineList[i]), "passed to deDIF:\n")
    print(tmp_string)
    cat("\n  ", names(lineList[i]), "as processed by deDIF:\n")
    print(string)
  }

  string
} # end of deDIF
