#'
#' Determine the type(s) of Compression in Use
#'
#' This function is NOT EXPORTED.
#' Users would not normally call this function.  See \code{\link{readJDX}}.
#' Documentation is provided for developers wishing to contribute to the package.
#'
#' @param VL Character.  The string to be checked.  If a vector of
#' strings (as is a variable list) it will be collapsed to a single string.
#'
#' @param debug Integer. See \code{\link{readJDX}} for details.
#'
#' @return A string giving all compression formats found.
#'
#' @noRd
#'
getComp <- function(VL, debug = 0) {

  # Drop the first line if this is 2D NMR the first line is still stuff saved for debugging
  if (grepl("##PAGE=", VL[1])) VL <- paste(VL[2:length(VL)], collapse = " ")
  if (!grepl("##PAGE=", VL[1])) VL <- paste(VL, collapse = " ")

  AFFN <- PAC <- SQZ <- DIF <- DUP <- FALSE
  if (grepl("[@A-Ia-i]", VL)) SQZ <- TRUE
  if (grepl("[%J-Rj-r]", VL)) DIF <- TRUE
  # Check for + or - with a digit on each side, no space
  # This would be PAC, even if the very first character is a space
  if (grepl("\\d+[\\+\\-]{1}\\d+", VL)) PAC <- TRUE
  if (grepl("[S-Zs]", VL)) DUP <- TRUE
  AFFN <- TRUE # X values are always AFFN

  if (debug %in% c(2, 4)) {
    cat("\nCompression formats in use:\n")
    cat("AFFN =", AFFN, "\n")
    cat("PAC =", PAC, "\n")
    cat("SQZ =", SQZ, "\n")
    cat("DIF =", DIF, "\n")
    cat("DUP =", DUP, "\n")
  }

  ans <- c("AFFN", "PAC", "SQZ", "DIF", "DUP")
  keep <- NA_integer_
  if (AFFN) keep <- c(keep, 1)
  if (PAC) keep <- c(keep, 2)
  if (SQZ) keep <- c(keep, 3)
  if (DIF) keep <- c(keep, 4)
  if (DUP) keep <- c(keep, 5)
  keep <- keep[-1]
  ans[keep]
} # end of getComp
