#'
#' Insert Duplicate Entries
#'
#' This function is NOT EXPORTED.
#' Users would not normally call this function.  See \code{\link{readJDX}}.
#' Documentation is provided for developers wishing to contribute to the package.
#'
#' @param line A character vector. Entries are named with the ASDF compression mode.  X values still present.
#'
#' @template debug-arg
#'
#' @return A string representing a processed line.
#'
#' @noRd
#'

insertDUPs <- function(line, debug) {
  # Inspect a character vector for DUPs and expand them if found.
  # This function will not be efficient b/c we can't know the final
  # string length in advance.

  # DUP codes are [S-Zs].  T means repeat the value 2x, but this includes the
  # original value (see sec. 5.9 of McDonald 1988).  The meaning of S,
  # repeat 1x including the original value, was at first opaque since
  # it amounts to doing nothing at all.  However, it was
  # pointed out in an e-mail from Peter Lampen that S2 is a valid
  # DUP string which is translated as 12 repeats.

  # DUPs were already and identified by name before arriving here, no need to relocate

  newLine <- NA_character_ # new string ready to grow

  if (debug == 5) cat("\nProcessing DUP values...\n")

  for (i in 1:length(line)) {

    # All of this has potential to fail if there is no i - 1 value, but that seems
    # impossible given the standard
    if (names(line[i]) != "DUP") newLine <- c(newLine, line[i]) # nothing to change

    if (names(line[i]) == "DUP") { # found a DUP to process
      dupEntries <- repDUPs(line[i - 1], line[i]) # returns the original value + replicates
      lenDE <- length(dupEntries)
      names(dupEntries) <- c(names(line[i - 1]), rep("DUP", (lenDE - 1)))
      newLine <- c(newLine[-length(newLine)], dupEntries)
    }
  }

  newLine <- newLine[-1]

  if (debug == 5) {
    cat("\nOriginal line:\n")
    print(line)
    cat("\nLine with DUPs inserted:\n")
    print(newLine)
    cat("\n--------------------\n")
  }

  newLine
}
