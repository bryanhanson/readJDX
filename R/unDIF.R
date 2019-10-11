#'
#' Convert from DIF Format to a Numeric Vector
#'
#' This function is NOT EXPORTED.
#' Users would not normally call this function.  See \code{\link{readJDX}}.
#' Documentation is provided for developers wishing to contribute to the package.
#'
#' @param string Character. A character vector to be converted. Named with compression codes.
#'
#' @return A numeric vector with the final values, named with the compression codes.
#'
#' @importFrom stringr str_detect str_replace_all
#'
# @noRd
#'

unDIF <- function(string) {

  stringNames <- names(string) # save for updating later
  
  # DIF codes can be interspersed with SQZ etc, so locate the DIFs and deal with them.
    
  # Mark the DIF codes
  pat <- "[%J-Rj-r]"
  dflag <- str_detect(string, pat) # flag to mark where the DIF codes are in the string
  # note that entries labeled DUP will still be flagged correctly since the character string has
  # the DIF code in it (that means one cannot use the names as the flag)
  
  # Replace the characters with numbers
  string <- str_replace_all(
    string,
    c(
      "%" = "0", # effectively the same as a DUP character (add nothing, i.e. repeat the character)
      "J" = "1",
      "K" = "2",
      "L" = "3",
      "M" = "4",
      "N" = "5",
      "O" = "6",
      "P" = "7",
      "Q" = "8",
      "R" = "9",
      "j" = "-1",
      "k" = "-2",
      "l" = "-3",
      "m" = "-4",
      "n" = "-5",
      "o" = "-6",
      "p" = "-7",
      "q" = "-8",
      "r" = "-9"
    )
  )

  string <- as.numeric(string) # call it numstring?
  values <- rep(NA_real_, length(string))

  # Do the DIF
  # Note X values are automatically skipped over since the first two entries are NUM SQZ
  for (i in 1:length(values)) { # amounts to cumsum over only selected portions of the string
    if (!dflag[i]) values[i] <- string[i]
    if (dflag[i]) values[i] <- string[i] + values[i - 1]
  }

  names(values) <- stringNames
  values
}
