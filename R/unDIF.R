#'
#' Convert from DIF Format to a Numeric Vector
#'
#' This function is NOT EXPORTED.
#' Users would not normally call this function.  See \code{\link{readJDX}}.
#' Documentation is provided for developers wishing to contribute to the package.
#' 
#' @param string Character.  The string to be converted.
#'
#' @return A data frame. First element are the numeric values.  Second is the logical vector flagging DIF codes.
#'
#' @aliases unDIF unSQZ
#'
#' @importFrom stringr str_detect str_replace_all
#'
#' @noRd

unDIF <- function(string) {
	
	# DIF codes can be interspersed with SQZ etc, so we have to find the DIFs and deal with them.
	# Also, as note by Daniel Jacob in Github issue #6, the y value check only applies if the 
	# very last entry is a DIF code, so we need to know that as well.
	pat <- "[%JKLMNOPQRjklmnopqr]"
	dflag <- str_detect(string, pat) # flag to mark where the DIF codes are in the string
	string <- str_replace_all(string,
		c("%" = "0",  # effectively the same as a DUP character (add nothing, i.e. repeat the character)
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
		  "r" = "-9"))
		  
	string <- as.numeric(string)
	values <- rep(NA_real_, length(string))
	for (i in 1:length(values)) { # amounts to cumsum over only selected portions of the string
		if (!dflag[i]) {values[i] <- string[i]; next}
		if (dflag[i]) values[i] <- string[i] + values[i-1]
	}
	
	res <- data.frame(values = values, DIFmode = dflag)
	res$values
	}
	