##'
##' Convert from DIF Format to a Numeric Vector
##'
##' This function is NOT EXPORTED.
##' Users would not normally call this function.  See \code{\link{readJDX}}.
##' Documentation is provided for developers wishing to contribute to the package.
##' 
##' @param string Character.  The string to be converted.
##'
##' @return A numeric vector.
##'
##' @aliases unDIF unSQZ
##'
##' @importFrom stringr str_detect str_replace_all
##'
##' @noRd

unDIF <- function(string) {
	pat <- "[%JKLMNOPQRjklmnopqr]"
	# dflag <- str_detect(string, pat) # flag to mark where the DIF codes are in the string
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
		  
	# string <- as.numeric(string)
	# values <- rep(NA_real_, length(string))
	# for (i in 1:length(values)) { # amounts to cumsum over only selected portions of the string
		# if (!dflag[i]) {values[i] <- string[i]; next}
		# if (dflag[i]) values[i] <- string[i] + values[i-1]
	# }
	
	values <- cumsum(as.numeric(string))
	return(values)	
	}
	
	
	
# Test line
#line <- c("32000", "32000", "32000", "n", "o", "k", "L", "L", "m", "j1", "p", "%", "K", "J", "%", "j", "j", "M", "M", "%", "m", "n", "n", "l", "%", "L", "K", "J", "%", "K", "j", "n", "j2", "j9", "k9", "k6", "p", "J9", "K7", "J4", "o", "j2", "k", "J7", "K8", "K3", "J2", "L", "%")

#line2 <- c("31992", "k", "j", "J", "L", "L", "K", "J", "%", "k", "m", "j", "K", "L", "K", "l", "l", "l", "K", "K", "%", "k", "j", "%", "J", "J", "K", "%", "j", "J", "K", "J", "%", "%", "%", "k", "k", "k", "%", "%", "%", "%", "%", "j", "%", "%", "%", "L", "M", "%", "j", "K", "k", "l", "l", "j", "L", "M", "L", "%", "%", "%", "%", "%", "8", "k", "%", "j", "J", "%")
