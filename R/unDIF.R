##'
##' Convert from DIF or SQZ Format to a Character String
##'
##' This function is NOT EXPORTED.
##' Users would not normally call this function.  See \code{\link{readJDX}}.
##' Documentation is provided for developers wishing to contribute to the package.
##' 
##' @param string Character.  The string to be converted.
##'
##' @return A string of digits (type character).
##'
##' @aliases unDIF unSQZ
##'
##' @noRd

unDIF <- function(string) {
	string <- gsub("%", "0", string) # effectively means no change
	string <- gsub("J", "1", string)
	string <- gsub("K", "2", string)
	string <- gsub("L", "3", string)
	string <- gsub("M", "4", string)
	string <- gsub("N", "5", string)
	string <- gsub("O", "6", string)
	string <- gsub("P", "7", string)
	string <- gsub("Q", "8", string)
	string <- gsub("R", "9", string)
 	string <- gsub("j", "-1", string)
	string <- gsub("k", "-2", string)
	string <- gsub("l", "-3", string)
	string <- gsub("m", "-4", string)
	string <- gsub("n", "-5", string)
	string <- gsub("o", "-6", string)
	string <- gsub("p", "-7", string)
	string <- gsub("q", "-8", string)
	string <- gsub("r", "-9", string)
	return(string)	
	}
