##'
##' Replicate Duplicate Values
##'
##' This function is NOT EXPORTED.
##' Users would not normally call this function.  See \code{\link{readJDX}}.
##' Documentation is provided for developers wishing to contribute to the package.
##' 
##' @param string Character.  A string to be replicated, as type character.
##' May contain a number or a character or a combination, but the whole is a
##' character string.
##'
##' @param dupstr Character.  A character string which when decoded indicates
##' the number of times to repeat \code{string}.
##'
##' @return An expanded string.
##' 
##' @noRd

repDUPs <- function(string, dupstr) {
	
	if (length(string) > 1) stop("repDUPs only accepts a length 1 string")
	
	# Expand dupstr into an actual number, then duplicate string accordingly.
	# See insertDUPs for a discussion of the meaning and use of S and some other issues.
	
	if (nchar(dupstr) == 1) { # common situation
		if (dupstr == "S") return(rep(string, 1))
		if (dupstr == "T") return(rep(string, 2))
		if (dupstr == "U") return(rep(string, 3))
		if (dupstr == "V") return(rep(string, 4))
		if (dupstr == "W") return(rep(string, 5))
		if (dupstr == "X") return(rep(string, 6))
		if (dupstr == "Y") return(rep(string, 7))
		if (dupstr == "Z") return(rep(string, 8))
		if (dupstr == "s") return(rep(string, 9))	
	}
	
	if (nchar(dupstr) > 1) {
		firstc <- substring(dupstr, 1, 1) # first character
		firsti <- NA_integer_ # first character as integer, momentarily
		rest <- substring(dupstr, 2, nchar(dupstr)) # rest of the string
		if (firstc == "S") firsti <- 1L
		if (firstc == "T") firsti <- 2L
		if (firstc == "U") firsti <- 3L
		if (firstc == "V") firsti <- 4L
		if (firstc == "W") firsti <- 5L
		if (firstc == "X") firsti <- 6L
		if (firstc == "Y") firsti <- 7L
		if (firstc == "Z") firsti <- 8L
		if (firstc == "s") firsti <- 9L
		
		if (is.na(firsti)) stop("Could not find the DUP code")
		
		repval <- as.numeric(paste(firsti, rest, sep = ""))
		return(rep(string, repval))
		
	}
	
	}
