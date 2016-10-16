##'
##' Replicate Duplicate Values
##'
##' This function is NOT EXPORTED.
##' Users would not normally call this function.  See \code{\link{readJDX}}.
##' Documentation is provided for developers wishing to contribute to the package.
##' 
##' @param string Character.  A string to be replicated, as type character.
##' May be a number or a character or a combination.
##'
##' @param dupchar Character.  A single character from \code{[S-Zs]} indicating
##' the number of times to repeat \code{string}.
##'
##' @return An expanded string.
##' 
##' @noRd

repDUPs <- function(string, dupchar) {
	
	if (length(string) > 1) stop("expandDUPs only accepts a length 1 string")
	# Expand and return dupchar
	
	# Note: DUP value of S appears to have no purpose.
	# Consistent with this, it doesn't appear in test files that I have checked
	# other than SPECFILE.DX where it appears many times
	
	if (dupchar == "S") {
		stop("S found as a DUP code!")
		}
	if (dupchar == "T") return(rep(string, 2))
	if (dupchar == "U") return(rep(string, 3))
	if (dupchar == "V") return(rep(string, 4))
	if (dupchar == "W") return(rep(string, 5))
	if (dupchar == "X") return(rep(string, 6))
	if (dupchar == "Y") return(rep(string, 7))
	if (dupchar == "Z") return(rep(string, 8))
	if (dupchar == "s") return(rep(string, 9))
	}
