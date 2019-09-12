##'
##' Determine the type(s) of Compression in Use
##'
##' This function is NOT EXPORTED.
##' Users would not normally call this function.  See \code{\link{readJDX}}.
##' Documentation is provided for developers wishing to contribute to the package.
##' 
##' @param string Character.  The string to be checked.  If a vector of
##' strings it will be collapsed to a single string.
##'
##' @param debug Integer.  See \code{\link{readJDX}} for details.
##'
##' @return A string giving all compression formats found.
##' 
##' @noRd
##'
getJDXcompression <- function (string, debug = 0){

# There are 4 possible compression formats:
# AFFN: ASCII numbers separated by at least one space
# PAC: Numbers separated by 1 space, + or -
# SQZ: Delimiter, leading digit and sign are replaced by a pseudo-digit
# DIF: like SQZ, but giving differences between each data value
# and DUP: Not a format per se, but a method of signifying repeated values.
# "E" is allowed in AFFN to signify exponential, and probably in PAC too.
# JCAMP-DX 4.24 states (section 5.2) that any combination is possible,
# But it appears that AFFN is typically never combined with any other format.

	if (!requireNamespace("stringr", quietly = TRUE)) {
		stop("You need to install package stringr to use this option")
		}

	instring <- string # save a copy for debug reporting
	
	# For the purposes of determining the format, all comments should have been dropped
	# before arriving here, an comments could contain any characters which will confuse
	# the process here.
	string <- paste(string, collapse = " ")

	AFFN <- PAC <- SQZ <- DIF <- FMT <- DUP <- FALSE # FMT is a flag to indicate some format has been found

	if (debug == 4) { 	# Reporting these now shows strings that will fail
						# later when the format it not understood
		message("\nDetermining compression method...")
		message("1st 5 lines (x values stripped off):")
		print(instring[1:5])
		}

	if (grepl("[@A-Ia-i]", string)) {SQZ <- TRUE; FMT <- TRUE}
	
	if (grepl("[%J-Rj-r]", string)) {DIF <- TRUE; FMT <- TRUE}
		
	# Check for + or - with a digit on each side, no space
	# This would be PAC, even if the very first character is a space
	if (grepl("\\d+[\\+\\-]{1}\\d+", string)) {PAC <- TRUE; FMT <- TRUE}
	
	if (!FMT) AFFN <- TRUE # Only remaining option
		
	# Check to see if DUP is in use
	if (grepl("[S-Zs]", string)) DUP <- TRUE

	if (debug %in% c(2, 4)) {
		message("\nCompression formats in use:")
		cat("AFFN =", AFFN, "\n")
		cat("PAC =", PAC, "\n")
		cat("SQZ =", SQZ, "\n")
		cat("DIF =", DIF, "\n")
		cat("DUP =", DUP, "\n")
		}

	# Check that some format was recognized.
	if (!((AFFN) | (PAC) | (SQZ) | (DIF))) stop("JCAMP-DX compression format not recognized")

	# Check that AFFN is not (apparently) mixed with any of the others
	ASDF <- (PAC | SQZ | DIF | DUP)
	if (AFFN & ASDF) stop("AFFN format seems to be mixed with ASDF format")
			
	# Set up return value
	
	ans <- c("AFFN", "PAC", "SQZ", "DIF", "DUP")
	keep <- NA_integer_
	if (AFFN) keep <- c(keep, 1)
	if (PAC) keep <- c(keep, 2)
	if (SQZ) keep <- c(keep, 3)
	if (DIF) keep <- c(keep, 4)
	if (DUP) keep <- c(keep, 5)
	keep <- keep[-1]
	return(ans[keep])
	
	} # end of getJCAMPcompression
