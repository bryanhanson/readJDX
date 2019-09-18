#'
#' Insert Duplicate Entries
#'
#' This function is NOT EXPORTED.
#' Users would not normally call this function.  See \code{\link{readJDX}}.
#' Documentation is provided for developers wishing to contribute to the package.
#' 
#' @param string Character vector.  Partially processed lines from the
#' original variable list.
#'
#' @param debug Integer.  See \code{\link{readJDX}} for details.
#'
#' @return A string.
#' 
#' @noRd

insertDUPs <- function(string, debug) {					
	# Inspect a character vector for DUPs and expand them if found.
	# This function will not be efficient b/c we can't know the final
	# string length in advance.

	# DUP codes are [S-Zs].  T means repeat the value 2x, but this includes the
	# original value (see sec. 5.9 of McDonald 1988).  The meaning of S, 
	# repeat 1x including the original value, was at first opaque since
	# it amounts to doing nothing at all.  However, it was
	# pointed out in an e-mail from Peter Lampen that S2 is a valid
 	# DUP string which is translated as 12 repeats.
 	
 	# When a DUP string is found we duplicate whatever is ahead of it.

	pat <- "[S-Zs]{1}[0123456789]*"
	dup <- grep(pat, string) # identify which string elements have a DUP
	
	yS <- NA_character_ # new y string ready to grow
	
	if (debug == 6) message("\nProcessing DUP values...")
	
	# Better if string was named here as it is elsewhere for debug reporting (see below)
	
	for (i in 1:length(string)) {

		if (!i %in% dup) {
			yS <- c(yS, string[i]) # no DUPs this line, just append the current piece
			}
			
		if (i %in% dup) {
			line <- string[i]
			line <- unlist(strsplit(line, "\\s+"))
			newline <- NA_character_ # grow this string
			for (j in 1:length(line)) {
				newline <- c(newline, line[j]) # build up the new string one char at a time (ugly)
				if (grepl(pat, line[j])) {
					tmp <- repDUPs(line[j-1], line[j])
					# Remove the value you are replacing (DUP count / tmp includes it)
					lnl <- length(newline)
					newline <- c(newline[-c((lnl-1):lnl)], tmp)					}
				}
			if (debug == 6) {
				cat("\nOriginal line:", names(string)[i], "\n")
				cat("\t", line, "\n")
				cat("\nLine", names(string)[i], "with DUPs inserted:\n")
				cat("\t", newline[-1], "\n")
				cat("\n--------------------\n")
				}
			newline <- paste(newline[-1], collapse = " ")
			yS <- c(yS, newline)
			}
		}

	return(yS[-1])
	}
