##'
##' Insert Duplicate Entries
##'
##' This function is NOT EXPORTED.
##' Users would not normally call this function.  See \code{\link{readJDX}}.
##' Documentation is provided for developers wishing to contribute to the package.
##' 
##' @param string Character vector.  Partially processed lines from the
##' original data table.
##'
##' @param debug Integer.  See \code{\link{readJDX}} for details.
##'
##' @param nlmd Integer.  The number of lines of meta data.  Used in debug reporting.
##'
##' @return A string.
##' 
##' @noRd

insertDUPs <- function(string, debug, nlmd) {					
	# Take a character vector with DUPs and expand them.
	# This function will not be efficient b/c we don't know the final length
	# in advance

	# There are 2 cases to keep in mind:
 	# 1. DUP character has a number ahead of it.  Duplicate this number.
 	# 2. DUP character has a DIF character ahead of it.
 	# Either way, just repeat whatever is ahead of the DIF character.
 	# The other unpacking steps are handled elsewhere.
 	
	dup <- grep("[S-Zs]{1}", string)
	
	yS <- NA_character_

	if ((debug == 3) | (debug == 4)) {
		message("Finding and expanding DUP codes")
		cat("DUPs found on the following lines of the original file:\n")
		print(dup + nlmd)
		}
	
	if (debug == 4) {
		cat("\n")
		message("Original lines & expanded lines:")
		}
		
	for (i in 1:length(string)) {

		if (!i %in% dup) {
			yS <- c(yS, string[i]) # no DUPs this line
			}
			
		if (i %in% dup) {
			line <- string[i]
			line <- unlist(strsplit(line, "\\s+"))
			newline <- NA_character_
			for (j in 1:length(line)) {
				newline <- c(newline, line[j]) # build up the new string one char at a time (ugly)
				if (grepl("[S-Zs]", line[j])) {
					tmp <- repDUPs(line[j-1], line[j])
					# Remove the value you are replacing (DUP count / tmp includes it)
					lnl <- length(newline)
					newline <- c(newline[-c((lnl-1):lnl)], tmp)
					}
				}
			if (debug == 4) {
				cat("\nOriginal line:", i + nlmd, "\n")
				cat("\t", line, "\n")
				cat("\nLine", i + nlmd, "with DUPs inserted:\n")
				cat("\t", newline[-1], "\n")
				cat("\n--------------------\n")
				}
			newline <- paste(newline[-1], collapse = " ")
			yS <- c(yS, newline)
			}
		}

	return(yS[-1])
	}
