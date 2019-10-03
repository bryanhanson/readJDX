#'
#' Carry Out the y Value Check
#'
#' This function is NOT EXPORTED.
#' Users would not normally call this function.  See \code{\link{readJDX}}.
#' Documentation is provided for developers wishing to contribute to the package.
#'
#' @param lineList A list of numeric vectors to be checked.  Named with line numbers.
#'        Individual numbers are named with the ASDF code.  X values still present!
#'
#' @param debug Integer.  See \code{\link{readJDX}} for details.
#'
#' @return A list of numeric vectors, after the y check has been done and the extra
#'         values removed. It will stop if there is a problem.
#' 
# @noRd
#'

yValueCheck <- function(lineList, debug = 0) {
	
	if (debug == 6) message("\nCarrying out Y value check...")
	
	lineNames <- names(lineList) # grab to re-use later when nuked
		
	# Carry out the Y value check required by the standard
	# First value on a line should = last value on the prev. line, IFF
	# the last value was in DIF mode (per careful reading of the standard,
	# as pointed out by Daniel Jacob in Github issue #6)
	# Names must be stripped for the all.equal check below.

	# Figure out first and last Y values on all lines, makes checking & reporting problems easier later
	# Get the ASDF mode too
	# Note X values are still here, must skip over them and ask for 2nd entry
	fun <- function(x) {x[2]}
	firstY <- unlist(lapply(lineList, fun), use.names = FALSE)
	fun <- function(x) {x[length(x)]}
	lastY <- unlist(lapply(lineList, fun), use.names = TRUE)
	dMode <- names(lastY)
	dMode <- gsub("Line_[0-9]*\\.", "", dMode) # names were whacked during unlisting
	attributes(lastY) <- NULL
		
	# Run the Y value check
	for (i in 2:length(dMode)) { # i indexes both lineList and dMode

		modeCheck <- dMode[i-1]

		if (modeCheck == "DIF") { # Final value was based on a DIF code, run y value check
			ychk <- isTRUE(all.equal(firstY[i], lastY[i-1]))
			if (!ychk) { # Failed Y value error reporting
				message("\nAttempting to sum DIFs, but Y value check failed; nearby values:")
				if (i <= 5) rpt <- 2:6
				if (i >= 6) rpt <- (i-2):(i+2)
				if (i >= (length(firstY) - 2)) rpt <- (length(firstY) - 5):length(firstY)
				DF <- data.frame(LineNo = names(lineList)[rpt],
					FirstYonLine = firstY[rpt], LastYonPrevLine = lastY[rpt-1],
					Problem = ifelse(firstY[rpt] == lastY[rpt-1], "", "*"))
				print(DF)
				stop("Y value check failed")
			} # End of failed Y value reporting
			
			# Remove the extra Y value that was included for the Y value check
			# DO NOT remove it from the start of the string, it may be the checkpoint value
			# DO remove it from the end of the i - 1 string,
			# See section 5.8.3 of the 1988 publication
			
			if (debug == 6) cat("\nyValueCheck is removing the last value,", lineList[[i-1]][length(lineList[[i-1]])],
				"from", names(lineList[i-1]), "\n")
			if (i != length(dMode)) lineList[[i-1]] <- lineList[[i-1]][-length(lineList[[i-1]])]
			
			if (i == length(dMode)) {
				# Note at the checkpoint, lastY and firstY are the same value
				# If we are on checkpoint line, remove it completely.
				if (debug == 6) cat("\nyValueCheck is removing the checkpoint line,", names(lineList[i]), "\n")
				lineList[[i]] <- NULL
				lineNames <- lineNames[-length(lineNames)]
			}
			
			
		} # end of if (modeCheck)
	
	} # end of y value check
	
	if (debug == 6) message("\nY value check successful...")
    
    names(lineList) <- lineNames
	lineList
	
	} # end of yValueCheck




