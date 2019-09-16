#'
#' Process DIF Encoded Strings
#'
#' This function is NOT EXPORTED.
#' Users would not normally call this function.  See \code{\link{readJDX}}.
#' Documentation is provided for developers wishing to contribute to the package.
#'
#' @param string Character.  String to be processed.  Named with line numbers.
#'
#' @param debug Integer.  See \code{\link{readJDX}} for details.
#'
#' @return A numeric vector.
#' 
#' @noRd
#'
deDIF <- function(string, debug) {
	
	# The JCAMP std states that in DIF form the first entry on a line
	# is an X value (already removed), the 2nd value is in SQZ form
	# and the subsequent values are differences between *adjacent* values:
	# X, Y1, D1, D2, D3, D4...
	# corresponding, after conversion, to:
	# X, Y1, Y2, Y3, Y4, Y5...
	# where D3 is the offset from D2 for example. So to construct
	# Y4 you must do Y1 + D1 + D2 + D3
	# However, DIF mode can be mixed with other modes, so
	# DUPs should be handled before arriving at this function,
	# and SQZ values should have been converted to numbers already.
	# This is the last step, only the DIF'ing needs to be done.
	
	# As received, the string is composed of numeric values and DIF characters
	
	# Step 1: Convert to a list which keeps the original lines intact but
	# allows us to split the lines
		
	string <- as.list(string)
	FUN <- function(x) {unlist(strsplit(x, "\\s+"))}
	string <- lapply(string, FUN)
		
	# Step 2: Convert the DIF characters to the corresponding numbers and fix offset

	if (debug == 7) tmp_string <- string # save copy for reporting
	
	res <- lapply(string, unDIF) # string is a named list coming in; res is a named list of data frames, one for each line
		
	# Separate yValues and DIFmode entries into separate lists
	yValues <- vector("list", length(res))
	names(yValues) <- names(res)
	dMode <- vector("list", length(res))
	
	for (i in 1:length(res)) {
		yValues[[i]] <- res[[i]]$values
		dMode[[i]] <- res[[i]]$DIFmode		
	}
				
	if (debug == 7) {
		message("\nUndoing DIF compression:")
		message("\nLines passed to deDIF:")
		print(tmp_string)
		message("\nLines as processed by deDIF:")
		print(yValues)
		message("\nCarrying out y value check...")
	}
		
	# Step 3: Carry out the y value check required by the standard
	# First value on a line should = last value on the prev. line, IFF
	# the last value was in DIF mode (per careful reading of the standard,
	# as pointed out by Daniel Jacob in Github issue #6)
	# Names must be stripped for the all.equal check below.

	# Figure out first and last values on all lines, makes checking & reporting problems easier later
	fun <- function(x) {x[1]}
	first <- unlist(lapply(yValues, fun), use.names = FALSE)
	fun <- function(x) {x[length(x)]}
	last <- unlist(lapply(yValues, fun), use.names = FALSE)
	
	# Run the y value check
	for (i in 2:length(dMode)) { # i indexes both yValues and dMode
		checkvec <- dMode[[i - 1]]
		modeCheck <- checkvec[length(checkvec)]
		
		if (modeCheck) { # Final value was based on a DIF code, run y value check
			ychk <- isTRUE(all.equal(first[i], last[i-1]))
			# print("Found a final DIF entry")
			if (!ychk) { # Failed y value error reporting
				message("\nAttempting to compute differences, but Y value check failed; nearby values:")
				if (i <= 5) rpt <- 2:6
				if (i >= 6) rpt <- (i-2):(i+2)
				if (i >= (length(first) - 2)) rpt <- (length(first) - 5):length(first)
				DF <- data.frame(LineNo = names(string)[rpt],
					FirstYonLine = first[rpt], LastYonPrevLine = last[rpt-1],
					Problem = ifelse(first[rpt] == last[rpt-1], "", "*"))
				print(DF)
				message("\nUse debug = 7 to see every line before and after conversion.\nYou may wish to capture the output with sink()\nand search for specific line numbers.")
				stop("Y value check failed")
			} # End of failed y value reporting
			
		# Remove the extra y value that was included for the y value check
		# Remove it from the start of string i, not the end of string i-1
		yValues[[i]] <- yValues[[i]][-1]
		} # end of if (modeCheck)
		
	} # end of y value check
	
	if (debug == 7) message("\ny value check successful...")
    
	# Step 4: Wrap up and return

	# Note: comments are still NA
	return(yValues)		
	
	} # end of deDIF




