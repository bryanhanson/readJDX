##'
##' Process DIF Encoded Strings
##'
##' This function is NOT EXPORTED.
##' Users would not normally call this function.  See \code{\link{readJDX}}.
##' Documentation is provided for developers wishing to contribute to the package.
##'
##' @param string Character.  String to be processed.
##'
##' @param debug Integer.  See \code{\link{readJDX}} for details.
##'
##' @return A numeric vector.
##' 
##' @noRd
##'
deDIF <- function(string, lineNos, debug) {
	
	# The JCAMP std states that in DIF form the first entry on a line
	# is an X value (already removed), the 2nd value is in SQZ form
	# and the subsequent values are differences between *adjacent* values:
	# X, Y1, D1, D2, D3, D4...
	# corresponding, after conversion, to:
	# X, Y1, Y2, Y3, Y4, Y5...
	# where D3 is the offset from D2 for example. So to construct
	# Y4 you must do Y1 + D1 + D2 + D3

	# Note: DUPs should be handled before arriving at this function,
	# and SQZ values should have been converted to numbers already.
	# This is the last step!
	
	# As received, the string is composed of numeric values and DIF characters
	
	# Step 1: Convert to a list which keeps the original lines intact but
	# allows us to split the lines
	
	
	string <- as.list(string)
	FUN <- function(x) {unlist(strsplit(x, "\\s+"))}
	string <- lapply(string, FUN)
	names(string) <- paste("Line", lineNos, sep = "_")
	
	if ((debug == 3) | (debug == 4)) message("Undoing DIF compression\n")
	
	if (debug == 5) {
		message("First 5 lines of data table (step 1 in deDIF):")
		print(string[1:5])
		}
	
	# Step 2: Convert the DIF characters to the corresponding numbers
	
	string <- lapply(string, unDIF)
	values <- lapply(string, as.numeric)

	if (debug == 5) {
		message("First 5 lines of data table (step 2 in deDIF):")
		print(values[1:5])
		}
			
	# Step 3: Fix the offsets
	
	yValues <- lapply(values, cumsum)
	
	if (debug == 5) {
		message("First 5 lines of data table (step 3 in deDIF):")
		print(yValues[1:5])
		}
		
	# Step 4: Carry out the y value check required by the standard
	# First value on a line should = last value on the prev. line.
	# Names must be stripped for the all.equal check below.

	if ((debug == 3) | (debug == 4)) message("Carrying out y value check")
	
	fun <- function(x) {x[1]}
	first <- unlist(lapply(yValues, fun), use.names = FALSE)
	fun <- function(x) {x[length(x)]}
	last <- unlist(lapply(yValues, fun), use.names = FALSE)
		
	for (i in 2:length(first)) {
		if (is.na(first[i])) next # These originate from comment only lines e.g. Bruker NMR
		ychk <- isTRUE(all.equal(first[i], last[i-1]))
		if (!ychk) {
			msg <- "\nY value check failed; nearby values:"
			message(msg)
			if (i <= 5) rpt <- 2:6
			if (i >= 6) rpt <- (i-2):(i+2)
			if (i >= (length(first) - 2)) rpt <- (length(first) - 5):length(first)
			DF <- data.frame(Line = lineNos[rpt],
				FirstYonLine = first[rpt], LastYonPrevLine = last[rpt-1],
				Problem = ifelse(first[rpt] == last[rpt-1], "", "*"))
			print(DF)
			message("\nCorresponding complete lines processed from original file:")
			print(yValues[rpt])
			stop("Y value check failed")
			}
		}
		
	# Step 5: Remove extra y values that were needed for the check
			
	verylast <- yValues[[length(yValues)]] # save to replace in a moment
	fun <- function(x) {x[-length(x)]}
	yValues <- lapply(yValues, fun) # removes all values in last element
	yValues[[length(yValues)]] <- verylast

	# Step 6: Wrap up and return

	# Note: comments are still NA and lineNos is still correct
	
	return(unlist(yValues))		
	
	} # end of deDIF




