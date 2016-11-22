##'
##' Extract the values in JCAMP-DX file with an XYY data table.
##'
##' This function is NOT EXPORTED.
##' Users would not normally call this function.  See \code{\link{readJDX}}.
##' Documentation is provided for developers wishing to contribute to the package.
##' 
##' @param dt Character.  The data table to be processed.  Includes pre-pended
##' code giving the type of data (e.g. XYY, XRR, XII).
##'
##' @param params Numeric. Vector of parameters extracted from file header.
##' NMR parameters are not the same as non-NMR parameters.
##'
##' @param debug Integer.  See \code{\link{readJDX}} for details.
##'
##' @param nlmd Integer.  The number of lines of meta data.  Used in debug reporting.
##'
##' @param SOFC Logical.  See \code{\link{readJDX}} for details.
##'
##' @return A data frame with elements \code{x} and \code{y}.
##' 
##' @importFrom stringr str_locate str_trim
##'
##' @noRd

decompressJDXxyy <- function (dt, params, debug = 0, nlmd, SOFC) {
		
	# For XYY, each line of the data table begins with a frequency
	# followed by the y values in various compressed formats.
		
	# Note that xString and yString are pieces corresponding to the individual lines
	# of dt, each is of type character until fully decompressed.
	
	type <- dt[1]
	dt <- dt[-1]
	
	if (type == "XRR") {if (debug >= 1) message("\nProcessing real data...")}
	if (type == "XII") {if (debug >= 1) message("\nProcessing imaginary data...")}
	if (type == "XYY") {if (debug >= 1) message("\nProcessing data table...")}

	### Split each line of dt in an x part and y part
	
	numpat <- "[0-9]+[.,]?[0-9]*\\s*" # , needed for EU format (also need to pick up integers)
	xString <- yString <- rep(NA_character_, length(dt))
	for (i in 1:length(dt)) {
		pos <- str_locate(dt[i], numpat)[1,2]
		xString[i] <- substring(dt[i], 1, pos)
		yString[i] <- substring(dt[i], pos + 1, nchar(dt[i]))
		}
		
	#### Process the x values to numeric

	# The x values in the data table appear in most cases to
	# be significantly rounded relative to FIRSTX.
	# It appears as though the intent of the standard is to construct the sequence of 
	# x values from FIRSTX, LASTX, and NPOINTS, not the actual values in the
	# in the data table. The values in the data table however must be
	# checked for integrity (but see later for a work-around).
	
	# Important: these xValues are only used to verify parsing is correct,
	# e.g. there were no characters caught and no NA generated.
	# They are not used to construct the actual x values, that is done at
	# the very end using the parameters.
	
	tmp <- xString # hold for debug reporting
	xString <- gsub(",", ".", xString) # replace ',' with '.' -- needed for EU style files
	xValues <- as.numeric(xString)
	
	if (debug == 2) { # stop and report each line if requested (huge!)
		for (i in 1:length(xValues)) {
			cat("\nParsing line", i + nlmd, "for x values\n")
			cat("\tx value (character):", tmp[i], "\n")
			cat("\tx value (numeric):", xValues[i], "\n")
			}
		}
	
	# The standard requires that
	# each line in the data table be checked to make sure no lines were skipped or dupped.
		
	if (anyDuplicated(xValues)) stop("Data table appears to have duplicated lines")
	if (length(yString) != length(xValues)) stop("The number of x values and y values aren't the same")
	xValues <- NULL # safety mechanism; recomputed at end

	### Process the y values to numeric
	
	NUM <- FALSE # flag to indicate we now have a numeric vector "answer"

 	yString <- gsub(",", ".", yString) # Replace ',' with '.' for EU style files

	fmt <- getJDXcompression(yString, debug = debug) # Get the compression format
	
	# Check to see if yString has any comments in it: $$
	# remove it and any following characters if found
	
	yString <- gsub("\\$\\$.*", "", yString)
	
	# Now deal with the various compression options
	
	# Note AFFN is separated by any amount of white space so no special action needed,
	# can proceed immediately to conversion to numeric, exponents handled automatically,
	# and white space stripped off automatically.  It appears AFFN is never mixed
	# with other formats; the other formats are collectively called ASDF in the standard.
	
	if ("AFFN" %in% fmt) {
		yString <- paste(yString, collapse = " ") # turn into one long string
		yString <- strsplit(yString, split = " ")
		yString <- unlist(yString)
		yString <- str_trim(yString, side = "both")
		yValues <- na.omit(as.numeric(yString))
		NUM <- TRUE # done, control picks up at checking the results below
		}
		
	if (!"AFFN" %in% fmt) {
		# Break into pieces corresponding to individual numbers by inserting spaces
		
		# This PAC approach will not catch 123-j123 
		# Put space ahead of +|- signs (PAC)
		yString <- gsub("(\\+|-)([0-9]+)", " \\1\\2", yString)
		
		# Put space ahead of SQZ codes preceeded by a number
		yString <- gsub("([0-9]+)([@A-Ia-i]{1})", "\\1 \\2", yString)
		
		# Put a space between adjacent SQZ codes e.g. a215Hb513
		yString <- gsub("([@A-Ia-i]{1})([@A-Ia-i]{1})", "\\1 \\2", yString)
		
		# When a single SQZ code is immediately followed by a DIF code,
		# put a space between them, e.g. @j097795
		yString <- gsub("([@A-Ia-i]{1})([%J-Rj-r]{1})", "\\1 \\2", yString)
				                                                            
		# Put space ahead of DIF codes preceeded by a number
		yString <- gsub("([0-9]+)([%J-Rj-r]{1})", "\\1 \\2", yString)
		                                                            
		# Put space ahead and behind DIF codes followed by another DIF code
		# but, CRITICALLY, leave a DIF code followed by a number attached
		# to that number e.g. "1jj%j32rR15MNOP5" j32 and R15 and P5 
		# must stay together
		yString <- gsub("([%J-Rj-r]{1})(?=[%J-Rj-r]+)", " \\1 ", yString, perl = TRUE)

		# Separate DUP entries
		yString <- gsub("([S-Zs])", " \\1 ", yString)

 		# Check for DUP pseudo-digits and process if found.  This must be done first!
 		if ("DUP" %in% fmt) yString <- insertDUPs(yString, debug, nlmd)

		# Now process SQZ	
 		if ("SQZ" %in% fmt) yString <- unSQZ(yString) # if pure SQZ this is sufficient
			
		# Finally, take care of any DIFs
		# Done last, since only now do we have a number at what was the beginning of the line
		
 		if ("DIF" %in% fmt) {
  			yValues <- deDIF(yString, debug, nlmd) #  Returns a numeric vector
 			# deDIF has it's own error checking, problems there will stop there.
			NUM <- TRUE
			}

		# At this point, PAC needs no special handling, other formats have been handled
		# Convert to numeric, if !DIF
		
	if (!NUM) {

		# Do things one step at a time and combine at the end.
		# This will be slower as it is not vectorized, but allows for
		# line-by-line error reporting
		
		yValues <- NA_real_
		
		for (i in 1:length(yString)) {
			ytmp <- unlist(strsplit(yString[i], "\\s+"))
			if (ytmp[1] == "") ytmp <- ytmp[-1] # some PACs have an extra space
			
			ytmp <- as.numeric(ytmp)
						
			if (any(is.na(ytmp))) {
				message("Problem: NA found at line no: ", i + nlmd, "!\n")
				print(ytmp)
				stop("Conversion to numeric introduced NA")
				}
				
			yValues <- c(yValues, ytmp)					
			}
				
		yValues <- yValues[-1]
		}				
	} # end of !"AFFN"	
 	
	### Check the integrity of the results
		
	NMR <- FALSE
	if (length(params) == 12) NMR <- TRUE
	
	if (!NMR) {
		
		# Check that we got the right number of y values
		
		npoints <- as.integer(params[1])
		firstX <- params[2]
		lastX <- params[3]
		firstY <- params[4]
		factorX <- params[5]
		factorY <- params[6]
		if (debug >= 1) cat("\nNPOINTS =", npoints, "\n")
		if (debug >= 1) cat("Actual no. data points found  =", length(yValues), "\n")
		
		if (!npoints == length(yValues)) stop("NPOINTS and length of yValues don't match")

		# Check first y value
		
		if (!SOFC) warning("SOFC is FALSE, skipping FIRSTY check")
		
		if (SOFC) {
			tol <- 0.001*diff(range(yValues)) # apparently needed due to lots of
											  # character -> integer/numeric coercions
		
			if (!isTRUE(all.equal(yValues[1]*factorY, firstY, check.names = FALSE, tolerance = tol))) {
				cat("First y value from data table:", yValues[1]*factorY, "\n")
				cat("First y value from metadata:", firstY, "\n")
				stop("Error parsing yValues")
				}			
			}

		# Compute xValues based on params (see notes earlier); update yValues
		
		dx <- (lastX-firstX)/(npoints - 1)
		xValues <- seq(firstX, lastX, by = dx)
		
		xValues <- xValues*factorX
		yValues <- yValues*factorY
		
		} # end of !NMR
		
	if (NMR) {
		
		pointsX <- as.integer(params[1])
		pointsR <- as.integer(params[2])
		pointsI <- as.integer(params[3])
		firstX <- params[4]
		firstR <- params[5]
		firstI <- params[6]
		lastX <- params[7]
		lastR <- params[8]
		lastI <- params[9]
		factorX <- params[10]
		factorR <- params[11]
		factorI <- params[12]
		
		if (debug >= 1) cat("\nNo. data points from metadata =", pointsX, "\n")
		if (debug >= 1) cat("Actual no. data points found  =", length(yValues), "\n")
		
		if (pointsX != length(yValues)) stop("Data points found != data points in metadata")

		tol <- 0.001*diff(range(yValues)) # apparently needed due to lots of character -> integer/numeric coercions
		
		if (type == "XRR") {
						
			if (!isTRUE(all.equal(yValues[1]*factorR, firstR, check.names = FALSE, tolerance = tol))) {					
				cat("First real value from data table:", yValues[1]*factorR, "\n")
				cat("First real value from metadata:", firstR, "\n")
				stop("Error parsing real values")
				}
			
			if (!isTRUE(all.equal(yValues[length(yValues)]*factorR, lastR, check.names = FALSE, tolerance = tol))) {		
				cat("Last real value from data table:", yValues[length(yValues)]*factorR, "\n")
				cat("Last real value from metadata:", lastR, "\n")
				stop("Error parsing real values")
				}
			
			yValues = yValues*factorR
			
			} # end of XRR

		if (type == "XII") {

			if (!isTRUE(all.equal(yValues[1]*factorI, firstI, check.names = FALSE, tolerance = tol))) {		
				cat("First imaginary value from data table:", yValues[1]*factorI, "\n")
				cat("First imaginary value from metadata:", firstI, "\n")
				stop("Error parsing imaginary values")
				}
			
			if (!isTRUE(all.equal(yValues[length(yValues)]*factorI, lastI, check.names = FALSE, tolerance = tol))) {		
				cat("Last imaginary value from data table:", yValues[length(yValues)]*factorI, "\n")
				cat("Last imaginary value from metadata:", lastI, "\n")
				stop("Error parsing imaginary values")
				}
			
			yValues = yValues*factorI
			
			} # end of XII

			
		# Compute xValues based on params (see notes earlier)
		
		dx <- (lastX-firstX)/(pointsX - 1)
		xValues <- seq(firstX, lastX, by = dx)
		xValues = xValues*factorX

		} # end of NMR
		

	### And we're done...
	
	xydata <-data.frame(x = xValues, y = yValues)
	return(xydata)	
	}
