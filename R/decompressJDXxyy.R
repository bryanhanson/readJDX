##'
##' Extract the values in JCAMP-DX file with an XYY data table.
##'
##' This function is NOT EXPORTED.
##' Users would not normally call this function.  See \code{\link{readJDX}}.
##' Documentation is provided for developers wishing to contribute to the package.
##' 
##' @param dt Character.  The data table to be processed.  Includes one pre-pended
##' line giving the type of data (e.g. XYY, XRR, XII).
##'
##' @param params Numeric. Vector of parameters extracted from file header.
##'
##' @param params lineNos. A vector containing the original line numbers of this
##'        data table in the original file.  Used for debugging responses.
##'
##' @param mode Character. One of c("IR", "NMR", "NMR2D")
##'
##' @param debug Integer.  See \code{\link{readJDX}} for details.
##'
##' @param SOFC Logical.  See \code{\link{readJDX}} for details.
##'
##' @return A data frame with elements \code{x} and \code{y}.
##' 
##' @importFrom stringr str_locate str_trim
##'
##' @noRd

decompressJDXxyy <- function (dt, params, mode, lineNos, SOFC, debug = 0) {
	
	
	# For XYY, each line of the data table begins with a frequency (x value)
	# followed by the y values in various compressed formats.
		
	# Note that xString and yString are pieces corresponding to the individual lines
	# of dt, each is of type character until fully decompressed.
	
	type <- dt[1]
	dt <- dt[-1]
		
	if (length(dt) != length(lineNos)) stop("lineNos doesn't match data table")
	
	if (type == "XRR") {if (debug >= 1) message("\nProcessing real data...")}
	if (type == "XII") {if (debug >= 1) message("\nProcessing imaginary data...")}
	if (type == "XYY") {if (debug >= 1) message("\nProcessing data table...")}
	if (type == "F2") {
		if (debug >= 1) {
			message("\nProcessing F2 spectra...")
			message("\n", dt[1])
		}
		dt <- dt[-1] # Remove e.g. ##PAGE= F1= 4.7865152724775
		lineNos <- lineNos[-1] # Now just numbers remain to be processed
		}

	### Split each line of dt in an x part and y part
	
	numpat <- "[0-9]+[.,]?[0-9]*\\s*" # , needed for EU format (also need to pick up integers)
	xString <- yString <- rep(NA_character_, length(dt))
	for (i in 1:length(dt)) {
		if (grepl("^\\$\\$", dt[i])) next # skip over comment (probably not necessary here)
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
		message("Here come the raw x values, line by line from the file")
		for (i in 1:length(xValues)) {
			cat("\nParsing line", lineNos[i], "for x values\n")
			cat("\tx value (character):", tmp[i], "\n")
			cat("\tx value (numeric):", xValues[i], "\n")
			}
		}
	
	# Save the first and last xValues for checking in a bit
	firstXcheck <- xValues[1]
#	lastXcheck <- xValues[length(xValues)]
	lastXcheck <- xValues[length(xValues[!is.na(xValues)])] # Clunky work around for comments -> NA
	xtol <- 0.0001*diff(range(xValues, na.rm = TRUE)) # Comments lead to NAs
	
	# The standard requires that
	# each line in the data table be checked to make sure no lines were skipped or dupped.
		
	if (anyDuplicated(xValues)) stop("Data table appears to have duplicated lines")
	# Technically, this next line compares the row count, not the actual number of x and y values
	if (length(yString) != length(xValues)) stop("The number of x values and y values aren't the same")
	xValues <- NULL # safety mechanism; recomputed at end

	### Process the y values to numeric
	
	NUM <- FALSE # flag to indicate we now have a numeric vector "answer"

 	yString <- gsub(",", ".", yString) # Replace ',' with '.' for EU style files
	
	# Check to see if yString has any comments in it: $$
	# remove it and any following characters if found
	
	yString <- gsub("\\$\\$.*", "", yString)
	
	# And, be sure there are no NA's present (clunky; seems to be related to comments)
	
	yString[is.na(yString)] <- " " # Cannot just delete
	
	fmt <- getJDXcompression(yString, debug = debug) # Get the compression format
		
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
 		if ("DUP" %in% fmt) yString <- insertDUPs(yString, lineNos, debug = debug)

		# Now process SQZ	
 		if ("SQZ" %in% fmt) yString <- unSQZ(yString) # if pure SQZ this is sufficient
			
		# Finally, take care of any DIFs
		# Done last, since only now do we have a number at what was the beginning of the line
		
 		if ("DIF" %in% fmt) {
  			yValues <- deDIF(yString, lineNos, debug) #  Returns a numeric vector
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
				message("Problem: NA found at line no: ", lineNos[i], "!\n")
				print(ytmp)
				stop("Conversion to numeric introduced NA")
				}
				
			yValues <- c(yValues, ytmp)					
			}
				
		yValues <- yValues[-1]
		}
						
	} # end of !"AFFN"	
 	
 	ytol <- 0.0001*diff(range(yValues))

	### Check the integrity of the results
			
	if (mode == "IR") {
		
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
		
			if (!isTRUE(all.equal(yValues[1]*factorY, firstY, check.names = FALSE, tolerance = ytol))) {
				cat("First y value from data table:", yValues[1]*factorY, "\n")
				cat("First y value from metadata:", firstY, "\n")
				stop("Error parsing yValues")
				}			
			}
		
		# Check first and last xValues (saved earlier).  The standard is ambiguous about doing this,
		# but out of an abundance of caution we will do it.
		
		if (!isTRUE(all.equal(firstXcheck*factorX, firstX, check.names = FALSE, tolerance = xtol))) {
			cat("First x value from data table:", firstXcheck*factorX, "\n")
			cat("First x value from metadata:", firstX, "\n")
			stop("Error parsing xValues (firstX)")
			}			
		
		# Do a lastX check if DIF format (where there is a check point)
		
		if ("DIF" %in% fmt) {
			if (!isTRUE(all.equal(lastXcheck*factorX, lastX, check.names = FALSE, tolerance = xtol))) {
				cat("Last x value from data table:", lastXcheck*factorX, "\n")
				cat("Last x value from metadata:", lastX, "\n")
				stop("Error parsing xValues (lastX)")
				}
			}
				
		# Compute xValues based on params (see notes earlier); update yValues
		
		dx <- (lastX-firstX)/(npoints - 1)
		xValues <- seq(firstX, lastX, by = dx)
		yValues <- yValues*factorY
		
		} # end of mode = "IR"
		
	if (mode == "NMR") {
		
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
				
		# Check first and last xValues (saved earlier).  The standard is ambiguous about doing this,
		# but out of an abundance of caution we will do it.
		
		if (!isTRUE(all.equal(firstXcheck*factorX, firstX, check.names = FALSE, tolerance = xtol))) {
			cat("First x value from data table:", firstXcheck*factorX, "\n")
			cat("First x value from metadata:", firstX, "\n")
			stop("Error parsing xValues (firstX)")
			}			
		
		# Do a lastX check if DIF format (where there is a check point)
		
		if ("DIF" %in% fmt) {
			if (!isTRUE(all.equal(lastXcheck*factorX, lastX, check.names = FALSE, tolerance = xtol))) {
				cat("Last x value from data table:", lastXcheck*factorX, "\n")
				cat("Last x value from metadata:", lastX, "\n")
				stop("Error parsing xValues (lastX)")
				}
			}
				

		if (type == "XRR") { # Check yValues (real)
						
			if (!isTRUE(all.equal(yValues[1]*factorR, firstR, check.names = FALSE, tolerance = ytol))) {			
				cat("First real value from data table:", yValues[1]*factorR, "\n")
				cat("First real value from metadata:", firstR, "\n")
				stop("Error parsing real values")
				}
			
			if (!isTRUE(all.equal(yValues[length(yValues)]*factorR, lastR, check.names = FALSE, tolerance = ytol))) {		
				cat("Last real value from data table:", yValues[length(yValues)]*factorR, "\n")
				cat("Last real value from metadata:", lastR, "\n")
				stop("Error parsing real values")
				}
			
			yValues = yValues*factorR
			
			} # end of XRR

		if (type == "XII") { # Check yValues (imaginary)

			if (!isTRUE(all.equal(yValues[1]*factorI, firstI, check.names = FALSE, tolerance = ytol))) {		
				cat("First imaginary value from data table:", yValues[1]*factorI, "\n")
				cat("First imaginary value from metadata:", firstI, "\n")
				stop("Error parsing imaginary values")
				}
			
			if (!isTRUE(all.equal(yValues[length(yValues)]*factorI, lastI, check.names = FALSE, tolerance = ytol))) {		
				cat("Last imaginary value from data table:", yValues[length(yValues)]*factorI, "\n")
				cat("Last imaginary value from metadata:", lastI, "\n")
				stop("Error parsing imaginary values")
				}
			
			yValues = yValues*factorI
			
			} # end of XII

			
		# Compute xValues based on params (see notes earlier)
		
		dx <- (lastX-firstX)/(pointsX - 1)
		xValues <- seq(firstX, lastX, by = dx)

		} # end of mode = "NMR"
		
	if (mode == "NMR2D") {
		
		pointsF1 <- as.integer(params[1])
		pointsF2 <- as.integer(params[2])
		firstF1 <- params[3]
		firstF2 <- params[4]
		lastF1 <- params[5]
		lastF2 <- params[6]
		factorF1 <- params[7]
		factorF2 <- params[8]
		factorZ <- params[9]
		
		if (debug >= 1) cat("\nNo. F2 points from metadata =", pointsF2, "\n")
		if (debug >= 1) cat("Actual F2 points found  =", length(yValues), "\n")
		
		if (pointsF2 != length(yValues)) stop("Data points found != data points in metadata")
				
		# Check first and last xValues (saved earlier).  The standard is ambiguous about doing this,
		# but out of an abundance of caution we will do it.
		
		if (!isTRUE(all.equal(firstXcheck*factorF2, firstF2, check.names = FALSE, tolerance = xtol))) {
			cat("First F2 value from data table:", firstXcheck*factorF2, "\n")
			cat("First F2 value from metadata:", firstF2, "\n")
			stop("Error parsing xValues (firstF2)")
			}			
		
		# Do a lastX check if DIF format (where there is a check point)
		
		if ("DIF" %in% fmt) {
			if (!isTRUE(all.equal(lastXcheck*factorF2, lastF2, check.names = FALSE, tolerance = xtol))) {
				cat("Last F2 value from data table:", lastXcheck*factorF2, "\n")
				cat("Last F2 value from metadata:", lastF2, "\n")
				stop("Error parsing xValues (lastF2)")
				}
			}
				
		# There is a poorly-documented check of the first y value in the 2D NMR format.
		# For the time-being we will not do the check, as it only checks one value.
			
		yValues = yValues*factorZ
						
		# Compute xValues based on params (see notes earlier)
		
		dx <- (lastF2-firstF2)/(pointsF2 - 1)
		xValues <- seq(firstF2, lastF2, by = dx)

		} # end of mode = "NMR2D"
		
	### And we're done...
	
	xydata <-data.frame(x = xValues, y = yValues)
	return(xydata)	
	}
