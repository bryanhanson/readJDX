##'
##' Decompress a Single Data Table from a JCAMP-DX file
##'
##' This function is NOT EXPORTED.
##' Users would not normally call this function.  See \code{\link{readJDX}}.
##' Documentation is provided for developers wishing to contribute to the package.
##' 
##' @param dt Character.  A vector of character strings which contains
##' the data table.  First line should be the format code (an
##' extra line inserted by this package).  Depending upon mode, there
##' may be some metadata that still needs to be stripped off.
##'
##' @param params Numeric. Vector of parameters from file header.
##'
##' @param params lineNos. Two integers giving the first and last lines
##'        of the data table in the original file.  Used for debugging responses.
##'
##' @param mode Character. One of c("IR", "NMR", "NMR2D")
##'
##' @param debug Integer.  See \code{\link{readJDX}} for details.
##'
##' @param SOFC Logical.  See \code{\link{readJDX}} for details.
##'
##' @return A data frame with elements $x$ and $y$.
##' 
##' @noRd
processDataTable <- function (dt, params, mode, lineNos, SOFC, debug = 0){

	# This function is mostly supervisory
	# Strip off non-numerical lines at beginning and end
	# Get lineNos ready to pass on
	
	lineNos <- unlist(lineNos)
	
	if (mode == "IR") {
		dt <- dt[-c(2, length(dt))]
		lineNos[1] <- lineNos[1] + 1
		lineNos[2] <- lineNos[2] - 1
		lineNos <- lineNos[1]:lineNos[2]
	}
	
	if (mode == "NMR") {
		
		if (dt[1] == "XRR") {
			dt <- dt[-c(2, 3)]
			lineNos[1] <- lineNos[1] + 2
			lineNos <- lineNos[1]:lineNos[2]
		}
		
		if (dt[1] == "XII") {
			dt <- dt[-c(2, 3, length(dt))]
			lineNos[1] <- lineNos[1] + 2
			lineNos[2] <- lineNos[2] - 1
			lineNos <- lineNos[1]:lineNos[2]
		}
	}
	
	if (mode == "NMR2D") {
		# Keep line 2 of dt for debug reporting later (e.g. ##PAGE= F1= 4.7865152724775)
		dt <- dt[-c(3, 4)]		
		lineNos[1] <- lineNos[1] + 2
		lineNos <- lineNos[1]:lineNos[2]
	}
	
	fmt <- dt[1] # Get the format & dispatch based on it

	if ((fmt == "XRR") | (fmt == "XII") | (fmt == "F2")) fmt <- "XYY"
	
	if (fmt == "XYY") {
		xydata <- decompressJDXxyy(dt, params, mode, lineNos, SOFC, debug = debug)
		return(xydata)
	}
				
	stop("Did not find a valid format")	# Should never reach this line...
	}
