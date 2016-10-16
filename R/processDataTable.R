##'
##' Decompress a Single Data Table from a JCAMP-DX file
##'
##' This function is NOT EXPORTED.
##' Users would not normally call this function.  See \code{\link{readJDX}}.
##' Documentation is provided for developers wishing to contribute to the package.
##' 
##' @param dt Character.  A vector of character strings which contains
##' the data table.  First line should be the format code (an
##' extra line inserted by this package).
##'
##' @param params Numeric. Vector of parameters from file header.
##'
##' @param debug Integer.  See \code{\link{readJDX}} for details.
##'
##' @param nlmd Integer.  The number of lines of meta data.  Used in debug reporting.
##'
##' @return A data frame with elements $x$ and $y$.
##' 
##' @noRd
processDataTable <- function (dt, params, debug = 0, nlmd){

	# This function is supervisory, does no real work
	
	fmt <- dt[1] # Get the format & dispatch based on it
	
	if ((fmt == "XRR") | (fmt == "XII")) fmt <- "XYY"
	
	if (fmt == "XYY") xydata <- decompressJDXxyy(dt, params, debug = debug, nlmd = nlmd)
	
	# Add other variable list formats here
		
	return(xydata)	
	}
