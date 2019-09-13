#'
#' Decompress a Single Variable List from a JCAMP-DX file
#'
#' This function is NOT EXPORTED.
#' Users would not normally call this function.  See \code{\link{readJDX}}.
#' Documentation is provided for developers wishing to contribute to the package.
#' 
#' @param dt Character.  A vector of character strings which contains
#' the variable list.  First line should be the format code (an
#' extra line inserted by this package).  Depending upon mode, there
#' may be some other stuff that still needs to be stripped off to get to just numbers.
#'
#' @param params Numeric. Vector of parameters from file header.
#'
#' @param params lineNos. Two integers giving the first and last lines
#'        of the variable list in the original file.  Used for debugging responses.
#'
#' @param mode Character. One of c("IR_etc", "NMR", "NMR2D")
#'
#' @param debug Integer.  See \code{\link{readJDX}} for details.
#'
#' @param SOFC Logical.  See \code{\link{readJDX}} for details.
#'
#' @return A data frame with elements $x$ and $y$, unless the input is 2D NMR, in which case a matrix.
#' 
#' @noRd
#'
processDataTable <- function (dt, params, mode, lineNos, SOFC, debug = 0){

	# Strip off non-numerical lines at beginning and end,
	# and adjust lineNos accordingly.
	# After this, dt should consist only of numerical values or their surrogates,
	# but it is possible there are still some comments embedded within dt.
	# These will be handled during decompression.
	
	lineNos <- unlist(lineNos)
	fmt <- dt[1]
	
	if (mode == "IR_etc") {
		dt <- dt[-c(2, length(dt))]
		st <- lineNos[1] + 1
		end <- lineNos[2] - 1
		lineNos <- c(NA_integer_, st:end)
		if (debug == 2) {
			message("\nHead of the variable list:")
			print(data.frame(lineNo = lineNos[1:10], first30characters = substring(dt[1:10], 1, 30)))
		}
	}
	
	if (mode == "NMR") {
		
		if (fmt == "XRR") {
			dt <- dt[-c(2, 3)]
			st <- lineNos[1] + 2
			end <- lineNos[2]
			lineNos <- c(NA_integer_, st:end)
			if (debug == 2) {
				message("\nHead of the variable list:")
				print(data.frame(lineNo = lineNos[1:10], first30characters = substring(dt[1:10], 1, 30)))
			}
		}
		
		if (fmt == "XII") {
			dt <- dt[-c(2, 3, length(dt))]
			st <- lineNos[1] + 2
			end <- lineNos[2] - 1
			lineNos <- c(NA_integer_, st:end)
			if (debug == 2) {
				message("\nHead of the variable list:")
				print(data.frame(lineNo = lineNos[1:10], first30characters = substring(dt[1:10], 1, 30)))
			}
		}
	}
	
	if (mode == "NMR2D") {
		# Keep line 2 of dt for debug reporting during decompression (e.g. ##PAGE= F1= 4.7865152724775)
		dt <- dt[-c(3, 4)]
		st <- lineNos[1]
		end <- lineNos[2]
		lineNos <- c(NA_integer_, st, (st + 3L):end)
		if (debug == 2) {
			message("\nHead of the variable list:")
			print(data.frame(lineNo = lineNos[1:10], first30characters = substring(dt[1:10], 1, 30)))
		}
	}
	
	lineNos <- paste("Line", lineNos, sep = "_")
	names(dt) <- lineNos
	
	# Dispatch based on fmt -- At this point only XYY is understood

	if ((fmt == "XRR") | (fmt == "XII") | (fmt == "NMR_2D")) fmt <- "XYY"
	
	if (fmt == "XYY") {
		xydata <- decompressXYY(dt, params, mode, SOFC, debug = debug)
		return(xydata)
	}
				
	stop("Did not find a valid format")	# Should never reach this line...
	}
