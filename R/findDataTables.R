##'
##' Locate a Data Table in a JCAMP-DX File
##'
##' This function is NOT EXPORTED.
##' Users would not normally call this function.  See \code{\link{readJDX}}.
##' Documentation is provided for developers wishing to contribute to the package.
##' 
##' @param jdx Character.  A vector of character strings which hopefully contains
##' the data table.  Each string is a line of the complete original file.
##'
##' @param file Character.  The name of the file being processed.  Only used for
##' debugging output.
##'
##' @param debug Integer.  See \code{\link{readJDX}} for details.
##'
##' @return A list of length 2 or longer.  The first element is the metadata.
##' Additional elements are the actual data table, with a format string pre-pended.
##'
##' @importFrom stats na.omit
##'
##' @noRd
##'
findDataTables <- function (jdx, file, debug = 0){

	# A data table is defined by a variable list.
	# The following is structured to make it easy to add other options.
	
	# Possibilities: Add other variable list patterns here
	VL_pats <- c("^\\s*##XYDATA\\s*=\\s*\\(X\\+\\+\\(Y\\.\\.Y\\)\\)$", # IR, typically
		"^\\s*##DATA\\s{1}TABLE\\s*=\\s*\\(X\\+\\+\\(R\\.\\.R\\))", # real NMR data # CHECK \\)
		"^\\s*##DATA\\s{1}TABLE\\s*=\\s*\\(X\\+\\+\\(I\\.\\.I\\))", # imaginary NMR data
		"^\\s*##DATA\\s{1}TABLE\\s*=\\s*\\(F2\\+\\+\\(Y\\.\\.Y\\)\\)") # 2D NMR data (real) JEOL style

	# Possibilities: Add other variable list format short names here
	VL_fmts <- c("XYY", "XRR", "XII", "F2")
	
	# Possibilities: Add other END patterns here (each associated with a specific VL_fmts entry)
	# Note: this assumes imaginary always follows real data
	END_pats <- c("^\\s*##END\\s*=",
		"^\\s*##PAGE\\s*=\\s*N=2",
		"^\\s*##END\\s{1}NTUPLES\\s*=",
		"\\$\\$\\s{1}checkpoint")
	
	# Find the beginning & end of each individual data set
	
	spec_st <- NA_integer_
	spec_end <- NA_integer_
	fmt <- NA_character_
	for (i in 1:length(VL_fmts)) {
		g1 <- grep(VL_pats[i], jdx)
		if (length(g1) != 0) {
			spec_st <- c(spec_st, g1)
			g2 <- grep(END_pats[i], jdx)
			if (length(g2) == 0) stop("Found the start of a data table, but not the end")
			spec_end <- c(spec_end, g2)
			fmt <- c(fmt, rep(VL_fmts[i], length(g1)))
			}
		}
	
	spec_st <- spec_st[-1]
	if (length(spec_st) == 0) {
		fmts <- paste(VL_fmts, collapse = ", ")
		msg <- paste("Couldn't find the data table start.  Supported formats are:", fmts, sep = " ")
		stop(msg)
		}
	spec_st <- spec_st + 1 # drop variable list format statement, leaving only numbers
	spec_end <- spec_end[-1]
	spec_end <- spec_end -1 # trim so only numbers remain
	fmt <- fmt[-1]
	
	metadata <- jdx[1:(spec_st[1] - 1)]

	DF <- data.frame(Format = c("metadata", fmt),
		FirstLine = c(1, spec_st),
		LastLine = c(length(metadata), spec_end),
		stringsAsFactors = FALSE)

	if (debug >= 1) {
		message("Apparent data chunks:")
		print(DF)
		cat("\n")
		}
	
	dtlist <- vector("list", nrow(DF))
	dtlist[[1]] <- metadata
	for (i in 2:length(dtlist)) {
		dtlist[[i]] <- c(DF$Format[i], jdx[DF$FirstLine[i]:DF$LastLine[i]])
		}
		
	return(dtlist)
	}
