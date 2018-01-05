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
##' @return A list.  First element is a data frame giving information about the structure of the file.
##'         Columns will be Format, FirstLine, LastLine.  Next the metadata. Then each chunk
##'         of data that was found, with the format pre-pended.
##'
##' @importFrom stats na.omit
##'
##' @noRd
##'
findDataTables <- function (jdx, file, debug = 0){

	# A data table is defined by a variable list.
	# The following is structured to make it easy to add other options.
	
	# Add other variable list patterns here
	# We have to search for things that are sufficiently unique
	VL_pats <- c("^\\s*##XYDATA\\s*=\\s*\\(X\\+\\+\\(Y\\.\\.Y\\)\\)$", # IR, typically
		"^\\s*##PAGE\\s*=\\s*N=1", # real NMR data
		"^\\s*##PAGE\\s*=\\s*N=2", # imaginary NMR data
		"^\\s*##PAGE\\s*=\\s*F1=") # 2D NMR data (real) JEOL style

	# Add other variable list format short names here
	VL_fmts <- c("XYY", "XRR", "XII", "F2")
	
	# Add other END patterns here (each associated with a specific VL_fmts entry)
	END_pats <- c("^\\s*##END\\s*=",
		"^\\s*##PAGE\\s*=\\s*N=2",
		"^\\s*##END\\s{1}NTUPLES\\s*=",
		"\\$\\$\\s{1}checkpoint")
	
	# Find the beginning & end of each individual data set
	# We are checking for any and all formats in the file
	# We will capture some meta-information for completeness,
	# and drop it in a later step
	
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
	
	# Check & clean up initial results
	
	spec_st <- spec_st[-1] # remove NAs
	spec_end <- spec_end[-1]
	fmt <- fmt[-1]
	
	if (length(spec_st) == 0) { # Check to see if we actually found any data tables
		fmts <- paste(VL_fmts, collapse = ", ")
		msg <- paste("Couldn't find any data tables.  Supported formats are:", fmts, sep = " ")
		stop(msg)
		}

	metadata <- jdx[1:(spec_st[1] - 1)]
	
	Format <- c("metadata", fmt)
	FirstLine <- c(1, spec_st)
	LastLine = c(spec_st[1]-1, spec_end)
	
	DF <- data.frame(Format, FirstLine, LastLine, stringsAsFactors = FALSE)

	# Up to this point, processing has been generic & spec_st, spec_end reflect grep'ing of patterns
	# Now we need to tweak things depending upon the format
	
	for (i in 1:nrow(DF)) {
				
		if (DF$Format[i] == "XRR") {
			DF$LastLine[i] <- DF$LastLine[i] - 1
		}

	}

	if (debug >= 1) {
		message("Apparent data chunks:")
		print(DF)
		cat("\n")
		}
		
	dtlist <- vector("list", nrow(DF) + 1)
	dtlist[[1]] <- DF # dataGuide
	dtlist[[2]] <- metadata
	
	for (i in 3:length(dtlist)) {
		dtlist[[i]] <- c(DF$Format[i-1], jdx[DF$FirstLine[i-1]:DF$LastLine[i-1]])
	}

	return(dtlist)
	}
