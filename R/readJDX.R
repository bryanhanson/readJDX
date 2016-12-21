##'
##' Import a File Written in the JCAMP-DX Format
##'
##' This is the function users should call, and supervises the entire import process.
##' The JCAMP-DX standard allows quite a bit of lattitude and there are many
##' possible formats. Not all possible formats are supported; error messages will
##' generally let you know what's going on.  If you have a file that you feel should be
##' supported but gives an error, please file an issue at Github.
##' The standard allows many
##' variations and its impossible to test them all.
##'
##' @param file Character.  The file name to import.
##'
##' @param SOFC Logical.  "Stop on Failed Check" The JCAMP-DX standard requires
##' several checks of the data as it is decompressed.  These checks are essential
##' to obtaining the correct results.  However, some JCAMP-DX writing programs
##' do not follow the standard to the letter (for instance we have observed that
##' not all writers put FIRSTY into the metadata, even though it is required by
##' the standard).
##' This option is provided for those \pkg{advanced
##' users} who have carefully checked their original files and want to skip the 
##' required checks.  It may also be useful for troubleshooting.  
##' The default is \code{TRUE} i.e. stop when something is not right.
##' This ensures that correct data is returned.  Change to \code{FALSE} at your own risk.
##' NOTE: Only a few checks can be skipped via this option, as there are some
##' parameters that must be available in order to return an answer.
##'
##' @param debug Integer.  The level of debug reporting desired. 1
##' or higher = basic info about
##' each file, and import progress.  2 = detailed info about x values.
##' 3 = detailed info about y values. 4 = details about the DUP expansion
##' process. 5 = details about the calculation of differences when DIF
##' is in use.
##' In cases where an error is about to
##' stop execution, you get additional information regardless of
##' the \code{debug} value.
##'
##' @return A list.  The first element is the file metadata.  Additional elements
##' contain the extracted \code{x,y} data in one or more data frames as follows.
##' If the file contains multiple spectra
##' (not currently supported), there will be one data frame for each spectrum.
##' If the file contains the real and imaginary
##' parts of a 1D NMR spectrum, there will be two data frames, one containing the real portion
##' and the other the imaginary portion.  If the file contains one non-NMR spectrum,
##' a single data frame will be returned.  In all cases the data frame has
##' elements \code{x} and \code{y}.
##'
##' @seealso Do \code{browseVignettes("readJCAMPDX")} for background information,
##' references and supported formats.
##'
##' @section Included Data Files:
##' The examples make use of data files included with the package. File \code{SBO.jdx}
##' is an IR spectrum of Smart Balance Original spread (a butter substitute). The
##' spectrum is presented in transmission format, and was recorded on a ThermoFisher
##' instrument.  The file uses AFFN compression, and was written
##' with the JCAMP-DX 5.01 standard. Note that even though the y-axis is in 
##' percent transmission, in the JDX file it is stored on [0\ldots1].
##' File \code{PCRF.jdx} is a 1H NMR
##' spectrum of a hexane extract of a reduced fat potato chip.  The spectrum was
##' recorded on a JEOL instrument.  The file uses SQZ DIF compression, and was written
##' with the JCAMP-DX 6.00 standard.
##' File \code{PCRF_line265.jdx} has a deliberate error in it.  See the examples.
##' 
##' @section Precision:
##' Internally, this package uses a tolerance factor when comparing values during certain checks.
##' This is currently hardwired to \code{0.0001*diff(range(values))}.  This value works fine
##' in the test files.
##' This appears to be necessary because the original values in the files
##' are text strings of varying lengths which get converted to numerical values.  Some precision
##' may be lost but it appears trivial with the current settings.
##' 
##' @export
##'
##' @importFrom stringr str_trim
##'
##' @examples
##' sbo <- system.file("extdata", "SBO.jdx", package = "readJDX")
##' chk <- readJDX(sbo)
##' plot(chk[[2]]$x, chk[[2]]$y/100, type = "l", main = "Original Smart Balance Spread",
##' 	xlab = "wavenumber", ylab = "Percent Transmission")
##' 
##' pcrf <- system.file("extdata", "PCRF.jdx", package = "readJDX")
##' chk <- readJDX(pcrf)
##' plot(chk[[2]]$x, chk[[2]]$y, type = "l", main = "Reduced Fat Potato Chip Extract",
##' 	xlab = "ppm", ylab = "Intensity")
##' 
##' \dontrun{
##' # Line 265 has an N -> G typo.  Try with various levels of debug.
##' # Even with debug = 0 you get useful diagnostic info.
##' problem <- system.file("extdata", "PCRF_line265.jdx", package = "readJDX")
##' chk <- readJDX(problem)
##' }
##'
readJDX <- function (file = "", SOFC = TRUE, debug = 0){

	if (!requireNamespace("stringr", quietly = TRUE)) {
		stop("You need to install package stringr to use this function")
		}

	if (file == "") stop("No file specified")

	jdx <- readLines(file)


##### Step 1.  Check the overall file structure.

	# A data block consists of ##TITLE= up to ##END=
	# However, link blocks can be used to contain data blocks, in which
	# case one has a compound file (not supported).
	
	# Consider searching for something more robust.
	
	cmpd <- FALSE
	NMR <- FALSE
	blocks <- grep("^\\s*##TITLE\\s*=.*", jdx)
	nb <- length(blocks)
	if (nb == 0) stop("This does not appear to be a JCAMP-DX file")
	if (nb > 1) {
		cmpd <- TRUE
		if (cmpd) stop("Compound (multi-block / multi-spectra) data sets not supported")
		}

	ntup <- grepl("^\\s*##NTUPLES", jdx)
	if (any(ntup)) {
		NMR <- TRUE
		nb <- 2 # real & imaginary data
		}
		
	if (debug >= 1) message("\n\nProcessing file ", file, "\n")
			
##### Step 2. Locate the parameters and the data table
##### Store each separately as a list element

	dblist <- findDataTables(jdx, file, debug)
	nlmd <- lengths(dblist) # save for other functions: will allow debug reporting by original line no.
	
	# Get the string that comes after title, and use that as the name
	# in the returned list.
	# This code could be moved to findDataTables at some point.
	
	if (!NMR) {
		specnames <- jdx[blocks] # each line with title
		specnames <- str_trim(substring(specnames, 9, nchar(specnames)))
		}
		
	if (NMR) specnames <- c("real", "imaginary")
	
	specnames <- c("metadata", specnames)
	if (length(specnames) != nb+1) stop("Something went setting up the data list")
	names(dblist) <- specnames
	
	# Remove comment-only lines from the data tables (these mess up processing later)
	# They also pose challenges for reporting errors by original line no.
	
	toss <- 0L
	for (i in 2:length(dblist)) {
		tmp <- grep("^\\$\\$", dblist[[i]])
		toss <- c(toss, length(tmp))
		if (length(tmp) != 0) dblist[[i]] <- dblist[[i]][-tmp]
		}
		
##### Step 3. Extract the needed parameters

	params <- extractParams(dblist[[1]], NMR, SOFC, debug)
	
##### Step 4.  Process the data table(s)

	for (i in 2:length(dblist)) dblist[[i]] <- {
		processDataTable(dblist[[i]], params, debug, sum(nlmd[1:(i-1)])+toss[i], SOFC)
		}
		
##### And we're done!

	if (debug >= 1) message("\n\nDone processing ", file, "\n")

	return(dblist)
	
} # end of readJDX
