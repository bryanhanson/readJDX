#'
#' Import a File Written in the JCAMP-DX Format
#'
#' This function supervises the entire import process.
#' The JCAMP-DX standard allows many variations and it is difficult to anticipate all permutations.
#' Not all possible official formats are supported; error messages will
#' generally let you know what's going on.  If you have a file that you feel should be
#' supported but gives an error, please file an issue at Github and share the file.
#'
#' @param file Character.  The file name to import.
#'
#' @param SOFC Logical.  "Stop on Failed Check" The JCAMP-DX standard requires
#' several checks of the data as it is decompressed.  These checks are essential
#' to obtaining the correct results.  However, some JCAMP-DX writing programs
#' do not follow the standard to the letter (for instance we have observed that
#' not all writers put FIRSTY into the metadata, even though it is required by
#' the standard).  In other cases values in the file have low precision (see section on precision).
#' This option is provided for those \pkg{advanced
#' users} who have carefully checked their original files and want to skip the 
#' required checks.  It may also be useful for troubleshooting.  
#' The default is \code{TRUE} i.e. stop when something is not right.
#' This ensures that correct data is returned.  Change to \code{FALSE} at your own risk.
#' NOTE: Only a few checks can be skipped via this option, as there are some
#' parameters that must be available and correct in order to return any answer.
#'
#' @param debug Integer.  The level of debug reporting desired.  For those options giving
#'        a lot of output, you may wish to consider directing the output via \code{sink()}
#'        and then search the results for the problematic lines.
#' \itemize{
#'   \item 1 or higher = import progress is reported.
#'   \item 2 = details about the variable lists, compression formats and
#'             parameters that were found.
#'   \item 3 = print the extracted x values (huge output!).
#'   \item 4 = detailed info on the y value processing (huge output!).
#'   \item 5 = detailed info about processing the y values when DUP is in use (huge output!).
#'   \item 6 = detailed info about processing the y values when DIF is in use (huge output!).
#' }
#' In cases where an error is about to stop execution, you get additional information regardless of
#' the \code{debug} value.
#'
#' @return A list, as follows: 
#'
#' \itemize{
#'
#' \item The first element is a data frame summarizing the pieces of the imported file.
#'
#' \item The second element is the file metadata.
#'
#' \item The third element is a integer vector giving the comment lines found
#'       (exclusive of the metdata, which typically contains many comments).
#' }
#'
#' Additional elements contain the extracted data as follows:
#'
#' \itemize{
#'
#' \item If the file contains multiple spectra
#' (not currently supported), there will be one data frame for each spectrum.
#'
#' \item If the file contains the real and imaginary
#' parts of a 1D NMR spectrum, there will be two data frames, one containing the real portion
#' and the other the imaginary portion.
#' 
#' \item If the file contains one non-NMR spectrum,
#' a single data frame will be returned.
#'
#' \item In all cases above, the data frame has
#' elements \code{x} and \code{y}.
#'
#' \item In the case of 2D NMR data, additional list elements are returned including
#' the F2 frequency values, the F1 frequency values, and a matrix containing the 2D data.
#'
#' }
#'
#' @seealso Do \code{browseVignettes("readJCAMPDX")} for background information,
#' references and supported formats.
#'
#' @section Included Data Files:
#' The examples make use of data files included with the package. File \code{SBO.jdx}
#' is an IR spectrum of Smart Balance Original spread (a butter substitute). The
#' spectrum is presented in transmission format, and was recorded on a ThermoFisher
#' instrument.  The file uses AFFN compression, and was written
#' with the JCAMP-DX 5.01 standard. Note that even though the y-axis was recorded in 
#' percent transmission, in the JDX file it is stored on [0\ldots1].
#' File \code{PCRF.jdx} is a 1H NMR
#' spectrum of a hexane extract of a reduced fat potato chip.  The spectrum was
#' recorded on a JEOL instrument.  The file uses SQZ DIF compression, and was written
#' with the JCAMP-DX 6.00 standard.
#' File \code{PCRF_line265.jdx} has a deliberate error in it.  See the examples.
#' 
#' @section Precision:
#' Internally, this package uses a tolerance factor when comparing values during certain checks.
#' This is desirable because the original values in the files
#' are text strings of varying lengths which get converted to numerical values.  Occasionally
#' values in the file, such as FIRSTY, are stored with low precision, and the computation of the
#' value to be compared occurs with much greater precision.  In these cases the check can fail
#' even when the tolerance is pretty loose.  In these cases one might consider setting 
#' \code{SOFC = FALSE} to allow the calculation to proceed.  If you do this, be certain to check
#' the results carefully.
#' 
#' 
#' @section Performance:
#' \code{readJDX} is not particularly fast.  Priority has been given to assuring correct answers,
#' helpful debugging messages and understandable code.
#'
#' @export
#'
#' @importFrom stringr str_trim
#'
#' @examples
#' sbo <- system.file("extdata", "SBO.jdx", package = "readJDX")
#' chk <- readJDX(sbo)
#' plot(chk[[4]]$x, chk[[4]]$y/100, type = "l", main = "Original Smart Balance Spread",
#' 	xlab = "wavenumber", ylab = "Percent Transmission")
#' 
# pcrf <- system.file("extdata", "PCRF.jdx", package = "readJDX")
# chk <- readJDX(pcrf)
# plot(chk[[4]]$x, chk[[4]]$y, type = "l", main = "Reduced Fat Potato Chip Extract",
# 	xlab = "ppm", ylab = "Intensity")
# 
#' \dontrun{
#' # Line 265 has an N -> G typo.  Try with various levels of debug.
#' # Even with debug = 0 you get useful diagnostic info.
#' problem <- system.file("extdata", "PCRF_line265.jdx", package = "readJDX")
#' chk <- readJDX(problem)
#' }
#'
readJDX <- function (file = "", SOFC = TRUE, debug = 0){

	if (!requireNamespace("stringr", quietly = TRUE)) {
		stop("You need to install package stringr to use this function")
		}

	if (file == "") stop("No file specified")

	jdx <- readLines(file)


##### Step 1.  Check the overall file structure.

	# A data block consists of ##TITLE= up to ##END=
	# However, link blocks can be used to contain data blocks, in which
	# case one has a compound file.  I have never seen this in the wild.
	# Link blocks are not supported. NMR data sets, including 2D NMR data sets,
	# use a different scheme to hold multiple data sets.
	
	mode <- "IR_etc" # until proven otherwise; IR includes Raman, UV, pretty much anything other than NMR or 2D NMR
	
	blocks <- grep("^\\s*##TITLE\\s*=.*", jdx)
	nb <- length(blocks)
	if (nb == 0) stop("This does not appear to be a JCAMP-DX file")
	if (nb > 1) stop("Compound (multi-block / multi-spectra) data sets not supported")

	ntup <- grepl("^\\s*##NTUPLES", jdx)
	nD <- grepl("^\\s*##NTUPLES=\\s*nD", jdx)
	if (any(ntup) & !any(nD)) mode <- "NMR"
	if (any(ntup) & any(nD)) {
		mode <- "NMR2D"
		if (debug >= 1) message("\nreadJDX has been tested against a limited number of 2D NMR data sets.  We encourage you to file issues on Github, share problematic files and help us improve readJDX.")
	}
			
	if (debug >= 1) message("\n\nProcessing file ", file, " which appears to contain ", mode, " data")
			
##### Step 2. Locate the parameters and the variable list(s)
	
	dblist <- findDataTables(jdx, debug)
		
##### Step 3. Extract the needed parameters

	params <- extractParams(dblist[[2]], mode, SOFC, debug)
		
##### Step 4.  Process the variable list(s) into the final lists

	if ((mode == "IR_etc") | (mode == "NMR")) {
		# Return value is a list: dataGuide, metadata, comment lines + data frames of x, y
		# dataGuide, metadata & comments already in place; process each variable list

		for (i in 4:length(dblist)) {
			dblist[[i]] <- processDataTable(dblist[[i]], params, mode, dblist[[1]][i-2, c(2,3)], SOFC, debug)
		}

		# Fix up names
		if (mode == "IR_etc") {
			specnames <- jdx[blocks] # each line with title
			specnames <- str_trim(substring(specnames, 9, nchar(specnames)))		
		}

		if (mode == "NMR") specnames <- c("real", "imaginary")
	
		names(dblist) <- c("dataGuide", "metadata", "commentLines", specnames)
	}

		
	if (mode == "NMR2D") {
		# Return value is a list: dataGuide, metadata, comment lines, F2, F1, + a matrix w/2D data
		# dataGuide, metadata & comments already in place; add F2, F1, M and drop extra stuff
		M <- matrix(NA_real_, ncol = params[2], nrow = params[1]) # matrix to store result
		
		for (i in 4:length(dblist)) {
			tmp <- processDataTable(dblist[[i]], params, mode, dblist[[1]][i-2, c(2,3)], SOFC, debug)
			M[i-3,] <- tmp$y
		}
		# Update dblist
		dblist[[4]] <- seq(params[4], params[6], length.out = params[2]) # add F2
		dblist[[5]] <- seq(params[3], params[5], length.out = params[1]) # add F1
		dblist[[6]] <- M
		dblist <- dblist[1:6] # toss the other stuff
		names(dblist) <- c("dataGuide", "metadata", "commentLines", "F2", "F1", "Matrix")
	}
		
##### And we're done!

	if (debug >= 1) message("\nDone processing ", file)

	return(dblist)
	
} # end of readJDX
