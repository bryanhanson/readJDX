#'
#' Import a File Written in the JCAMP-DX Format
#'
#' This function supervises the entire import process.
#' Not all official formats are supported, see the vignettes.
#' Prior to release, this package is checked against a very large number of files in the
#' author's collection.  However, the JCAMP-DX standard allows many variations and it is
#' difficult to anticipate all permutations.  Error messages will
#' generally let you know what's going on.  If you have a file that you feel should be
#' supported but gives an error, please file an issue at Github and share the file.
#'
#' @param file Character.  The file name to import.
#'
#' @param SOFC Logical.  "Stop on Failed Check"
#' The default is \code{TRUE} i.e. stop when something is not right.
#' This ensures that correct data is returned.  Change to \code{FALSE} at your own risk.
#' NOTE: Only certain checks can be skipped via this option, as there are some
#' parameters that must be available and correct in order to return any answer.
#' For instance, one must end up with the same number of X and Y values.
#' This option is provided for those \pkg{advanced
#' users} who have carefully checked their original files and want to skip the
#' required checks.  It may also be useful for troubleshooting.
#' The JCAMP-DX standard requires
#' several checks of the data as it is decompressed.  These checks are essential
#' to obtaining the correct results.  However, some JCAMP-DX writing programs
#' do not follow the standard to the letter.  For instance we have observed that
#' not all JCAMP-DX writers put FIRSTY into the metadata, even though it is required by
#' the standard.  In other cases values in the file have low precision (see section on precision).
#' Another example is we have observed files where the X values are the count of data points,
#' and FIRSTY is given in Hz.  Since the field strength and center of the sweep frequency is needed
#' to convert to ppm, and these are  not required in the standard, one cannot return an answer in
#' ppm or Hz automatically.
#' In cases like this, once can set \code{SOFC = FALSE} and then manually convert the X axis.
#'
#' @param debug Integer.  The level of debug reporting desired.  For those options giving
#'        a lot of output, you may wish to consider directing the output via \code{\link{sinkall}}
#'        and then search the results for the problematic lines.
#' \itemize{
#'   \item 1 or higher = import progress is reported.
#'   \item 2 = details about the variable lists, compression formats and
#'             parameters that were found.
#'   \item 3 = print the extracted X values (huge output!).
#'   \item 4 = detailed info on the Y value processing (huge output!).
#'   \item 5 = detailed info about processing the Y values when DUP is in use (huge output!).
#'   \item 6 = detailed info about processing the Y values when DIF is in use (huge output!).
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
#' \item If the file contains one non-NMR spectrum, or a processed NMR spectrum (i.e. only 
#'       the final real data), a single data frame.
#'
#' \item If the file contains the real and imaginary
#'       parts of a 1D NMR spectrum, there will be two data frames, one containing the real portion
#'       and the other the imaginary portion.
#'
#' \item In all cases above, the data frame has elements \code{x} and \code{y}.
#'
#' \item In the case of 2D NMR data, additional list elements are returned including
#'       the F2 frequency values, the F1 frequency values, and a matrix containing the 2D data.
#'
#' }
#'
#' @seealso Do \code{browseVignettes("readJDX")} for background information,
#' references, supported formats, and details about the roles of each function.
#' If you have a multiblock file (which contains multiple spectra), please see
#' \code{\link{splitMultiblockDX}} which is a function to break such files into
#' individual files which can then be processed with this function.
#'
#' @section Included Data Files:
#' The examples make use of data files included with the package. File \code{SBO.jdx}
#' is an IR spectrum of Smart Balance Original spread (a butter substitute). The
#' spectrum is presented in transmission format, and was recorded on a ThermoFisher
#' instrument.  The file uses AFFN compression, and was written
#' with the JCAMP-DX 5.01 standard. Note that even though the Y-axis was recorded in
#' percent transmission, in the JDX file it is stored on [0\ldots1].
#' File \code{PCRF.jdx} is a 1H NMR
#' spectrum of a hexane extract of a reduced fat potato chip.  The spectrum was
#' recorded on a JEOL instrument.  The file uses SQZ DIF DUP compression, and was written
#' with the JCAMP-DX 6.00 standard.
#' File \code{PCRF_line265.jdx} has a deliberate error in it.
#' File \code{isasspc1.jdx} is a 2D NMR file recorded on a JEOL GX 400 instrument.
#' The file is freely available at \url{http://www.jcamp-dx.org/}.
#' File \code{MiniDIFDUP.JDX} is a small demonstration file, used in the vignettes to
#' illustrate the decompression process.  It is derived from a freely available file.
#'
#' @section Precision:
#' Internally, this package uses a tolerance factor when comparing values during certain checks.
#' This is desirable because the original values in the files
#' are text strings of varying lengths which get converted to numerical values by \code{R}.  Occasionally
#' values in the file, such as FIRSTY, are stored with low precision, and the computation of the
#' value to be compared occurs with much greater precision.  In these cases the check can fail
#' even when the tolerance is pretty loose.  In these cases one might consider setting
#' \code{SOFC = FALSE} to allow the calculation to proceed.  If you do this, be certain to check
#' the results carefully as described under \code{SOFC}.
#'
#' @section Y Value Check:
#' The standard requires a "Y Value Check" when in DIF mode.  Extra Y values have been appended to each
#' line to use in the check, and the last Y value on a line must equal the first Y value on the next line
#' IFF one is in DIF mode.  After a successful check, the extra Y value must be removed.  In actual practice,
#' some vendors, at least some of the time, seem to differ as to the definition of
#' "being in DIF mode".  In turn, this determines how the Y value check should proceed.
#' \itemize{
#'   \item The standard says "When, and only when, the last ordinate of a line is in DIF form ...
#'         The first ordinate of the next line ... is always an actual value, equal to the last
#'         calculated ordinate of the previous line". See section 5.8.3 of the 1988 publication.
#'   \item Taking this definition literally, the Y value check (and removal of the extra value),
#'         should occur when one sees e.g. ... DIF DIF DIF (end of line). Let's call
#'         this "literal DIF".  A literal DIF is easy to detect and act on.
#'   \item In other cases, something like ... DIF DUP DUP (end of line) is considered to be in DIF mode
#'         for Y value check purposes. In these cases we have look backwards to see if we are in DIF mode.
#'         Let's call this "relayed DIF".
#'   \item However, some vendors may treat ... DIF DUP DUP (end of line) as not in DIF mode, and hence
#'         one should not do the Y value check and not remove any values, since this vendor would not have
#'         added an extra Y value.
#'   \item In addition to these three possibilities, \code{readJDX} through versions 0.3.xx used a different
#'         definition, namely if there were any DIF entries anywhere on the line, then DIF mode was
#'         assumed and the Y value check carried out.  This worked for many files, but not all.
#'   \item In the 0.4.xx series, \code{readJDX} detects both the literal and relayed definitions and
#'         tries to keep moving forward as much as possible.
#' }
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
#' # IR spectrum
#' sbo <- system.file("extdata", "SBO.jdx", package = "readJDX")
#' chk <- readJDX(sbo)
#' plot(chk[[4]]$x, chk[[4]]$y / 100,
#'   type = "l", main = "Original Smart Balance Spread",
#'   xlab = "wavenumber", ylab = "Percent Transmission"
#' )
#'
#' # 1H NMR spectrum
#' pcrf <- system.file("extdata", "PCRF.jdx", package = "readJDX")
#' chk <- readJDX(pcrf)
#' plot(chk[[4]]$x, chk[[4]]$y,
#'   type = "l", main = "Reduced Fat Potato Chip Extract",
#'   xlab = "ppm", ylab = "Intensity"
#' )
#'
#' # Capturing processing for troubleshooting
#' mdd <- system.file("extdata", "MiniDIFDUP.JDX", package = "readJDX")
#' tf <- tempfile(pattern = "Troubleshooting", fileext = "txt")
#' chk <- readJDX(mdd, debug = 6)
#' sinkall() # close the file connection
#' file.show(tf)
#'
#' # 2D HETCORR spectrum
#' \dontrun{
#' nmr2d <- system.file("extdata", "isasspc1.dx", package = "readJDX")
#' chk <- readJDX(nmr2d)
#' contour(chk$Matrix, drawlabels = FALSE) # default contours not optimal
#' }
#'
#' \dontrun{
#' # Line 265 has an N -> G error.  Try with various levels of debug.
#' # Even with debug = 0 you get useful diagnostic info.
#' problem <- system.file("extdata", "PCRF_line265.jdx", package = "readJDX")
#' chk <- readJDX(problem)
#' }
#'
readJDX <- function(file = "", SOFC = TRUE, debug = 0) {
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("You need to install package stringr to use this function")
  }

  if (file == "") stop("No file specified")

  jdx <- readLines(file)

  ##### Step 1.  Check the overall file structure.

  # A data block consists of ##TITLE= up to ##END=
  # However, link blocks can be used to contain data blocks, in which
  # case one has a compound file. Link blocks and compound files are not supported.
  # NMR data sets, including 2D NMR data sets, use a different scheme to hold multiple data sets.

  blocks <- grep("^\\s*##TITLE\\s*=.*", jdx)
  nb <- length(blocks)
  if (nb == 0) stop("This does not appear to be a JCAMP-DX file")
  if (nb > 1) stop("Compound (multi-block / multi-spectra) data sets not supported")

  ##### Step 2. Locate the parameters and the variable list(s)

  VL <- findVariableLists(jdx, debug)

  # fmt is a character vector extracted by findVariableLists.

  fmt <- VL[["DataGuide"]][, "Format"][-1]

  # "mode" is a length one string derived from fmt and is used to determine the type of
  # processing needed; we are not using "fmt" because we need a more user-friendly string

  mode <- NA_character_
  if ("XYY" %in% fmt) mode <- "XY_data"
  if ("XRR" %in% fmt) mode <- "NMR_1D"
  if ("NMR_2D" %in% fmt) mode <- "NMR_2D"
  if ("LC_MS" %in% fmt) <- mode <- "LC_MS"
  if ("PEAK_TABLE" %in% fmt) mode <- "PEAK_TABLE"
  if (is.na(mode)) stop("Could not determine the type of data in the file")

  if (debug >= 1) cat("\n\nProcessing file", file, "which appears to contain", mode, "data\n")

  ##### Step 3. Extract the needed parameters

  params <- extractParams(VL[[2]], mode, SOFC, debug)

  ##### Step 4.  Process the variable list(s) into the final lists

  if ((mode == "XY_data") | (mode == "NMR_1D")) {
    # Return value is a list: dataGuide, metadata, comment lines + data frames of x, y
    # dataGuide, metadata & comments already in place; process each variable list

    for (i in 4:length(VL)) {
      VL[[i]] <- processVariableList(VL[[i]], params, mode, VL[[1]][i - 2, c(2, 3)], SOFC, debug)
    }

    # Fix up names
    if (mode == "XY_data") {
      specnames <- jdx[blocks] # each line with title
      specnames <- str_trim(substring(specnames, 9, nchar(specnames)))
    }

    if (mode == "NMR_1D") specnames <- c("real", "imaginary")

    names(VL) <- c("dataGuide", "metadata", "commentLines", specnames)
  }


  if (mode == "NMR_2D") {
    # Return value is a list: dataGuide, metadata, comment lines, F2, F1, + a matrix w/2D data
    # dataGuide, metadata & comments already in place; add F2, F1, M and drop extra stuff
    M <- matrix(NA_real_, ncol = params[2], nrow = params[1]) # matrix to store result

    for (i in 4:length(VL)) {
      tmp <- processVariableList(VL[[i]], params, mode, VL[[1]][i - 2, c(2, 3)], SOFC, debug)
      M[i - 3, ] <- tmp$y
    }
    # Update VL
    VL[[4]] <- sort(seq(params[4], params[6], length.out = params[2])) # add F2
    VL[[5]] <- sort(seq(params[3], params[5], length.out = params[1])) # add F1
    M <- M[nrow(M):1, ] # reverse order of rows, works for Bruker files
    VL[[6]] <- M
    VL <- VL[1:6] # toss the other stuff
    names(VL) <- c("dataGuide", "metadata", "commentLines", "F2", "F1", "Matrix")
  }

  if (mode == "LC_MS") {
    # Return value is a list: dataGuide, metadata, comment lines, a data frame for each time point
    # dataGuide, metadata & comments already in place; add data frames
    for (i in 4:length(VL)) {
      tmp <- processVariableList(VL[[i]], params, mode, VL[[1]][i - 2, c(2, 3)], SOFC, debug)
      something
      name it
    }
    names(VL) <- c("dataGuide", "metadata", "commentLines", "F2", "F1", "Matrix")
  }

  if (mode == "PEAK_TABLE") {
    for (i in 4:length(VL)) {
      VL[[i]] <- processVariableList(VL[[i]], params, mode, VL[[1]][i - 2, c(2, 3)], SOFC, debug)
    }
    specnames <- jdx[blocks] # each line with title
    specnames <- str_trim(substring(specnames, 9, nchar(specnames)))
    names(VL) <- c("dataGuide", "metadata", "commentLines", specnames)
  }

  ##### And we're done!

  if (debug >= 1) cat("\nDone processing ", file, "\n")

  return(VL)
} # end of readJDX
