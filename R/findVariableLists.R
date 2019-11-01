#'
#' Locate Variable Lists and Related Information in a JCAMP-DX File
#'
#' This function is NOT EXPORTED.
#' Users would not normally call this function.  See \code{\link{readJDX}}.
#' Documentation is provided for developers wishing to contribute to the package.
#'
#' @param jdx Character.  A vector of character strings which hopefully contains
#' one or more variable lists.  Each string is a line of the complete original file.
#'
#' @param debug Integer.  See \code{\link{readJDX}} for details.
#'
#' @return A list.
#' \itemize{
#'   \item A data frame giving information about the structure of the file. Columns will be
#'         Format, FirstLine, LastLine.  Serves as a Data Guide.
#'   \item The metadata
#'   \item The line numbers of comments (excluding comments in the metadata).
#'   \item Each variable list that was found, with the format pre-pended.
#'  }
#'
#' @importFrom stats na.omit
#'
#' @noRd
#'
findVariableLists <- function(jdx, debug = 0) {

  # A data set is defined by a variable list.
  # The following is structured to make it easy to add other options.

  # Add other variable list FORMAT short names here
  # These are used to tweak the lines selected
  VL_fmts <- c("XYY", "XRR", "XII", "NMR_2D")
  nf <- length(VL_fmts)

  # Add other variable list START patterns here (each associated with a specific VL_fmts entry)
  # Must search for things that are sufficiently unique
  ST_pats <- c(
    "^\\s*##XYDATA\\s*=\\s*\\(X\\+\\+\\(Y\\.\\.Y\\)\\)$", # IR, typically
    "^\\s*##PAGE\\s*=\\s*N=1", # real NMR data
    "^\\s*##PAGE\\s*=\\s*N=2", # imaginary NMR data
    "^\\s*##PAGE\\s*=\\s*F1=") # 2D NMR data (real part only)

  # Add other END patterns here (each associated with a specific VL_fmts entry)
  END_pats <- c(
    "^\\s*##END\\s*=", # IR, typically
    "^\\s*##PAGE\\s*=\\s*N=2", # real NMR data
    "^\\s*##END\\s{1}NTUPLES\\s*=", # imaginary NMR data
    "^\\s*##PAGE\\s*=\\s*F1=") # 2D NMR data (not the very last entry however; fixed later in this function)

  # Find the beginning & end of each variable list.
  # We are checking for any and all formats in the file
  # We will capture some meta-information for completeness,
  # and drop it in a later step.

  spec_st <- NA_integer_
  spec_end <- NA_integer_
  fmt <- NA_character_

  for (i in 1:nf) {
    gST <- grep(ST_pats[i], jdx)
    if (length(gST) == 0L) next
    gEND <- grep(END_pats[i], jdx)
    if (length(gEND) == 0L) next
    # the above ensures that if we don't find both strings we don't proceed
    spec_st <- c(spec_st, gST)
    spec_end <- c(spec_end, gEND)
    fmt <- c(fmt, rep(VL_fmts[i], length(gST)))
  }

  spec_st <- spec_st[-1] # remove NAs
  spec_end <- spec_end[-1]
  fmt <- fmt[-1]

  # Run some checks...

  if (length(spec_st) == 0L) { # Check to see if we actually found any variable lists
    fmts <- paste(VL_fmts, collapse = ", ")
    msg <- paste("Couldn't find any variable lists.  Supported formats are:", fmts, sep = " ")
    stop(msg)
  }

  if (length(spec_end) == 0L) stop("Found the start of a variable list, but not the end")

  # Organize the return values

  metadata <- jdx[1:(spec_st[1] - 1)]

  Format <- c("metadata", fmt)
  FirstLine <- c(1, spec_st)
  LastLine <- c(spec_st[1] - 1, spec_end)

  DF <- data.frame(Format, FirstLine, LastLine, stringsAsFactors = FALSE)

  # Find all comment only lines exclusive of metadata; these cause a variety of problems.
  # Keep original line numbers. CURRENTLY NOT USED OTHER THAN THIS FUNCTION

  comOnly <- grep("^\\$\\$", jdx)
  comOnly <- setdiff(comOnly, 1:(spec_st[1] - 1))

  # Check to see if this is 2D NMR data, if so find the vendor, as adjustments will be necessary

  TwoD <- FALSE
  vendor <- NULL

  if (length(unique(fmt)) == 1L) {
    if (unique(fmt) == "NMR_2D") TwoD <- TRUE
  }

  if (TwoD) {
    if (any(grepl("JEOL NMR", jdx))) vendor <- "JEOL"
    if (any(grepl("Bruker BioSpin GmbH", jdx))) vendor <- "Bruker"
    if (is.null(vendor)) warning("Looks like 2D NMR but could not identify vendor")
  }

  # Up to this point, processing has been generic & spec_st, spec_end reflect grep'ing of patterns.
  # Now we need to tweak things depending upon the format & vendor, to narrow the actual variable list
  # as close as possible.

  for (i in 1:nrow(DF)) {
    if (DF$Format[i] == "XRR") {
      DF$LastLine[i] <- DF$LastLine[i] - 1 # removes the ##PAGE= N=2 line
    }

    if (DF$Format[i] == "NMR_2D") {
      if (i != nrow(DF)) DF$LastLine[i] <- DF$FirstLine[i + 1] - 1 # removes the ##PAGE= N=2 line
      # Next line removes
      # ##END NTUPLES=
      # ##END=
      # from the end of the file
      if (i == nrow(DF)) DF$LastLine[i] <- length(jdx) - 2
    }

    # Check to see if the apparent last row(s) of a variable list is actually a comment,
    # and adjust accordingly.  Occurs for example in BRUKERNTUP.DX

    while (DF$LastLine[i] %in% comOnly) DF$LastLine[i] <- DF$LastLine[i] - 1
  }

  if (debug == 2) {
    cat("\nApparent data chunks:\n")
    print(DF)
  }

  VL <- vector("list", nrow(DF) + 2)
  VL[[1]] <- DF
  VL[[2]] <- metadata
  VL[[3]] <- comOnly

  for (i in 4:length(VL)) {
    VL[[i]] <- c(DF$Format[i - 2], jdx[DF$FirstLine[i - 2]:DF$LastLine[i - 2]])
  }

  names(VL) <- c("DataGuide", "Metadata", "Comments", paste("VL", 1:(length(VL) - 3), sep = "_"))
  return(VL)
}
