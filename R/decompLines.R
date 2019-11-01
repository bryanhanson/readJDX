#'
#' Decompress the Data Lines in a JCAMP-DX File
#'
#' This function is used by the package, and can also be called by the user who wishes
#' to study problematic lines in a file in a standalone manner.  See the examples.
#'
#' @param VL Character.  A vector of character strings composed of the compressed
#'        data strings in the file.  If coming from the package internals, named as "Line_1" etc.
#'        If used in standalone mode, names will be added/overwritten as "Line_1" etc.
#'
#' @param debug Integer.  See \code{\link{readJDX}} for details.
#'
#' @return A list of numeric strings, the result of unpacking the compressed data.  One list
#'         element is returned for each line.  The numeric string is named with the ASDF compression mode.
#'
#' @section Details:
#'          Each individual line passed here is converted to a list to make processing and
#'          naming the pieces easier.The individual
#'          entries are named according to their \emph{original} ASDF compression code.  Unless otherwise noted
#'          all functions called from here should act on a list of lines, via lapply. Note too that the X
#'          values are present, e.g. X, Y1, Y2 ... Yn.
#'
#' @section Formats:
#'          AFFN is separated by any amount of white space so processing is straightforward, as exponents are
#'          handled automatically, and white space stripped off automatically, courtesy of R internals.  It appears
#'          AFFN is never mixed with other formats.  The other formats are collectively called ASDF in the standard.
#'
#' @importFrom stringr str_replace_all str_trim
#' @export
#'
#' @examples
#' testLines1 <- c(
#'   "482A885145L989378k853295J46714q39581j382088R41076k774051J365135l135709P53917",
#'   "472a903359j71857q18832K573831k615133L481852l395846L894844l478693M916433",
#'   "463B483240m513444O146172m826168N233079m522551M000252m097028L466111l460183",
#'   "454i0520L061628k524598K788931k509430L219286k511160K709095k122775J594246"
#' )
#' demo1 <- decompLines(testLines1, debug = 6)
#'
#' testLines2 <- c(
#'   # EU AFFN:
#'   "1898,58486802541 -0,0170190036296844 -0,0170874372124672 -0,0171865783631802",
#'   "1917,23501403401 -0,0176097713410854 -0,0177919361740351 -0,0179808251559734",
#'   # AFFN with fixed field width/extra space:
#'   "           16383       2259260      -5242968      -7176216      -1616072",
#'   "           16379      10650432       4373926      -3660824       2136488"
#' )
#'
#' demo2 <- decompLines(testLines2, debug = 6)
decompLines <- function(VL, debug = 0) {
  comp <- getComp(VL, debug = debug)

  lineNames <- names(VL) # save to replace when functions nuke
  # Next line ensures correct names exist when passing a vector of lines to decompLines in standalone mode
  if (is.null(lineNames)) lineNames <- paste("Line", 1:length(VL), sep = "_")

  if (debug >= 4L) {
    cat("\n\n\n====================  Raw lines:\n\n")
    print(VL)
  }

  # Helper Function
  nameComp <- function(cvec) { # cvec = character vector
    names(cvec) <- rep(NA_character_, length(cvec))
    names(cvec)[grepl("[@A-Ia-i]{1}", cvec)] <- "SQZ"
    names(cvec)[grepl("[%J-Rj-r]{1}", cvec)] <- "DIF"
    names(cvec)[grepl("[S-Zs]{1}", cvec)] <- "DUP"
    names(cvec)[!grepl("[@%A-Za-rs]", cvec)] <- "NUM" # anything else is a NUM/AFFN
    cvec
  }

  # Preliminaries
  VL <- gsub(",", ".", VL) # replace ',' with '.' -- needed for EU style files
  VL <- gsub("\\s+\\${2}.*$", "", VL) # remove any ...xxxx  $$ checkpoint type entries (done earlier now)
  VL <- gsub("(\\+|-){1}([0-9]+)", " \\1\\2", VL) # put space ahead of +|- signs (PAC)
  VL <- str_trim(VL, side = "both") # remove extra white space
  VL <- str_replace_all(VL, "([@%A-Za-rs])", " \\1") # break into pieces by compression mode

  # Convert to list to process each piece separately
  lineList <- as.list(VL)
  names(lineList) <- lineNames # replace the names that were nuked

  FUN <- function(x) {
    unlist(strsplit(x, "\\s+"))
  }
  lineList <- lapply(lineList, FUN)

  # Name each entry by compression mode
  lineList <- lapply(lineList, nameComp) # name each entry by the compression mode

  if (debug >= 4L) {
    cat("\n\n\n====================  Lines after preliminary processing:\n\n")
    print(lineList)
    DF1 <- lengths(lineList)
    DF2 <- cumsum(lengths(lineList))
  }

  # Replace SQZ codes with the corresponding digit
  if ("SQZ" %in% comp) lineList <- lapply(lineList, unSQZ)

  if (debug >= 4L) {
    DF3 <- lengths(lineList)
    DF4 <- cumsum(lengths(lineList))
  }

  # Replicate any DUP entries
  if ("DUP" %in% comp) lineList <- lapply(lineList, insertDUPs, debug = debug)

  if (debug >= 4L) {
    DF5 <- lengths(lineList)
    DF6 <- cumsum(lengths(lineList))
  }

  # Finally, take care of any DIFs
  # Done last, since only now are we in a position to convert the string to numeric
  # Next call based on https://stackoverflow.com/a/6253459/633251
  if ("DIF" %in% comp) lineList <- lapply(names(lineList), deDIF, lineList = lineList, debug = debug)

  if (debug >= 4L) {
    DF7 <- lengths(lineList)
    DF8 <- cumsum(lengths(lineList))
  }

  # A y value check is required if the last entry on a line is in DIF mode.
  # We will run the check if any line of the VL is in DIF mode.  Possibley a tiny bit wasteful.
  # The y value check requires two lines at a time, since the last value on one line
  # is compared to the first value on the next.  Therefore lapply cannot be used.
  # The yValueCheck will fail if there is a problem.

  names(lineList) <- lineNames

  if (("DIF" %in% comp) & (length(lineList) > 1)) lineList <- yValueCheck(lineList, debug = debug)

  if (debug >= 4L) {
    DF9 <- lengths(lineList)
    DF10 <- cumsum(lengths(lineList))
  }

  if (!is.numeric(lineList)) lineList <- lapply(lineList, as.numeric) # first part is always true, its a list!

  if (debug >= 4L) {
    DF11 <- lengths(lineList)
    DF12 <- cumsum(lengths(lineList))
    DF13 <- cumsum(lengths(lineList) - 1) # count w/o X values
  }

  if (debug >= 4L) {
    cat("\n\n\n====================  Lines after full processing to numeric:\n\n")
    print(lineList)

    # if (length(DF1) != length(DF9)) { # checkpoint line was removed; pad it
    # DF9 <- c(DF9, NA)
    # DF10 <- c(DF10, NA)
    # DF11 <- c(DF11, NA)
    # DF12 <- c(DF12, NA)
    # DF13 <- c(DF13, NA)
    # }

    # cat("\n\n\n====================  Summary of line counts during processing:\n\n")
    # DF <- data.frame(DF1, DF2, DF3, DF4, DF5, DF6, DF7, DF8, DF9, DF10, DF11, DF12, DF13)
    # names(DF) <- c("PRE", "P_CS", "SQZ", "S_CS", "DUP", "DU_CS", "DIF", "DI_CS", "YV", "Y_CS", "NUM", "N_CS", "YCNT")
    # print(DF, width = 100)

    # # Further checks that only report problems
    # keep <- "PRE"
    # if ("SQZ" %in% comp) keep <- c(keep, "SQZ")
    # if ("DUP" %in% comp) keep <- c(keep, "DUP")
    # if ("DIF" %in% comp) keep <- c(keep, "DIF")
    # DFcore <- DF[, keep]

    # for (i in 1:nrow(DFcore)) {
    # if (length(unique(DFcore[i,,drop = TRUE])) != 1L) {
    # cat("Possible problem in parsing ASDF codes to numeric values:\n")
    # print(DFcore[i,])
    # }
    # }
  } # end of debug = 4

  lineList
}
