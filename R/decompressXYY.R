#'
#' Extract the values in JCAMP-DX file with an XYY variable list.
#'
#' This function is NOT EXPORTED.
#' Users would not normally call this function.  See \code{\link{readJDX}}.
#' Documentation is provided for developers wishing to contribute to the package.
#'
#' @param VL Character.  The variable list to be processed as a character vector. Includes one pre-pended
#' line giving the fmt of data (e.g. XYY, XRR, XII).
#'
#' @param params Numeric. Vector of parameters extracted from file header.
#'
#' @param mode Character. One of c("IR_etc", "NMR", "NMR2D")
#'
#' @param debug Integer.  See \code{\link{readJDX}} for details.
#'
#' @param SOFC Logical.  See \code{\link{readJDX}} for details.
#'
#' @return A data frame with elements \code{x} and \code{y}.
#'
#' @importFrom stringr str_locate str_trim
#'
# @noRd
#'

decompressXYY <- function(VL, params, mode, SOFC, debug = 0) {

  # For XYY, each line of the variable list begins with a frequency (x value) in AFFN
  # followed by the y values in various compressed formats.

  fmt <- VL[1]
  VL <- VL[-1] # Remove the pre-pended format string

  if (fmt == "XRR") {
    if (debug >= 1) cat("\nProcessing real data...\n")
  }
  if (fmt == "XII") {
    if (debug >= 1) cat("\nProcessing imaginary data...\n")
  }
  if (fmt == "XYY") {
    if (debug >= 1) cat("\nProcessing variable list...\n")
  }
  if (fmt == "NMR_2D") {
    if (debug >= 1) {
      cat("\nProcessing F2 spectra...", VL[1], "\n")
    }
    VL <- VL[-1] # Remove e.g. ##PAGE= F1= 4.7865152724775 now that we have used it for debugging.
  }

  ### Step 1. Decompress the lines.  This is most of the work.
  # X values present, so lines are X, Y1, Y2, ...
  
  # Remove comment only lines entirely
  comOnly <- grepl("^\\$\\$", VL)
  VL <- VL[!comOnly]
  
  # Remove any comments that follow numbers etc
  VL <- gsub("\\$\\$.*", "", VL)
  
  # Decompress
  LineList <- decompLines(VL, debug = debug)

  ### Step 1a.  Get the X values & check them
  # The X values in the variable list appear in most cases to be significantly rounded relative to FIRSTX.
  # It appears as though the intent of the standard is to construct the sequence of X values from FIRSTX,
  # LASTX, and NPOINTS, not the actual values in the in the variable list. However, the values in the variable list
  # must be checked for integrity, even at their lower precision.

  getX <- function(line) line[1] # Helper Function
  xValues <- lapply(LineList, getX)
  xValues <- unlist(xValues) # line names lost

  if (debug == 3) {
    cat("\nHere are the X values:\n")
    xV <- xValues
    attributes(xV) <- NULL # drop compression code labels
    print(xV)
  }

  if (!is.numeric(xValues)) stop("Parsing xValues failed (not numeric)")

  # Save the first and last xValues for checking a bit later
  firstXcheck <- xValues[1]
  lastXcheck <- xValues[length(xValues)]
  xtol <- 1.5 * abs(mean(diff(xValues), na.rm = TRUE))

  # The standard requires that each line in the variable list be checked to make
  # sure no lines were skipped or duplicated.  Checking for skipping is complicated by the
  # fact that xValues are not evenly spaced, since they depend upon how many yValues were
  # packed into the previous line. So we are not checking for skipped values.
  if (anyDuplicated(xValues[!is.na(xValues)])) stop("Variable list appears to have duplicated lines")
  xValues <- NULL # safety mechanism; recomputed later at higher resolution

  ### Step 1b. Get the y values

  getY <- function(line) line[2:(length(line))] # Helper Function
  yValues <- unlist(lapply(LineList, getY)) # line names lost, but there was debugging back in decompLines

  # Set ytol; some files have low precision FIRSTY etc, and the computation here
  # can lead to values with much greater precision, and hence the all.equal check (later)
  # fails.  In these cases one might consider setting SOFC = FALSE and still
  # get a correct data import.
  ytol <- 2.0 * abs(mean(diff(yValues), na.rm = TRUE))

  ### Step 2. Check the integrity of the results

  if (mode == "IR_etc") {

    # Check that we got the right number of y values

    npoints <- as.integer(params[1])
    firstX <- params[2]
    lastX <- params[3]
    firstY <- params[4]
    factorX <- params[5]
    factorY <- params[6]
    if (debug == 2) cat("\nNPOINTS =", npoints, "\n")
    if (debug == 2) cat("Actual no. data points found  =", length(yValues), "\n")

    if (!npoints == length(yValues)) stop("NPOINTS and length of yValues don't match")

    # Check first y value

    if (!SOFC) warning("SOFC is FALSE, skipping FIRSTY check")

    if (SOFC) {
      if (!isTRUE(all.equal(yValues[1] * factorY, firstY, check.names = FALSE, tolerance = ytol))) {
        cat("First Y value from variable list:", yValues[1] * factorY, "\n")
        cat("First Y value from metadata:", firstY, "\n")
        stop("Error parsing yValues (firstY, IR_etc)")
      }
    }

    # Check first and last xValues (saved earlier).  The standard is ambiguous about doing this,
    # but out of an abundance of caution we will do it.  xtol is set pretty high due to
    # precision loss; X values at the start of lines are heavily rounded.

    if (!SOFC) warning("SOFC is FALSE, skipping FIRSTX check")

    if (SOFC) {
      if (!isTRUE(all.equal(firstXcheck * factorX, firstX, check.names = FALSE, tolerance = xtol))) {
        cat("First X value from variable list:", firstXcheck * factorX, "\n")
        cat("First X value from metadata:", firstX, "\n")
        stop("Error parsing xValues (firstX, IR_etc)")
      }
    }

    if (!SOFC) warning("SOFC is FALSE, skipping LASTX check")

    if (SOFC) {
      if (!isTRUE(all.equal(lastXcheck * factorX, lastX, check.names = FALSE, tolerance = xtol))) {
        cat("Last x value from variable list:", lastXcheck * factorX, "\n")
        cat("Last x value from metadata:", lastX, "\n")
        stop("Error parsing xValues (lastX, IR_etc)")
      }
    }

    # Compute xValues based on params (see notes earlier); update yValues

    dx <- (lastX - firstX) / (npoints - 1)
    xValues <- seq(firstX, lastX, by = dx)
    yValues <- yValues * factorY
  } # end of mode = "IR_etc"

  if (mode == "NMR") {
    pointsX <- as.integer(params[1])
    pointsR <- as.integer(params[2])
    pointsI <- as.integer(params[3])
    firstX <- params[4]
    firstR <- params[5]
    firstI <- params[6]
    lastX <- params[7]
    lastR <- params[8]
    lastI <- params[9]
    factorX <- params[10]
    factorR <- params[11]
    factorI <- params[12]

    if (debug == 2) cat("\nNo. data points from metadata =", pointsX, "\n")
    if (debug == 2) cat("Actual no. data points found  =", length(yValues), "\n")

    if (pointsX != length(yValues)) stop("Data points found != data points in metadata")

    # Check first and last xValues (saved earlier).  The standard is ambiguous about doing this,
    # but out of an abundance of caution we will do it.

    if (!SOFC) warning("SOFC is FALSE, skipping FIRSTX check")

    if (SOFC) {
      if (!isTRUE(all.equal(firstXcheck * factorX, firstX, check.names = FALSE, tolerance = xtol))) {
        cat("First X value from variable list:", firstXcheck * factorX, "\n")
        cat("First X value from metadata:", firstX, "\n")
        stop("Error parsing xValues (firstX, NMR)")
      }
    }

    if (!SOFC) warning("SOFC is FALSE, skipping LASTX check")

    if (SOFC) {
      if (!isTRUE(all.equal(lastXcheck * factorX, lastX, check.names = FALSE, tolerance = xtol))) {
        cat("Last X value from variable list:", lastXcheck * factorX, "\n")
        cat("Last X value from metadata:", lastX, "\n")
        stop("Error parsing xValues (lastX, NMR)")
      }
    }


    if (fmt == "XRR") { # Check yValues (real)

      yValues <- yValues * factorR

      if (!SOFC) warning("SOFC is FALSE, skipping FIRSTR check")

      if (SOFC) {
        if (!isTRUE(all.equal(yValues[1], firstR, check.names = FALSE, tolerance = ytol))) {
          cat("First real value from variable list:", yValues[1] * factorR, "\n")
          cat("First real value from metadata:", firstR, "\n")
          stop("Error parsing real values (firstR, NMR)")
        }
      }

      if (!SOFC) warning("SOFC is FALSE, skipping LASTR check")

      if (SOFC) {
        if (!isTRUE(all.equal(yValues[length(yValues)], lastR, check.names = FALSE, tolerance = ytol))) {
          cat("Last real value from variable list:", yValues[length(yValues)] * factorR, "\n")
          cat("Last real value from metadata:", lastR, "\n")
          stop("Error parsing real values (lastR, NMR)")
        }
      }
    } # end of XRR

    if (fmt == "XII") { # Check yValues (imaginary)

      yValues <- yValues * factorI


      if (!SOFC) warning("SOFC is FALSE, skipping FIRSTI check")

      if (SOFC) {
        if (!isTRUE(all.equal(yValues[1], firstI, check.names = FALSE, tolerance = ytol))) {
          cat("First imaginary value from variable list:", yValues[1] * factorI, "\n")
          cat("First imaginary value from metadata:", firstI, "\n")
          stop("Error parsing imaginary values (firstI, NMR)")
        }
      }

      if (!SOFC) warning("SOFC is FALSE, skipping LASTI check")

      if (SOFC) {
        if (!isTRUE(all.equal(yValues[length(yValues)], lastI, check.names = FALSE, tolerance = ytol))) {
          cat("Last imaginary value from variable list:", yValues[length(yValues)] * factorI, "\n")
          cat("Last imaginary value from metadata:", lastI, "\n")
          stop("Error parsing imaginary values (lastI, NMR)")
        }
      }
    } # end of XII


    # Compute xValues based on params (see notes earlier)

    dx <- (lastX - firstX) / (pointsX - 1)
    xValues <- seq(firstX, lastX, by = dx)
  } # end of mode = "NMR"

  if (mode == "NMR2D") {
    pointsF1 <- as.integer(params[1])
    pointsF2 <- as.integer(params[2])
    firstF1 <- params[3]
    firstF2 <- params[4]
    lastF1 <- params[5]
    lastF2 <- params[6]
    factorF1 <- params[7]
    factorF2 <- params[8]
    factorZ <- params[9]

    if (debug == 2) cat("\nNo. F2 points from metadata =", pointsF2, "\n")
    if (debug == 2) cat("Actual F2 points found  =", length(yValues), "\n")

    if (pointsF2 != length(yValues)) stop("Data points found != data points in metadata")

    # Check first and last xValues (saved earlier).  The standard is ambiguous about doing this,
    # but out of an abundance of caution we will do it.

    if (!SOFC) warning("SOFC is FALSE, skipping FIRSTF2 check")

    if (SOFC) {
      if (!isTRUE(all.equal(firstXcheck * factorF2, firstF2, check.names = FALSE, tolerance = xtol))) {
        cat("First F2 value from variable list:", firstXcheck * factorF2, "\n")
        cat("First F2 value from metadata:", firstF2, "\n")
        stop("Error parsing xValues (firstF2, NMR2D)")
      }
    }


    if (!SOFC) warning("SOFC is FALSE, skipping LASTF2 check")

    if (SOFC) {
      if (!isTRUE(all.equal(lastXcheck * factorF2, lastF2, check.names = FALSE, tolerance = xtol))) {
        cat("Last F2 value from variable list:", lastXcheck * factorF2, "\n")
        cat("Last F2 value from metadata:", lastF2, "\n")
        stop("Error parsing xValues (lastF2, NMR2D)")
      }
    }

    # There is a poorly-documented check of the first y value in the 2D NMR format.
    # For the time-being we will not do the check, as it only checks one value.

    yValues <- yValues * factorZ

    # Compute xValues based on params (see notes earlier)

    dx <- (lastF2 - firstF2) / (pointsF2 - 1)
    xValues <- seq(firstF2, lastF2, by = dx)
  } # end of mode = "NMR2D"

  ### And we're done...

  xydata <- data.frame(x = xValues, y = yValues)
  return(xydata)
}
