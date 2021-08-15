#'
#' Extract Parameters from Metadata
#'
#' This function is NOT EXPORTED.
#' Users would not normally call this function.  See \code{\link{readJDX}}.
#' Documentation is provided for developers wishing to contribute to the package.
#'
#' @param md Character.  A vector of character strings which contains the metadata.
#'
#' @template mode-arg
#'
#' @template SOFC-arg
#'
#' @template debug-arg
#'
#' @return A named numeric vector containing the extracted parameters.
#'         Contents will vary by \code{mode}.
#'
#' @noRd
#'
extractParams <- function(md, mode, SOFC, debug = 0) {

  if (mode == "XYY") {

    # The following parameters must be found

    firstX <- grep("^\\s*##FIRSTX\\s*=", md)
    if (firstX == 0) stop("Couldn't find FIRSTX")
    firstX <- md[firstX]
    firstX <- sub("^\\s*##FIRSTX\\s*=", replacement = "", firstX)
    firstX <- gsub(",", ".", firstX) # for EU style files
    firstX <- as.numeric(firstX)

    lastX <- grep("^\\s*##LASTX\\s*=", md)
    if (lastX == 0) stop("Couldn't find LASTX")
    lastX <- md[lastX]
    lastX <- sub("^\\s*##LASTX\\s*=", replacement = "", lastX)
    lastX <- gsub(",", ".", lastX) # for EU style files
    lastX <- as.numeric(lastX)

    npoints <- grep("^\\s*##NPOINTS\\s*=", md)
    if (npoints == 0) stop("Couldn't find NPOINTS")
    npoints <- md[npoints]
    npoints <- sub("^\\s*##NPOINTS\\s*=", replacement = "", npoints)
    npoints <- as.integer(npoints)

    factorX <- grep("^\\s*##XFACTOR\\s*=", md)
    if (factorX == 0) stop("Couldn't find XFACTOR")
    factorX <- sub("^\\s*##XFACTOR\\s*=", replacement = "", md[factorX])
    factorX <- gsub(",", ".", factorX) # for EU style files
    factorX <- as.numeric(factorX)

    factorY <- grep("^\\s*##YFACTOR\\s*=", md)
    if (factorY == 0) stop("Couldn't find YFACTOR")
    factorY <- sub("^\\s*##YFACTOR\\s*=", replacement = "", md[factorY])
    factorY <- gsub(",", ".", factorY) # for EU style files
    factorY <- as.numeric(factorY)

    # The following parameters may be skipped by setting SOFC = FALSE

    if (!SOFC) {
      firstY <- NA_real_ # warning issued in decompressJDXxyy
    }

    if (SOFC) {
      firstY <- grep("^\\s*##FIRSTY\\s*=", md)
      if (firstY == 0) stop("Couldn't find FIRSTY")
      firstY <- md[firstY]
      firstY <- sub("^\\s*##FIRSTY\\s*=", replacement = "", firstY)
      firstY <- gsub(",", ".", firstY) # for EU style files
      firstY <- as.numeric(firstY)
    }

    params <- c(as.numeric(npoints), firstX, lastX, firstY, factorX, factorY)
    names(params) <- c("npoints", "firstX", "lastX", "firstY", "factorX", "factorY")

    if (debug == 2) {
      cat("\nExtracted parameters:\n")
      print(params)
    }
  } # end of mode == XYY

  if (mode == "NMR_1D") {

    # This section does NOT currently make the EU conversion; watch out for strsplit choice
    # No parameters in this section can be skipped via SOFC

    npoints <- grep("^\\s*##VAR(\\s{1}|_)DIM\\s*=", md)
    # JEOL seems to use a space, not underscore
    if (npoints == 0) stop("Couldn't find VAR_DIM")
    npoints <- md[npoints]
    npoints <- sub("^\\s*##VAR(\\s{1}|_)DIM\\s*=", replacement = "", npoints)
    npoints <- as.numeric(unlist(strsplit(npoints, ",")))
    npoints <- npoints[1:3] # JEOL at least has a 4th entry, PAGE, Bruker doesn't (?)

    firsts <- grep("^\\s*##FIRST\\s*=", md)
    if (firsts == 0) stop("Couldn't find FIRST")
    firsts <- md[firsts]
    firsts <- sub("^\\s*##FIRST\\s*=", replacement = "", firsts)
    firsts <- as.numeric(unlist(strsplit(firsts, ",")))
    firsts <- firsts[1:3]

    lasts <- grep("^\\s*##LAST\\s*=", md)
    if (lasts == 0) stop("Couldn't find LAST")
    lasts <- md[lasts]
    lasts <- sub("^\\s*##LAST\\s*=", replacement = "", lasts)
    lasts <- as.numeric(unlist(strsplit(lasts, ",")))
    lasts <- lasts[1:3]

    factors <- grep("^\\s*##FACTOR\\s*=", md)
    if (factors == 0) stop("Couldn't find FACTOR")
    factors <- md[factors]
    factors <- sub("^\\s*##FACTOR\\s*=", replacement = "", factors)
    factors <- as.numeric(unlist(strsplit(factors, ",")))
    factors <- factors[1:3]

    pointsX <- npoints[1]
    pointsR <- npoints[2]
    pointsI <- npoints[3]

    firstX <- firsts[1]
    firstR <- firsts[2]
    firstI <- firsts[3]

    lastX <- lasts[1]
    lastR <- lasts[2]
    lastI <- lasts[3]

    factorX <- factors[1]
    factorR <- factors[2]
    factorI <- factors[3]

    params <- c(
      as.numeric(pointsX), as.numeric(pointsR), as.numeric(pointsI),
      firstX, firstR, firstI, lastX, lastR, lastI, factorX, factorR, factorI
    )
    names(params) <- c(
      "pointsX", "pointsR", "pointsI", "firstX", "firstR", "firstI",
      "lastX", "lastR", "lastI", "factorX", "factorR", "factorI"
    )

    if (debug == 2) {
      cat("\nExtracted parameters:\n")
      print(params)
    }

    if ((pointsX != pointsR) | (pointsX != pointsI)) stop("No. of frequency, real, imaginary points are not the same")
  } # end of mode == "NMR_1D"

  if (mode == "NMR_2D") {

    # This section does NOT currently make the EU conversion; watch out for strsplit choice
    # No parameters in this section can be skipped via SOFC

    npoints <- grep("^\\s*##VAR(\\s{1}|_)DIM\\s*=", md)
    # JEOL seems to use a space, not underscore
    if (npoints == 0) stop("Couldn't find VAR_DIM")
    npoints <- md[npoints]
    npoints <- sub("^\\s*##VAR(\\s{1}|_)DIM\\s*=", replacement = "", npoints)
    npoints <- as.numeric(unlist(strsplit(npoints, ",")))
    npoints <- npoints[-length(npoints)] # see above for a change that might be needed here as well

    firsts <- grep("^\\s*##FIRST\\s*=", md)
    if (length(firsts) == 0) stop("Couldn't find FIRST")
    firsts <- md[firsts]
    firsts <- sub("^\\s*##FIRST\\s*=", replacement = "", firsts)
    firsts <- as.numeric(unlist(strsplit(firsts, ",")))

    lasts <- grep("^\\s*##LAST\\s*=", md)
    if (lasts == 0) stop("Couldn't find LAST")
    lasts <- md[lasts]
    lasts <- sub("^\\s*##LAST\\s*=", replacement = "", lasts)
    lasts <- as.numeric(unlist(strsplit(lasts, ",")))

    factors <- grep("^\\s*##FACTOR\\s*=", md)
    if (factors == 0) stop("Couldn't find FACTOR")
    factors <- md[factors]
    factors <- sub("^\\s*##FACTOR\\s*=", replacement = "", factors)
    factors <- as.numeric(unlist(strsplit(factors, ",")))

    pointsF1 <- npoints[1]
    pointsF2 <- npoints[2]

    firstF1 <- firsts[1]
    firstF2 <- firsts[2]

    lastF1 <- lasts[1]
    lastF2 <- lasts[2]

    factorF1 <- factors[1]
    factorF2 <- factors[2]
    factorZ <- factors[3]

    params <- c(
      as.numeric(pointsF1), as.numeric(pointsF2),
      firstF1, firstF2, lastF1, lastF2, factorF1, factorF2, factorZ
    )
    names(params) <- c(
      "pointsF1", "pointsF2", "firstF1", "firstF2",
      "lastF1", "lastF2", "factorF1", "factorF2", "factorZ"
    )

    if (debug == 2) {
      cat("\nExtracted parameters:\n")
      print(params)
    }
  } # end of mode == "NMR_2D"

  if (mode == "LC_MS") {

    # This section does NOT currently make the EU conversion; watch out for strsplit choice
    # No parameters in this section can be skipped via SOFC

    # There do not appear to be any officially mandated checks, but we'll try to make some anyway,
    # but use the SOFC mechanism

    # TODO We are not currently checking any of these values; possibly the same in NMR_2D above

    npoints <- grep("^\\s*##VAR_DIM\\s*=", md) # in LC-MS this is the number of time points
    if (SOFC) if (npoints == 0L) stop("Couldn't find VAR_DIM")
    if (npoints != 0L) {
      npoints <- md[npoints]
      npoints <- sub("^\\s*##VAR_DIM\\s*=", replacement = "", npoints)
      npoints <- as.numeric(unlist(strsplit(npoints, ",")))
    }

    firsts <- grep("^\\s*##FIRST\\s*=", md)
    if (SOFC) if (length(firsts) == 0) stop("Couldn't find FIRST")
    if (firsts != 0L) {
      firsts <- md[firsts]
      firsts <- sub("^\\s*##FIRST\\s*=", replacement = "", firsts)
      firsts <- as.numeric(unlist(strsplit(firsts, ",")))
    }

    lasts <- grep("^\\s*##LAST\\s*=", md)
    if (SOFC) if (lasts == 0) stop("Couldn't find LAST")
    if (lasts != 0L) {
      lasts <- md[lasts]
      lasts <- sub("^\\s*##LAST\\s*=", replacement = "", lasts)
      lasts <- as.numeric(unlist(strsplit(lasts, ",")))
    }

    # The following assumes Waters Acquity QDA which exports the first and
    # last values in the wrong order (they refer to time, but are in the intensity position in the vector)
    # In the future, may need to extract vendor and use that info here
    time_points <- npoints[2]
    first_time <- firsts[3]
    last_time <- lasts[3]

    # if !SOFC these values will be zero
    params <- c(
      as.numeric(time_points), as.numeric(first_time), as.numeric(last_time)
    )
    names(params) <- c("time_points", "first_time", "last_time")

    if (debug == 2) {
      cat("\nExtracted parameters:\n")
      print(params)
    }
  } # end of mode == "LC_MS"

  if (mode == "XYXY") {

    # There are no official checks for this format (?)

    npoints <- grep("^\\s*##NPOINTS\\s*=", md)
    if (SOFC) if (npoints == 0) stop("Couldn't find NPOINTS")
    if (npoints != 0L) {
      npoints <- md[npoints]
      npoints <- sub("^\\s*##NPOINTS\\s*=", replacement = "", npoints)
      npoints <- as.integer(npoints)
    }

    params <- npoints # if !SOFC this will be 0L
    names(params) <- "npoints"

    if (debug == 2) {
      cat("\nExtracted parameters:\n")
      print(params)
    }
  } # end of mode == XYXY

  return(params)
} # end of extractParams
