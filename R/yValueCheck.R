#'
#' Conditionally Carry Out the y Value Check
#'
#' This function is NOT EXPORTED.
#' Users would not normally call this function.  See \code{\link{readJDX}}.
#' Documentation is provided for developers wishing to contribute to the package.
#'
#' @section Details:
#' Be sure to see the commentary about how and when to run the Y value check in \code{\link{readJDX}}.
#'
#' @param lineList A list of numeric vectors to be checked.  Named with line numbers.
#'        Individual entries are named with the ASDF code.  X values are still present.
#'
#' @param debug Integer.  See \code{\link{readJDX}} for details.
#'
#' @return A list of numeric vectors, after the y check has been done and any extra Y
#'         values removed.
#'
#' @noRd
#'

yValueCheck <- function(lineList, debug = 0) {
  if (debug == 6) cat("\nCarrying out Y value checks...\n\n")

  lineNames <- names(lineList) # grab to re-use later when nuked

  # Figure out first and last Y values on all lines, makes checking & reporting problems easier later
  # Note X values are still here, must skip over them and ask for 2nd entry
  fun <- function(x) {
    x[2]
  }
  firstY <- unlist(lapply(lineList, fun), use.names = FALSE)
  fun <- function(x) {
    x[length(x)]
  }
  lastY <- unlist(lapply(lineList, fun), use.names = TRUE)

  # Get the last mode for each line
  lastMode <- names(lastY)
  lastMode <- gsub("Line_[0-9]*\\.", "", lastMode) # names were whacked during unlisting

  # Run the Y value check; be sure to see the commentary in ?readJDX
  # We will try several approaches so the user has the best chance of success

  for (i in 2:length(lastMode)) { # i indexes lineList, lineNames and lastMode

    if (lastMode[i - 1] %in% c("SQZ", "PAC", "NUM")) next # no need to do anything

    yValChkOK <- FALSE

    if (lastMode[i - 1] == "DIF") {
      if (debug == 6) cat("yValueCheck sees a literal DIF mode on", lineNames[i - 1], "\n")
      yValChkOK <- .yvc(i, firstY, lastY) # literal DIF mode
    }

    if (!yValChkOK) { # Not in literal DIF mode. Check for relayed DIF mode
      if (lastMode[i - 1] == "DUP") {
        relayMode <- .getRelayMode(names(lineList[[i - 1]]))
        if (debug == 6) {
          if (relayMode == "CHKPT") cat("yValueCheck sees a checkpoint on", lineNames[i - 1], "\n")
          if (relayMode == "NOTDIF") cat("yValueCheck:", lineNames[i - 1], "is not in DIF mode\n")
          if (relayMode == "DIF") cat("yValueCheck sees a", relayMode, "followed by DUPs on", lineNames[i - 1], "\n")
        }
        if (relayMode == "DIF") {
          yValChkOK <- .yvc(i, firstY, lastY) # relayed DIF mode
        }
      }
    }

    # If Y value check was good, remove the extra value
    if (yValChkOK) lineList <- .cleanYvalues(i, lineList, lineNames, lastMode, debug)

    # If Y value check failed...
    if (!yValChkOK) {
      if (lastMode[i - 1] == "DUP") next # In this case apparently DIF mode was expected to be literal
      # so we can move on; otherwise something is wrong and make a report
      cat("\nAttempting to sum DIFs, but Y value check failed; nearby values:\n")
      if (i <= 5) rpt <- 2:6 # start of lineList
      if (i >= 6) rpt <- (i - 2):(i + 2) # middle of lineList
      if (i >= (length(firstY) - 2)) rpt <- (length(firstY) - 5):length(firstY) # end of lineList
      if (length(lineList) <= 5) rpt <- 2:(length(lineList)) # short lineList in standalone mode
      DF <- data.frame(
        LineNo = names(lineList)[rpt],
        FirstYonLine = firstY[rpt], LastYonPrevLine = lastY[rpt - 1],
        Problem = ifelse(firstY[rpt] == lastY[rpt - 1], "", "*")
      )
      rownames(DF) <- NULL
      print(DF)
      stop("Y value check failed")
    } # end of failed Y value check
  } # end of main loop


  if (debug == 6) cat("\n...Y value checks completed\n")

  lineList
} # end of yValueCheck
