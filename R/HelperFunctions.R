#'
#' Helper Functions used by yValueCheck
#'
#' @noRd
#'

##### Helper function to determine if we are in relayed DIF mode, which are situations
#     like ... DIF DUP DUP (end of line)
#     Returns one of "DIF", "SQZ", "NUM" representing the first non-DUP mode found working backwards
#     DO NOT call if last mode is DIF
.getRelayMode <- function(modeVec) {
  lmv <- length(modeVec)
  if (lmv == 2L) return("CHKPT") # a checkpoint line
  lastMode <- modeVec[lmv]
  if (lastMode == "SQZ") return("SQZ")
  if (lastMode == "NUM") return("NUM")
  if (lastMode == "DUP") { # work backwards until a non-DUP is encountered
    cnt <- 0L
    pos <- lmv
    while (pos != 2L) { # 2L because we don't want to check the X value
      cnt <- cnt + 1L
      pos <- lmv - cnt
      curMode <- modeVec[pos]
      # print(curMode)
      if (curMode == "DUP") next
      if (curMode == "DIF") return("DIF")
      if (curMode == "SQZ") return("SQZ")
      if (curMode == "NUM") return("NUM")
    }
  }
  stop("We shouldn't be here...")
} # end of getMode

##### Helper function that actually runs the Y value check & optionally makes a report if it fails
.yvc <- function(i, firstY, lastY, lineList, debug = 0) {
  attributes(firstY) <- NULL # drop line names
  attributes(lastY) <- NULL
  ychk <- isTRUE(all.equal(firstY[i], lastY[i - 1]))
  # if (!ychk) { # Failed Y value error reporting
    # cat("\nAttempting to sum DIFs, but Y value check failed; nearby values:\n")
    # if (i <= 5) rpt <- 2:6
    # if (i >= 6) rpt <- (i - 2):(i + 2)
    # if (i >= (length(firstY) - 2)) rpt <- (length(firstY) - 5):length(firstY)
    # DF <- data.frame(
      # LineNo = names(lineList)[rpt],
      # FirstYonLine = firstY[rpt], LastYonPrevLine = lastY[rpt - 1],
      # Problem = ifelse(firstY[rpt] == lastY[rpt - 1], "", "*")
    # )
    # print(DF)
    # stop("Y value check failed")
  # }
  ychk
} # end of yvc

##### Helper function that removes extra Y value & the checkpoint value
.cleanYvalues <- function(i, lineList, lineNames, dMode, debug = 0) {
  # Remove the extra Y value that was included for the Y value check
  # DO NOT remove it from the start of the string, it may be the checkpoint value
  # DO remove it from the end of the i - 1 string,
  # See section 5.8.3 of the 1988 publication

  if (debug == 6) {
    cat(
      "\nyValueCheck is removing the last value,", lineList[[i - 1]][length(lineList[[i - 1]])],
      "from", names(lineList[i - 1]), "\n"
    )
  }
  if (i != length(dMode)) lineList[[i - 1]] <- lineList[[i - 1]][-length(lineList[[i - 1]])]

  if (i == length(dMode)) {
    # Note at the checkpoint, lastY and firstY are the same value
    # If we are on checkpoint line, remove it completely.
    # However, at least some vendors don't follow the standard by not using the checkpoint
    # line.  Check for this, DO NOT remove the entire line, but DO remove the extra Y value
    lll <- length(lineList[[i]])
    if (lll > 2L) {
      if (debug == 6) cat("\nyValueCheck does not see a checkpoint value at", names(lineList[i]), "\n")
      lineList[[i]] <- lineList[[i]][-2] # first value is X, second value is the checkpoint value,
      # more Y values follow
    }

    if (lll == 2L) {
      if (debug == 6) cat("\nyValueCheck is removing the checkpoint line,", names(lineList[i]), "\n")
      lineList[[i]] <- NULL
      lineNames <- lineNames[-length(lineNames)]
    }
  }
  # names(lineList) <- lineNames
  lineList
} # end of cleanYvalues
