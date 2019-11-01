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
  if (lastMode == "SQZ") return("NOTDIF")
  if (lastMode == "NUM") return("NOTDIF")
  if (lastMode == "DUP") { # work backwards until a non-DUP is encountered
    cnt <- 0L
    pos <- lmv
    while (pos != 2L) { # 2L because we don't want to check the X value
      cnt <- cnt + 1L
      pos <- lmv - cnt
      curMode <- modeVec[pos]
      if (curMode == "DUP") next
      if (curMode == "DIF") return("DIF")
      if (curMode == "SQZ") return("NOTDIF")
      if (curMode == "NUM") return("NOTDIF")
    }
  }
  stop("We shouldn't be here...")
} # end of getMode

##### Helper function that actually runs the Y value check
.yvc <- function(i, firstY, lastY) {
  attributes(firstY) <- NULL # drop line names
  attributes(lastY) <- NULL
  ychk <- isTRUE(all.equal(firstY[i], lastY[i - 1]))
  ychk
} # end of yvc

##### Helper function that removes extra Y value & the checkpoint value
.cleanYvalues <- function(i, lineList, lineNames, dMode, debug = 0) {
  # Remove the extra Y value that was included for the Y value check
  # DO NOT remove it from the start of the string, it may be the checkpoint value
  # DO remove it from the end of the i - 1 string,
  # See section 5.8.3 of the 1988 publication

  if (debug >= 6) {
    cat(
      "yValueCheck is removing the last value,", lineList[[i - 1]][length(lineList[[i - 1]])],
      "from", names(lineList[i - 1]), "\n\n"
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
      if (debug >= 6) cat("\nyValueCheck:", names(lineList[i]), "does not appear to be a checkpoint line\n\n")
      lineList[[i]] <- lineList[[i]][-2] # first value is X, second value is the checkpoint value,
      # more Y values follow
    }

    if (lll == 2L) {
      if (debug >= 6) cat("yValueCheck is removing the checkpoint line,", names(lineList[i]), "\n\n")
      lineList[[i]] <- NULL
      lineNames <- lineNames[-length(lineNames)] # don't need?
    }
  }
  lineList
} # end of cleanYvalues
