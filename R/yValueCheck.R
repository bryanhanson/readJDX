#'
#' Conditionally Carry Out the y Value Check
#'
#' This function is NOT EXPORTED.
#' Users would not normally call this function.  See \code{\link{readJDX}}.
#' Documentation is provided for developers wishing to contribute to the package.
#'
#' @param lineList A list of numeric vectors to be checked.  Named with line numbers.
#'        Individual numbers are named with the ASDF code.  X values are still present!
#'
#' @param debug Integer.  See \code{\link{readJDX}} for details.
#'
#' @return A list of numeric vectors, after the y check has been done and any extra
#'         values removed. It will stop if there is a problem.
#'
# @noRd
#'

yValueCheck <- function(lineList, debug = 0) {
  	  
  if (debug == 6) cat("\nCarrying out Y value check...\n")

  lineNames <- names(lineList) # grab to re-use later when nuked
  
  # Figure out first and last Y values on all lines, makes checking & reporting problems easier later
  # Note X values are still here, must skip over them and ask for 2nd entry
  fun <- function(x) { x[2] }
  firstY <- unlist(lapply(lineList, fun), use.names = FALSE)
  fun <- function(x) { x[length(x)] }
  lastY <- unlist(lapply(lineList, fun), use.names = TRUE)
  
  # Get the last mode for each line
  lastMode <- names(lastY)
  lastMode <- gsub("Line_[0-9]*\\.", "", lastMode) # names were whacked during unlisting
    
  # Run the Y value check; BE SURE TO SEE THE COMMENTARY in ?readJDX
  for (i in 2:length(lastMode)) { # i indexes both lineList and lastMode
  	if (debug == 7) {
  		if (lastMode[i] == "DIF") cat("yValueCheck sees a literal DIF mode on", lineNames[i], "\n")
   	    if (lastMode[i] != "DIF") {
   	    	relayMode <- .getRelayMode(names(lineList[[i]]))
   	    	if (relayMode == "CHKPT") cat("yValueCheck sees a checkpoint on", lineNames[i], "\n")
   	    	if (relayMode != "CHKPT") cat("yValueCheck sees a", relayMode, "followed by DUPs on", lineNames[i], "\n")
 		}
  	}
    yValChkOK <- .yvc(i, firstY, lastY, lineList, debug)
    if (yValChkOK) lineList <- .cleanYvalues(i, lineList, lineNames, lastMode, debug)
    if (!yValChkOK) next
  }
  
  if (debug == 6) cat("\nY value check completed...\n")
  
  lineList
  } # end of yValueCheck
