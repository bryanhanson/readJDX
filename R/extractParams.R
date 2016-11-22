##'
##' Extract Parameters from Metadata
##'
##' This function is NOT EXPORTED.
##' Users would not normally call this function.  See \code{\link{readJDX}}.
##' Documentation is provided for developers wishing to contribute to the package.
##' 
##' @param md Character.  A vector of character strings which contains
##' the metadata.
##'
##' @param NMR Logical. Is this NMR data?
##'
##' @param SOFC Logical. Stop on Failed Check.   See \code{\link{readJDX}} for details.
##'
##' @param debug Integer.  See \code{\link{readJDX}} for details.
##'
##' @return A numeric vector containing the extracted parameters.
##' Contents will be different if NMR data is being processed.
##' 
##' @noRd
##'
extractParams <- function (md, NMR, SOFC, debug = 0){

	if (!NMR) {
		
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

		# The following parameters may be skipped with SOFC = FALSE
		
		if (!SOFC) {
			#warning("SOFC is FALSE, skipping FIRSTY check") # warning issued in decompressJDXxyy
			firstY <- NA_real_
			# warning("SOFC is FALSE, setting FACTORY = 1.0")
			# factorY <- 1.0
			# warning("SOFC is FALSE, setting FACTORX = 1.0")
			# factorX <- 1.0
			}
		
		if (SOFC) {		
			firstY <- grep("^\\s*##FIRSTY\\s*=", md)
			if (firstY == 0) stop("Couldn't find FIRSTY")
			firstY <- md[firstY]
			firstY <- sub("^\\s*##FIRSTY\\s*=", replacement = "", firstY)
			firstY <- gsub(",", ".", firstY) # for EU style files
			firstY <- as.numeric(firstY)	
			}

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
		
		params <- c(as.numeric(npoints), firstX, lastX, firstY,  factorX, factorY)
		names(params) <- c("npoints", "firstX", "lastX", "firstY", "factorX", "factorY")

		if (debug >= 1) {
			message("Extracted parameters:")
			print(params)
			}
	
		} # end of !NMR

	if (NMR)	 {
		
		# This section needs the EU conversion; watch out for strsplit choice
		# No parameters in this section can be skipped via SOFC
		
		npoints <- grep("^\\s*##VAR(\\s{1}|_)DIM\\s*=", md)
		# JEOL seems to use a space, not underscore
		if (npoints == 0) stop("Couldn't find VAR_DIM")
		npoints <- md[npoints]
		npoints <- sub("^\\s*##VAR(\\s{1}|_)DIM\\s*=", replacement = "", npoints)
		npoints <- as.numeric(unlist(strsplit(npoints, ",")))
		npoints <- npoints[-length(npoints)]

		firsts <- grep("^\\s*##FIRST\\s*=", md)
		if (firsts == 0) stop("Couldn't find FIRST")
		firsts <- md[firsts]
		firsts <- sub("^\\s*##FIRST\\s*=", replacement = "", firsts)
		firsts <- as.numeric(unlist(strsplit(firsts, ",")))
		firsts <- firsts[-length(firsts)]

		lasts <- grep("^\\s*##LAST\\s*=", md)
		if (lasts == 0) stop("Couldn't find LAST")
		lasts <- md[lasts]
		lasts <- sub("^\\s*##LAST\\s*=", replacement = "", lasts)
		lasts <- as.numeric(unlist(strsplit(lasts, ",")))
		lasts <- lasts[-length(lasts)]

		factors <- grep("^\\s*##FACTOR\\s*=", md)
		if (factors == 0) stop("Couldn't find FACTOR")
		factors <- md[factors]
		factors <- sub("^\\s*##FACTOR\\s*=", replacement = "", factors)
		factors <- as.numeric(unlist(strsplit(factors, ",")))
		factors <- factors[-length(factors)]
		
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
		
		params <- c(as.numeric(pointsX), as.numeric(pointsR), as.numeric(pointsI),
			firstX, firstR, firstI, lastX, lastR, lastI, factorX, factorR, factorI)
		names(params) <- c("pointsX", "pointsR", "pointsI", "firstX", "firstR", "firstI",
			"lastX", "lastR", "lastI", "factorX", "factorR", "factorI")
			
		if (debug >= 1) {
			message("Extracted parameters:")
			print(params)
			}

		if ((pointsX != pointsR) | (pointsX != pointsI)) stop("No. of frequency, real, imaginary points are not the same")

		} # end of NMR
	
	return(params)
	} # end of extractParams
