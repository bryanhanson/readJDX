#'
#' Split a Multiblock JCAMP-DX File into Individual Files
#'
#' Process a multiblock JCAMP-DX file into separate files, one for each data set.
#'
#' @section Warning:
#' \pkg{Individual data sets are written into the current directory if you set \code{subdir}
#' to \code{NULL}, so you may want to work on a copy of the data in this case}.
#'
#' @param file Length one character giving the name of the multiblock file.
#' @param subdir Character.  If not \code{NULL}, a subdirectory by this name will be 
#'        created in the working directory, and the individual files will be placed
#'        in this subdirectory.  If the subdirectory already exists, an error will be thrown.
#'
#' @return A list is returned invisibly that has one DX data set per list element.  \pkg{Individual files
#'        are written into the current directory if \code{subdir = NULL}, so you probably want
#'        to work on a copy of the data
#'        in this case}.  File names are extracted from the
#'        \code{##TITLE= sample_name} field and will be \code{sample_name} (more precisely,
#'        the name will be whatever follows \code{##TITLE=} on the line).  If
#'        sample names are duplicated the output files will be overwritten as the file
#'        is processed (with a warning).  In this case you should open the multiblock file in a plain text
#'        editor and search for \code{##TITLE=}, then edit the names to be unique before
#'        running this function.  Also, you should ensure that \code{sample_name} will
#'        result in a valid file name on your operating system.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords file
#' @keywords utilities
#'
#' @export
#'
splitMultiblockDX <- function(file = NULL, subdir = "data_sets") {

  if (is.null(file)) stop("You must provide a file name")
  if (length(file) > 1) stop("Please give a single file name")

  lines <- readLines(file)

  # Find blocks in file
  block_pat <- "##BLOCKS=(.*)"
  bc <- grep(block_pat, lines)
  bc <- sub(block_pat, "\\1", lines[bc])
  bc <- as.integer(bc)

  st_pat <- "##TITLE="
  st <- grep(st_pat, lines)
  st <- st[-1] # drop the first one, it is the LINK block
  
  end_pat <- "##END="
  end <- grep(end_pat, lines)

  if (length(st) != length(end)) stop("Block starts and stops did not match")
  nb <- length(st)
  if (nb != bc) stop("Block count in link block did not match the number of blocks found")

  # Place each block into a list element
  blocks <- vector("list", nb)
  fnames <- rep(NA_character_, nb)
  for (i in 1:nb) {
    blocks[[i]] <- (lines[st[i]:end[i]])
    fnames[i] <- lines[st[i]]
  }

  # Fix up names
  name_pat <- paste(st_pat, "(.*)", sep = "")
  fnames <- gsub(st_pat, "\\1", fnames)
  if (anyDuplicated(fnames)) {
    warning("Duplicated sample names found.\n\t\tLater samples will overwrite earlier samples\n\t\tunless you edit the original multiblock file.")
  }
  names(blocks) <- fnames

  # Create a subdirectory if requested & add to file names
  if (!is.null(subdir)) {
    if (dir.exists(subdir)) stop("subdir already exists!")
    dir.create(subdir)
    fnames <- paste(subdir, fnames, sep = "/")
  }


  # Write it out
  for (i in 1:nb) writeLines(blocks[[i]], paste(fnames[i], "dx", sep = "."))

  invisible(blocks)
}
