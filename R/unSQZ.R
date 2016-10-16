##'
##' @rdname unDIF
##'
##' @noRd

unSQZ <- function(string) {
	string <- gsub("@", "0", string)
	string <- gsub("A", "1", string)
	string <- gsub("B", "2", string)
	string <- gsub("C", "3", string)
	string <- gsub("D", "4", string)
	string <- gsub("E", "5", string)
	string <- gsub("F", "6", string)
	string <- gsub("G", "7", string)
	string <- gsub("H", "8", string)
	string <- gsub("I", "9", string)
 	string <- gsub("a", "-1", string)
	string <- gsub("b", "-2", string)
	string <- gsub("c", "-3", string)
	string <- gsub("d", "-4", string)
	string <- gsub("e", "-5", string)
	string <- gsub("f", "-6", string)
	string <- gsub("g", "-7", string)
	string <- gsub("h", "-8", string)
	string <- gsub("i", "-9", string)
	return(string)	
	}
	
