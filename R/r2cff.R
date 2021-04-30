#' @title Convert from R DESCRIPTION into CFF
#' @description Converts an R package DESCRIPTION file to Citation File Format
#' @param description_file Path and name of the DESCRIPTION file
#' @param export if `TRUE`, the output is saved as CITATION.cff
#' @return The package's DESCRIPTION file converted to CFF
#' @author Waldir Leoncio
#' @export
r2cff <- function(description_file = "DESCRIPTION", export = FALSE) {
	desc <- readLines(description_file)
	cff <- c("# YAML 1.2", "---")
	for (l in seq_along(desc)) {
		# TODO: transform if-statements into functions
		cff <- append(cff, fetchCFFelement(desc[l], "Title:"))
		cff <- append(cff, fetchCFFelement(desc[l], "Version:"))
	}
	if (!export) {
		return(cat(cff, sep = "\n"))
	} else {
		# TODO: implement saving feature
		stop("Exporting not yet available")
	}
}

fetchCFFelement <- function(string, element_r, element_cff = tolower(element_r)) {
	element_r_nchar <- nchar(element_r)
	if (substr(string, 1, element_r_nchar) == element_r) {
		cff_entry <- paste(element_cff, string) # FIXME: duplicate text
		return(cff_entry)
	}
}
