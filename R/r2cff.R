#' @title Convert from R DESCRIPTION into CFF
#' @description Converts an R package DESCRIPTION file to Citation File Format
#' @param description_file Path and name of the DESCRIPTION file
#' @param export if `TRUE`, the output is saved as CITATION.cff
#' @return The package's DESCRIPTION file converted to CFF
#' @author Waldir Leoncio
#' @export
r2cff <- function(description_file = "DESCRIPTION", export = FALSE) {
	desc <- readLines(description_file)
	cff <- c(
		"# YAML 1.2",
		"---",
		"cff-version: 1.1.0",
		'message: "If you use this software, please cite it using these metadata."'
	)
	for (l in seq_along(desc)) {
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
		element_value <- gsub(paste0(element_r, " "), "", string)
		cff_entry <- paste(element_cff, element_value)
		return(cff_entry)
	}
}
