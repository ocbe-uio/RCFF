#' @title Convert from R DESCRIPTION into CFF
#' @description Converts an R package DESCRIPTION file to Citation File Format
#' @param description_file Path and name of the DESCRIPTION file
#' @param export if `TRUE`, the output is saved as CITATION.cff
#' @return The package's DESCRIPTION file converted to CFF
#' @author Waldir Leoncio
#' @export
r2cff <- function(description_file = "DESCRIPTION", export = FALSE) {
	# ======================================================== #
	# Creating proto files for CFF and DESCRIPTION             #
	# ======================================================== #
	desc <- readLines(description_file)
	cff <- c(
		"# YAML 1.2",
		"---",
		"cff-version: 1.1.0",
		'message: "If you use this software, please cite it using these metadata."'
	)

	# ======================================================== #
	# Looping along DESCRIPTION to find CFF elements           #
	# ======================================================== #
	for (l in seq_along(desc)) {
		cff <- append(cff, fetchCFFelement(desc[l], "Title"))
		cff <- append(cff, fetchCFFelement(desc[l], "Version"))
		cff <- append(cff, fetchCFFelement(desc[l], "Date", "date-released"))
	}

	# TODO: add validation that throws warning when required CFF element is NA

	# ======================================================== #
	# Returning CFF file                                       #
	# ======================================================== #
	if (!export) {
		return(cat(cff, sep = "\n"))
	} else {
		# TODO: #5 implement saving feature
		stop("Exporting not yet available")
	}
}

fetchCFFelement <- function(string, element_r, element_cff = tolower(element_r)) {
	# ======================================================== #
	# Adding colons if necessary                               #
	# ======================================================== #
	element_r <- addColon(element_r)
	element_cff <- addColon(element_cff)
	element_r_nchar <- nchar(element_r)

	# ======================================================== #
	# Pasting CFF element (if found)                           #
	# ======================================================== #
	if (substr(string, 1, element_r_nchar) == element_r) {
		element_value <- gsub(paste0(element_r, " "), "", string)
		cff_entry <- paste(element_cff, element_value)
		return(cff_entry)
	}
}

addColon <- function(txt) {
	if (substring(txt, nchar(txt)) != ":") txt <- paste0(txt, ":")
	return(txt)
}
