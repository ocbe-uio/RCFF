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
		if (substr(desc[l], 1, 5) == "Title") {
			cff <- append(cff, paste("title:", desc[l])) # FIXME: duplicate text
		}
		if (substr(desc[l], 1, 7) == "Version") {
			cff <- append(cff, paste("version:", desc[l])) # FIXME: dupe text
		}
	}
	if (!export) {
		return(cat(cff, sep = "\n"))
	} else {
		# TODO: implement saving feature
		stop("Exporting not yet available")
	}
}