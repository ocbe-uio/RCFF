#' @title Convert from R DESCRIPTION into CFF
#' @description Converts an R package DESCRIPTION file to Citation File Format
#' @param description_file Path and name of the DESCRIPTION file
#' @param export if `TRUE`, the output is saved as CITATION.cff
#' @return The package's DESCRIPTION file converted to CFF
#' @author Waldir Leoncio
#' @export
r2cff <- function(description_file = "DESCRIPTION", export = FALSE) {
	validateR(description_file)

	# ======================================================== #
	# Creating proto files for CFF and DESCRIPTION             #
	# ======================================================== #
	desc <- readLines(description_file)
	cff  <- readLines("inst/extdata/CITATION-skeleton.cff")

	# ======================================================== #
	# Looping along DESCRIPTION to find CFF elements           #
	# ======================================================== #
	for (l in seq_along(desc)) {
		cff <- append(cff, fetchCFFelement(desc[l], "Title"))
		cff <- append(cff, fetchCFFelement(desc[l], "Version"))
		cff <- append(cff, fetchCFFelement(desc[l], "Date", "date-released"))
	}
	cff <- append(cff, "authors:", )
	for (l in seq_along(desc)) {
		cff <- append(
			cff,
			fetchCFFelement(
				desc[l], "person", "  -", add_colons=FALSE, element_value=""
			)
		)
		cff <- append(
			cff,
			fetchCFFelement(
				desc[l], "given =", "    given-names:", add_colons=FALSE,
				remove_elements=c('"', ",")
			)
		)
		cff <- append(
			cff,
			fetchCFFelement(
				desc[l], "family =", "    family-names:", add_colons=FALSE,
				remove_elements=c('"', ",")
			)
		)
		# FIXME: #8 if role = cph, given-name should be converted to name (organization)
	}
	validateCFF(cff)

	# ======================================================== #
	# Returning CFF file                                       #
	# ======================================================== #
	if (!export) {
		return(cat(cff, sep = "\n"))
	} else {
		exportCFF(cff)
	}
}

fetchCFFelement <- function(string, element_r, element_cff = tolower(element_r),
add_colons=TRUE, element_value=gsub(paste0(element_r, " "), "", string),
remove_elements=NULL) {
	# ======================================================== #
	# Adding colons if necessary                               #
	# ======================================================== #
	if (add_colons){
		element_r <- addColon(element_r)
		element_cff <- addColon(element_cff)
	}

	# ======================================================== #
	# Removing indentation                                     #
	# ======================================================== #
	string <- gsub("\t", "", string) # TODO: make this also work with space indentation

	# ======================================================== #
	# Pasting CFF element (if found)                           #
	# ======================================================== #
	element_r_nchar <- nchar(element_r)
	if (substr(string, 1, element_r_nchar) == element_r) {
		cff_entry <- paste(element_cff, element_value)
		if (!is.null(remove_elements)) {
			for (e in remove_elements) {
				cff_entry <- gsub(e, '', cff_entry)
			}
		}
		return(cff_entry)
	}
}

addColon <- function(txt) {
	if (substring(txt, nchar(txt)) != ":") txt <- paste0(txt, ":")
	return(txt)
}

exportCFF <- function(infile, outfile="CITATION.cff") {
	outfile <- "CITATION.cff"
	if (file.exists(outfile)) {
		outfile -> outfile_old
		outfile <- tempfile(pattern="CITATION_", tmpdir="", fileext=".cff")
		outfile <- gsub(pattern="/", replacement="", x=outfile)
		message(outfile_old, " already exists. Saving as ", outfile)
	}
	out_file <- file(outfile)
	writeLines(infile, out_file)
	close(out_file)
}

validateR <- function(r_file) {
	if (!file.exists(r_file)) {
		stop(r_file, " file not found on the provided file path.")
	}

}

validateCFF <- function(cff_file) {
	required_fields <- data.frame(
		cff = c("authors", "date-released", "title", "version"),
		r = c("person", "Date", "Title", "Version")
	)
	for (f in required_fields$cff) {
		found_f <- grepl(pattern=f, x=cff_file)
		if (!any(found_f)) {
			r_equivalent <- required_fields$r[match(f, required_fields$cff)]
			warning(
				f, " not found. It is a CFF 1.1.0 required field.\n",
				"Please add a '", r_equivalent, "' field to your input file."
			)
		}
	}
}