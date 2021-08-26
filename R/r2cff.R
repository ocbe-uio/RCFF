#' @title Convert from R DESCRIPTION into CFF
#' @description Converts an R package DESCRIPTION file to Citation File Format
#' @param description_file Path and name of the DESCRIPTION file
#' @param export if `TRUE`, the output is saved as CITATION.cff
#' @return The package's DESCRIPTION file converted to CFF
#' @author Waldir Leoncio
#' @export
#' @examples
#' massRdesc <- system.file("extdata", "DESCRIPTION-MASS", package="RCFF")
#' r2cff(massRdesc)
#' @importFrom desc desc
r2cff <- function(description_file = "DESCRIPTION", export = FALSE) {
	validateR(description_file)

	# ======================================================== #
	# Creating proto files for CFF and DESCRIPTION             #
	# ======================================================== #
	desc     <- desc::desc(description_file)
	cff_path <- system.file("extdata", "CITATION-skeleton.cff", package="RCFF")
	cff      <- readLines(cff_path)

	# ======================================================== #
	# Looping along DESCRIPTION to find CFF elements           #
	# ======================================================== #
	cff <- append2cff(cff, desc, "Title")
	cff <- append2cff(cff, desc, "Version")
	cff <- append2cff(cff, desc, "Date","date-released")
	cff <- append(cff, "authors:", )
	authors <- desc$get_authors()
	processed_author <- unlist(lapply(authors, processAuthor))
	cff <- append(cff, processed_author)
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

append2cff <- function(cff, desc, field, cff_field = tolower(field)) {
	# Finds a field in R DESCRIPTION and appends it to the CFF file
	value <- desc$get(field)
	if (!is.na(value)) {
		cff_txt <- paste(cff_field, ": ", value, collapse = "")
		cff <- append(cff, cff_txt)
	}
	return(cff)
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

processAuthor <- function(author) {
	author <- as.character(author) # it comes as "person" class
	roles <- gsub(".+\\[(.+)\\]$", "\\1", author)
	if (grepl("cph", roles)) {
		# Assumes "cph" belongs to an organization (see ?person for reason)
		name <- gsub("\\s\\[.+$", "", author)
		person_out <- paste(" - name:", name)
	} else {
		author_split <- strsplit(author, " ")[[1]]
		given_name   <- author_split[1]
		family_name  <- author_split[2]
		person_out <- c(
			paste(" - family-names:", family_name),
			paste("   given-names:", given_name)
		)
	}
	return(person_out)
}