# I/O ---------------------------------------------------------------------

#' Clean up a latex bibliography file
#'
#' In particular one exported from Mendeley
#'
#' @param raw_bib_file Path to raw .bib file; this function is designed in particular
#' to clean .bib files produced by Mendeley.
#' @param max_line_length Integer. Lines greater than this length will be removed from the
#' bibliography.
#' @param strip_fields Fields to remove from the bibliography. Note that "number"
#' is issue number.
#' @param strip_mendeley_header Logical; should the header added by Mendeley be removed?
#'
#' @return Character vector.
#' @examples
#' \dontrun{
#' # Clean bibliography.
#' clean_bib <- clean_bib("mendeley_refs.bib")
#' # Write out cleaned bibliography.
#' write_lines(clean_bib, "references.bib")
#' }
#' @export
clean_bib <- function (raw_bib_file,
                       max_line_length = 1000,
                       strip_fields = c("abstract",
                                        "file",
                                        "keywords",
                                        "url",
                                        "doi",
                                        "issn",
                                        "isbn",
                                        "month",
                                        "number"),
                       strip_mendeley_header = TRUE) {

  # Check input
  assertthat::assert_that(assertthat::is.readable(raw_bib_file))
  if(!is.null(max_line_length)) assertthat::assert_that(is.numeric(max_line_length))
  if(!is.null(strip_fields)) assertthat::assert_that(is.character(strip_fields))
  assertthat::assert_that(is.logical(strip_mendeley_header))

  # Read raw bib file
  bib <- readr::read_lines(raw_bib_file)

  # Strip mendeley header.
  mendeley_header <-
    stringr::str_detect(
      bib, "^Automatically generated|^Any changes to|^BibTeX export options")

  if(isTRUE(strip_mendeley_header)) bib <- bib[!mendeley_header]

  # Strip un-needed fields which can get too long to read in properly.
  strip_fields_pattern <- glue::glue("^{strip_fields}")
  strip_fields_pattern <- paste(strip_fields_pattern, collapse = "|")
  remove <- stringr::str_detect(bib, strip_fields_pattern)

  if(!is.null(strip_fields)) bib <- bib[!remove]

  # After removing these lines, we may have some dangling lines that were
  # actually part of a bib field (e.g., an abstract with multiple paragraphs).
  #
  # Only keep lines that are proper bibtex fields:
  # - beginning of an entry starting with `@`
  # - data field in an entry including `=`
  # - end of entry, marked by a single curly brace
  keep <- stringr::str_detect(bib, "^@| = |^\\}$")

  bib <- bib[keep]

  # Delete lines exceeding the maximum length, and print a
  # warning about those that got cut.
  too_long <- nchar(bib) > max_line_length

  if (!is.null(max_line_length)) bib <- bib[!too_long]

  if (sum(too_long) > 1)
    print(
      glue::glue("Deleted {sum(too_long)} lines > {max_length} chars.")
    )

  # Fix italics
  bib <- stringr::str_replace_all(bib, stringr::fixed("{\\textless}i{\\textgreater}"), stringr::fixed("\\textit{") )
  bib <- stringr::str_replace_all(bib, stringr::fixed("{\\textless}/i{\\textgreater}"), stringr::fixed("}") )

  # Make sure the last line preceding a closing bracket ends
  # with a bracket and not a comma.
  last_entry_lines <- which(stringr::str_detect(bib, "^\\}$"))
  last_entry_lines <- last_entry_lines - 1

  bib[last_entry_lines] <-
    stringr::str_replace(bib[last_entry_lines], "\\},$", "\\}")

  bib
}

#' Download all the files in a google drive folder.
#'
#' \code{dribble_data} should be obtained using \code{\link[googledrive]{drive_ls}}.
#'
#' @param dribble_data Data frame of class \code{dribble}.
#' @param folder Path to local folder to download data.
#' @param pattern Optional grep pattern; only file names matching this pattern
#' will be included.
#' @param negate Logical; should only file names that don't match the pattern
#' be included instead?
#' @param overwrite Logical; should existing files be overwritten?
#' @param ... Other arguments; not used by this function, but meant for workflow tracking with drake.
#'
#' @return Data frame of class dribble including the files that were downloaded
#'
#' @examples
#' \dontrun{
#' library(tidyverse)
#' library(googledrive)
#' # Make a temporary drive folder
#' folder <- drive_mkdir("temp")
#' # Upload example data
#' files <- map(c(drive_example("chicken.csv"), drive_example("chicken.txt")),
#'             drive_upload, path = folder)
#' # Download all files in the folder
#' folder_contents <- drive_ls("temp")
#' download_all_files_in_folder(folder_contents, tempdir(), overwrite = TRUE)
#' }
#' @export
download_all_files_in_folder <- function (dribble_data, folder, pattern = NULL,
                                         negate = FALSE, overwrite = FALSE, ...) {

  # Error-checking
  assertthat::assert_that(assertthat::is.dir(folder))
  assertthat::assert_that(is.logical(negate))
  assertthat::assert_that(any(grepl("dribble", class(dribble_data))),
                          msg = "dribble must of class dribble")

  # Filter out folders
  dribble_data <- dplyr::mutate(
    dribble_data,
    mimeType = purrr::map_chr(drive_resource, "mimeType")
  )

  dribble_data <- dplyr::filter(
    dribble_data,
    mimeType != "application/vnd.google-apps.folder"
  )

  # Filter files to download based on pattern
  if(!is.null(pattern)) assertthat::assert_that(assertthat::is.string(pattern))

  if (!is.null(pattern) & !isTRUE(negate)) {
    dribble_data <- dplyr::filter(
      dribble_data,
      grepl(pattern, dribble_data$name)
    )
  }

  if (!is.null(pattern) & isTRUE(negate)) {
    dribble_data <- dplyr::filter(
      dribble_data,
      !grepl(pattern, dribble_data$name)
    )
  }

  assertthat::assert_that(nrow(dribble_data) > 0,
                          msg = "No data to download under these arguments")

  # Download non-folders and anything passing filter
  purrr::walk2(
    purrr::map(dribble_data$id, googledrive::as_id),
    fs::path(folder, dribble_data$name),
    googledrive::drive_download,
    overwrite = overwrite
  )

  # Return the filtered data that was downloaded
  dribble_data
}

#' add_slash
#'
#' Add a trailing slash to a path.
#'
#' Checks if a path has a trailing slash and adds one if not.
#'
#' @param path Character vector of length one; the path to which to add a slash.
#' @return The path with a slash appended.
#' @examples
#' add_slash("path/wihout/a/trailing_slash")
#' add_slash("path/already/has/a/trailing_slash/")
#' @author Joel H Nitta, \email{joelnitta@@gmail.com}
#' @export
add_slash <- function (path) {

  if (!(is.character(path))) {stop ("path must be a character vector")}

  last_char <- substr(path, nchar(path), nchar(path))
  if (last_char != "/") {
    path <- paste0(path, "/")
  }
  path
}

# Dataframes --------------------------------------------------------------

#' all_duplicated
#'
#' Find all duplicate values
#'
#' Similar to base::duplicated(), but returns a logical vector
#' indicating all duplicate values (not just duplicates after
#' the first).
#'
#' @param x Vector or dataframe or array.
#' @return For vectors, a logical vector indicating which elements are duplicated.
#' For dataframes, a logical vector indicating which rows are duplicated.
#' @examples
#' all_duplicated(c("a","a","b"))
#' all_duplicated(data.frame(c("a","a","b", "d"), c("a","a","c", "e")))
#' @author Joel H Nitta, \email{joelnitta@@gmail.com}
#' @export
all_duplicated <- function (x) {
  duplicated(x, fromLast = TRUE) | duplicated(x, fromLast = FALSE)
}

#' resolve_col
#'
#' Find all duplicate values
#'
#' Resolve duplicates within a single column of a dataframe
#' that should only contain a unique value. To be used internally
#' with \code{\link{resolve_duplicates}}
#'
#' @param col List (supposed to be single column of a dataframe).
#' @param collapse_chr Character to use to separate values when pasting together elements
#' of items in the list that contain > 1 unique element.
#' @return List containing a single element per list item.
#' @author Joel H Nitta, \email{joelnitta@@gmail.com}
resolve_col <- function (col, collapse_chr = ", ") {
  nonNA_vals <- unique(col[!is.na(col)])
  # if only one unique value, return that
  if (length(nonNA_vals) == 1) { nonNA_vals }
  # if multiple, collapse and return
  else if (length(nonNA_vals) > 1) { paste(nonNA_vals, collapse = collapse_chr) }
  else {NA}
}

#' resolve_df
#'
#' Resolve duplicates across a dataframe
#'
#' To be used internally with \code{\link{resolve_duplicates}}
#'
#' @param df The dataframe to which \code{\link{resolve_col}} should be applied.
#' @param collapse_chr Character to use to separate values when pasting together elements
#' of items in the list that contain > 1 element.
#' @return Dataframe
resolve_df <- function (df, collapse_chr) {purrr::map_df (df, resolve_col, collapse_chr = collapse_chr)}

#' resolve_duplicates
#'
#' Resolve duplicate values in a dataframe by combining rows.
#'
#' Unlike base::unique(), which returns unique rows taking into account all columns,
#' resolve_duplicates will only check for duplicate values in the columns specified by
#' \code{search_col}. Conflicting values in columns not included in \code{search_col},
#' are resolved by pasting together separated by \code{collapse_chr}.
#'
#' @param data A dataframe
#' @param search_col Vector of column name(s) of the dataframe to check for
#' duplicate values.
#' @param collapse_chr Character used to separate duplicate values when combining cells
#'
#' @return A dataframe with duplicated rows removed and conflicting values pasted together.
#'
#' @examples
#' df <- data.frame(list(C1 = c("a","a","b","c","c","d","d"),
#'                      C2 = c("b","d","e","f","g","h","j"),
#'                      C3 = c("h","h","i","j","k","e","e")),
#'                  stringsAsFactors = FALSE)
#'
#' unique(df) # doesn't do anything because df already unique
#'            # when using all columns
#' resolve_duplicates(df, "C1")
#' resolve_duplicates(df, "C3")
#' resolve_duplicates(df, c("C1", "C3"))
#' resolve_duplicates(df, "C2")
#' @export
resolve_duplicates <- function (data, search_col, collapse_chr = ", ") {

  # Subset on search columns
  search_data <- data[colnames(data) %in% search_col]

  # If no dups, nothing to do
  if (sum(all_duplicated(search_data)) == 0) {
    print ("No duplicate values; returning original dataframe")
    data
  }

  # Find all duplicates of interest
  dups <- data[all_duplicated(search_data), ]

  # Remove these from data
  data <- data[!(all_duplicated(search_data)), ]

  # Resolve duplicates
  # split duplicates into list based on search column
  if (length(search_col) >1 ) {
    dups <- split(dups, f = apply(dups[,colnames(dups) %in% search_col], 1, paste, collapse="_"))
  } else {
    dups <- split(dups, f = dups[[search_col]])
  }
  # and resolve
  resolved <- purrr::map_df(dups, resolve_df, collapse_chr)

  # Merge now-resolved dups back in
  data <- dplyr::bind_rows(data, resolved)

  # Check that dups are gone
  if (nrow(data) != nrow(unique(data[colnames(data) %in% search_col]))) {
    stop ("Failed to resolve duplicates in search_col")
  } else {
    return (data)
  }
}

#' paste3
#'
#' Paste while removing NAs
#'
#' Removes NAs from pasted elements, but if ALL elements are NA, the result is NA.
#'
#' Shamelessly copied from
#' \url{https://stackoverflow.com/questions/13673894/suppress-nas-in-paste}
#' @param ... Strings to paste
#' @param sep Character used to separate pasted strings
#' @examples
#' paste3(c("a", "b", "c", NA), c("A","B", NA, NA))
#' @export
paste3 <- function(..., sep=" ") {
  L <- list(...)
  L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
  ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
             gsub(paste0(sep,sep),sep,
                  do.call(paste,c(L,list(sep=sep)))))
  is.na(ret) <- ret==""
  ret
}
