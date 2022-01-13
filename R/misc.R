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
      glue::glue("Deleted {sum(too_long)} lines > {max_line_length} chars.")
    )

  # Fix italics
  bib <- stringr::str_replace_all(bib, stringr::fixed("{\\textless}i{\\textgreater}"), stringr::fixed("\\textit{") )
  bib <- stringr::str_replace_all(bib, stringr::fixed("{\\textless}/i{\\textgreater}"), stringr::fixed("}") )

  ### Remove any entries that don't have a key
  # These look like "@article{," where there should be a key betwen the
  # opening curly brace and the comma, e.g., "@article{Hormaza1996,"
  # We need to remove the whole entry though, not just the key.
  missing_key <- stringr::str_detect(bib, "^@.*\\{,") %>% which

  if (length(missing_key) > 1) {
    # Make a vector of all closing brackets
    closing_brackets <- stringr::str_detect(bib, "^\\}$") %>% which

    # Simple function to find the closing bracket for a given opening bracket
    find_closing_bracket <- function(opening_bracket, closing_brackets) {
      diff <- closing_brackets - opening_bracket
      names(diff) <- closing_brackets
      closest_diff <- min(diff[diff > 0])
      as.numeric(names(diff[diff == closest_diff]))
    }

    # Find all the closing brackets for the missing keys
    closing_brackets_for_missing_keys <- sapply(
      missing_key,
      find_closing_bracket,
      closing_brackets = closing_brackets )

    # Convert this to a vector of lines including all the lines for entries
    # missing a key
    entries_without_key <- NULL
    for(i in 1:length(missing_key)) {
      entries_without_key[[i]] <-
        missing_key[[i]]:closing_brackets_for_missing_keys[[i]]
    }
    entries_without_key <- unlist(entries_without_key)

    # Remove the offending lines
    bib <- bib[-entries_without_key]

    message(
      glue::glue("Deleted {length(missing_key)} entries missing bibliography keys.")
    )
  }

  # Make sure the last line preceding a closing bracket ends
  # with a bracket and not a comma.
  last_entry_lines <- which(stringr::str_detect(bib, "^\\}$"))
  last_entry_lines <- last_entry_lines - 1

  bib[last_entry_lines] <-
    stringr::str_replace(bib[last_entry_lines], "\\},$", "\\}")

  bib
}

#' Produce a filtered, cleaned .bib file for an Rmd document
#'
#' Takes a raw .bib file (e.g., one exported from
#' Mendeley with thousands of entries), filters it
#' to only the citations in the Rmd file, and cleans
#' the references so pandoc won't crash.
#'
#' Note that individual entries in the cleaned bib file may still
#' need additional editing.
#'
#' @param rmd_file Path to Rmd file
#' @param raw_bib Path to raw bib file
#' @param final_bib Path to write cleaned, filtered bib file
#' @param exclude Optional character vector of reference IDs to exclude
#' from the final bibliography.
#' @param ... Additional named arguments passed to \code{\link{clean_bib}}
#' that will be used when cleaning the raw bib file.
#'
#' @return Nothing
#' @export
make_ref_list <- function(rmd_file, raw_bib, final_bib, exclude = NULL, ...) {

  # Process manuscript Rmd and pull out citations
  # (words that begin with '@')
  citations <-
    readr::read_lines(rmd_file) %>%
    stringr::str_split(" |;") %>%
    unlist %>%
    magrittr::extract(., stringr::str_detect(., "@")) %>%
    # Remove all extraneous characters to get to just reference ID
    stringr::str_remove_all("\\[|\\]|\\)|\\(|\\.$|,|\\{|\\}") %>%
    stringr::str_remove_all("-@") %>%
    stringr::str_remove_all("@") %>%
    unique %>%
    sort

  # Optionally exclude references from exclude list
  if(!is.null(exclude)) {
    assertthat::assert_that(is.character(exclude))
    citations <- citations[!citations %in% exclude]
  }

  # Read in entire raw bibliography exported from Mendeley as tibble
  bib_df <- bib2df::bib2df(raw_bib)

  # Select only needed columns and subset to citations in Rmd
  bib_df_selected <-
    bib_df %>%
    dplyr::select(CATEGORY, BIBTEXKEY, AUTHOR,
           BOOKTITLE, CHAPTER, EDITOR, EDITION, ADDRESS,
           JOURNAL, PAGES, PUBLISHER, TITLE, VOLUME, YEAR) %>%
    dplyr::filter(BIBTEXKEY %in% citations)

  # Fix formatting of "Jr."
  # This is only working for James E Watkins Jr for now, need to make it work generally
  # Eddie's name entered in Mendeley as (last, first): Watkins Jr., James E
  # For bibtex to properly format it, Jr. should be included in bib file as:
  # Watkins, Jr., James E.
  bib_df_selected <-
    bib_df_selected %>%
    dplyr::mutate(AUTHOR = purrr::map(AUTHOR, ~stringr::str_replace_all(., "^Watkins Jr.\\}", "\\{Watkins Jr.\\}"))) %>%
    dplyr::mutate(AUTHOR = purrr::map(AUTHOR, ~stringr::str_replace_all(., "\\{Watkins Jr.\\}", "Watkins, Jr.")))

  # Fix missing bracket for italics at start of title
  bib_df_selected <-
    bib_df_selected %>%
    dplyr::mutate(TITLE = purrr::map(TITLE, ~stringr::str_replace_all(., "^textless\\}i", "\\{\\\\textless\\}i")))

  # Wrap titles in double {{ }} to preserve capitalization
  bib_df_selected <-
    bib_df_selected %>%
    dplyr::mutate(TITLE = paste0("{", TITLE, "}"))

  bib_df_selected <-
    bib_df_selected %>%
    dplyr::mutate(BOOKTITLE = paste0("{", BOOKTITLE, "}"))

  # Write this out to temporary file so it can be cleaned
  # with jntools::clean_bib(), since that works by
  # reading in an external bib flie.
  temp_file <- tempfile()
  bib2df::df2bib(bib_df_selected, file = temp_file)

  # Do final cleaning with jntools::clean_bib
  jntools::clean_bib(raw_bib_file = temp_file, ...) %>%
    readr::write_lines(final_bib)

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

  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop("Package \"googledrive\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

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
all_duplicated <- function(x) {
  duplicated(x, fromLast = TRUE) | duplicated(x, fromLast = FALSE)
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
  ret <- gsub(paste0("(^",sep,"|",sep,"$)"),"",
             gsub(paste0(sep, sep), sep,
                  do.call(paste, c(L, list(sep = sep)))))
  is.na(ret) <- ret == ""
  ret
}

#' Round a number and include trailing zeros
#'
#' @param x Numeric vector
#' @param digits Number of digits for rounding.
#'
#' @return Character vector
#'
#' @examples
#' round_t(c(2.12, 2.3), 4)
#' @export
round_t <- function(x, digits) {
  assertthat::assert_that(is.numeric(x))
  assertthat::assert_that(assertthat::is.number(digits))
  round(x, digits) %>% sprintf(glue::glue("%.{digits}f"), .)
}

#' Spell out a number in English if it's less than 10
#'
#' @param x Number.
#' @param limit Upper limit of numbers to spell out
#' (default = 10).
#'
#' @return Character vector of length 1.
#'
#' @examples
#' english2(2)
#' english2(12)
#' english2(10)
#' @export
english2 <- function(x, limit = 10) {
  assertthat::assert_that(is.numeric(x))
  if (x < limit) return (english::words(x))
  as.character(x)
}
