# I/O ---------------------------------------------------------------------

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
resolve_df <- function (df, collapse_chr) {purrr::map_df (df, jntools:::resolve_col, collapse_chr = collapse_chr)}

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
  if (sum(jntools:::all_duplicated(search_data)) == 0) {
    print ("No duplicate values; returning original dataframe")
    data
  }

  # Find all duplicates of interest
  dups <- data[jntools:::all_duplicated(search_data), ]

  # Remove these from data
  data <- data[!(jntools:::all_duplicated(search_data)), ]

  # Resolve duplicates
  # split duplicates into list based on search column
  if (length(search_col) >1 ) {
    dups <- split(dups, f = apply(dups[,colnames(dups) %in% search_col], 1, paste, collapse="_"))
  } else {
    dups <- split(dups, f = dups[[search_col]])
  }
  # and resolve
  resolved <- purrr::map_df(dups, jntools:::resolve_df, collapse_chr)

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
#'
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
