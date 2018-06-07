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
  last_char <- substr(path, nchar(path), nchar(path))
  if (last_char != "/") {
    path <- paste0(path, "/")
  }
  path
}
