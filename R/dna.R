# DNA sequence manipulation -----------------------------------------------

#' flatten_DNA_list
#'
#' Flatten a list of lists of DNA sequences into a list of DNA sequences
#'
#' This comes in handy if \code{ape::read.FASTA()} is mapped onto
#' a vector of fasta files, where each fasta file may contain multiple
#' sequences.
#'
#' @param dna_list List of objects of class \code{"DNAbin"}.
#'
#' @return Single object of class \code{"DNAbin"}.
#' @examples
#' cat(">seq1",
#'     "ATCG",
#'     ">seq2",
#'     "GATC",
#'     ">seq2",
#'     "ATTG",
#'    file = tempfile("fasta1", fileext = ".fasta"), sep = "\n")
#'
#' cat(">seq4",
#'     "ATCG",
#'     ">seq5",
#'     "GATC",
#'     ">seq6",
#'     "ATTG",
#'    file = tempfile("fasta2", fileext = ".fasta"), sep = "\n")
#'
#' files <- list.files(tempdir(), pattern = "fasta", full.names = TRUE)
#'
#' nested_seqs <- lapply(files, ape::read.FASTA)
#'
#' flat_seqs <- flatten_DNA_list(nested_seqs)

#' @export
flatten_DNA_list <- function (dna_list) {
  assertthat::assert_that(is.list(dna_list))
  assertthat::assert_that(
    all(lapply(dna_list, class) == "DNAbin"),
    msg = "All elements of dna_list must be of class DNAbin")
  dna_list <- lapply(dna_list, as.character)
  dna_list <- unlist(dna_list, recursive = FALSE)
  ape::as.DNAbin(dna_list)
}
