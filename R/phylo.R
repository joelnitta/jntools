#' Infer a phylogenetic tree using FastTree
#'
#' Required FastTree to be installed and on user's PATH.
#'
#' @param seqs DNA alignment of class DNAbin
#' @param mol_type Molecule type; either "dna" or "aa" (proteins)
#' @param model Model to use for phylogenetic analysis. Choose
#' "wag" (WAG+CAT), "lg" (LG+CAT), "gtr" (GTR+CAT), "jc" (Jukes-Cantor + CAT)
#' @param gamma Logical; should branch lengths be rescaled and Gamma20-based
#' likelihood calculated?
#' @param echo Logical; should STDERR and STDOUT be printed to the screen?
#' @param ... Additional arguments; not used by this function, but meant for
#' tracking with \code{\link[drake]{drake_plan}}.
#'
#' @return List of class "phylo".
#' @references http://www.microbesonline.org/fasttree/
#'
#' @examples
#' \dontrun{
#' library(ape)
#' data(woodmouse)
#' fasttree(woodmouse)
#' }
#' @export
fasttree <- function (seqs, mol_type = "dna", model = "gtr", gamma = FALSE, echo = FALSE, ...) {

  # Make sure input types are correct
  assertthat::assert_that(inherits(seqs, "DNAbin"),
                          msg = "seqs must be of class DNAbin")
  assertthat::assert_that(is.matrix(seqs),
                          msg = "seqs must be in matrix format (aligned)")
  assertthat::assert_that(assertthat::is.string(mol_type))
  assertthat::assert_that(mol_type %in% c("dna", "aa"),
                          msg = "mol_type must be either 'dna' or 'aa'")
  assertthat::assert_that(assertthat::is.string(model))
  assertthat::assert_that(model %in% c("wag", "lg", "gtr", "jc"),
                          msg = "model must be either 'wag', 'lg', 'gtr', or 'jc'")
  assertthat::assert_that(is.logical(gamma))
  assertthat::assert_that(is.logical(echo))

  # Write out alignment to temp file
  temp_wd <- tempdir()
  ape::write.FASTA(seqs, fs::path(temp_wd, "seqs.fasta"))

  # Modify arguments for processx::run()
  mol_type <- if (mol_type == "dna") "-nt" else NULL
  model <- if (model %in% c("wag", "lg", "gtr")) paste0("-", model) else NULL
  gamma <- if (isTRUE(gamma)) "-gamma" else NULL
  alignment_file <- fs::path(temp_wd, "seqs.fasta")

  args <- c(mol_type,
            model,
            gamma,
            alignment_file)

  # Run command
  results <- processx::run(
    "fasttree",
    args, wd = temp_wd, echo = echo)

  # Convert tree to ape format by writing out then reading in
  readr::write_file(results$stdout, fs::path(temp_wd, "tre.fasta"))
  ape::read.tree(fs::path(temp_wd, "tre.fasta"))
}
