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

#' Run IQ-TREE
#'
#' For details, see http://www.iqtree.org/doc/
#'
#' @param alignment DNA alignment to use for phylogenetic analysis. Must be matrix
#' (i.e., aligned sequences) of class DNAbin
#' @param aln_path Path to DNA alignment. Either alignment, or aln_path must be provided,
#' but not both
#' @param tree_path Optional; path to tree when it is written out by IQ-TREE, useful
#' if this differs from default alignment name.
#' @param wd Path to working directory. The alignment and IQ-TREE intermediate files
#' and results will be written here.
#' @param bb Optional; number of ultrafast bootstrap replicates to run.
#' @param nt Optional; number of cores to use. Set to "AUTO" to determine automatically.
#' @param alrt Optional; number of SH-aLRT tests to run.
#' @param m Optional; specify model. If no model is given, ModelTest will be run
#' to identify the best model for the data.
#' @param redo Logical; should the analysis be redone from scratch if output from
#' previous runs is present?
#' @param spp Path to partition file.
#' @param seed Optional; Specify a random number seed to reproduce a previous run.
#' @param echo Logical; should STDERR be written to the screen?
#' @param ... Other arguments not used by this function but used by
#' drake for tracking.
#'
#' @return Phylogenetic tree (list of class "phylo")
#'
#' @examples
#' \dontrun{
#' library(ape)
#' data(woodmouse)
#' # Rapid boot-strap tree with 1000 replicates on best-fitting model
#' tree <- iqtree(woodmouse, tempdir(), bb = 1000, echo = TRUE)
#' plot(tree)
#' # Check the optimum number of cores to use for GTR+I+G model
#' iqtree(tempdir(), woodmouse, m = "GTR+I+G", nt = "AUTO", echo = TRUE, redo = TRUE)
#' }
#' @export
iqtree <- function (alignment = NULL, wd = getwd(),
                    aln_path = NULL,
                    tree_path = NULL,
                    bb = NULL, nt = NULL, alrt = NULL, m = NULL, redo = FALSE,
                    spp = NULL,
                    seed = NULL,
                    echo = FALSE, ...) {

  assertthat::assert_that(
    !is.null(alignment) | !is.null(aln_path),
    msg = "Either alignment or aln_path must be provided, but not both")

  assertthat::assert_that(
    is.null(alignment) | is.null(aln_path),
    msg = "Either alignment or aln_path must be provided, but not both")

  assertthat::assert_that(assertthat::is.dir(wd))

  assertthat::assert_that(is.logical(echo))

  assertthat::assert_that(is.logical(redo))

  if(!is.null(bb))
    assertthat::assert_that(assertthat::is.number(bb))

  if(!is.null(alrt))
    assertthat::assert_that(assertthat::is.number(alrt))

  if(!is.null(nt))
    assertthat::assert_that(assertthat::is.number(nt) | assertthat::is.string(nt))

  if(!is.null(m))
    assertthat::assert_that(assertthat::is.string(m))

  if(!is.null(spp))
    assertthat::assert_that(assertthat::is.readable(spp))

  if(!is.null(seed))
    assertthat::assert_that(assertthat::is.number(seed))

  wd <- fs::path_norm(wd)

  # check that iqtree is installed and on the PATH
  tryCatch({
    processx::run("iqtree", "-h", echo = FALSE)
  }, warning = function(w) {
    stop("iqtree not installed and on path")
  }, error = function(e) {
    stop("iqtree not installed and on path")
  }, finally = {
    TRUE
  })

  # Write alignment to working directory in phylip format if alignment
  # is provided via R as DNAbin
  if (is.null(aln_path)) {
    assertthat::assert_that(inherits(alignment, "DNAbin"),
                            msg = "alignment must be of class 'DNAbin'")
    assertthat::assert_that(is.matrix(alignment),
                            msg = "alignment must be a matrix (not a list of unaligned sequences)")

    aln_path <- fs::path(wd, deparse(substitute(alignment))) %>%
      fs::path_ext_set("phy")

    phangorn::write.phyDat(alignment, aln_path, format = "phylip")
  }

  assertthat::assert_that(assertthat::is.readable(aln_path))

  # Set up arguments
  iqtree_arguments <- c(
    "-s", fs::path_abs(aln_path),
    if(!is.null(bb)) "-bb",
    bb,
    if(!is.null(alrt)) "-alrt",
    alrt,
    if(!is.null(nt)) "-nt",
    nt,
    if(!is.null(m)) "-m",
    m,
    if(!is.null(seed)) "-seed",
    seed,
    if(!is.null(spp)) "-spp",
    fs::path_abs(spp),
    if(isTRUE(redo)) "-redo"
  )

  # Run iqtree command
  processx::run(
    "iqtree",
    iqtree_arguments, wd = wd, echo = echo)

  # Read in resulting consensus tree.
  # Use default name of .phy file if tree_path not provided
  if(is.null(tree_path)) {
    tree_path <- fs::path(wd, deparse(substitute(alignment))) %>%
      fs::path_ext_set(".phy.contree")
  }

  assertthat::assert_that(assertthat::is.readable(tree_path))

  ape::read.tree(tree_path)

}
