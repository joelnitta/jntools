context("DNA sequence manipulation")

# flatten_DNA_list --------------------------------------------------------

cat(">seq1",
    "ATCG",
    ">seq2",
    "GATC",
    ">seq2",
    "ATTG",
    file = tempfile("fasta1", fileext = ".fasta"), sep = "\n")

cat(">seq4",
    "ATCG",
    ">seq5",
    "GATC",
    ">seq6",
    "ATTG",
    file = tempfile("fasta2", fileext = ".fasta"), sep = "\n")

files <- list.files(tempdir(), pattern = "fasta", full.names = TRUE)
nested_seqs <- lapply(files, ape::read.FASTA)
improper_seqs <- nested_seqs
improper_seqs[[3]] <- "a"

test_that("Only accepts lists", {
  expect_error(flatten_DNA_list("a"), "dna_list is not a list")
  expect_error(flatten_DNA_list(1), "dna_list is not a list")
})

test_that("Only accepts lists of DNA sequences", {
  expect_error(flatten_DNA_list(improper_seqs),
               "All elements of dna_list must be of class DNAbin")
})

test_that("Returns DNA sequences", {
  expect_is(flatten_DNA_list(nested_seqs), "DNAbin")
})
