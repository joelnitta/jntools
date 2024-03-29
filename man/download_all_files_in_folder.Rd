% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{download_all_files_in_folder}
\alias{download_all_files_in_folder}
\title{Download all the files in a google drive folder.}
\usage{
download_all_files_in_folder(
  dribble_data,
  folder,
  pattern = NULL,
  negate = FALSE,
  overwrite = FALSE,
  ...
)
}
\arguments{
\item{dribble_data}{Data frame of class \code{dribble}.}

\item{folder}{Path to local folder to download data.}

\item{pattern}{Optional grep pattern; only file names matching this pattern
will be included.}

\item{negate}{Logical; should only file names that don't match the pattern
be included instead?}

\item{overwrite}{Logical; should existing files be overwritten?}

\item{...}{Other arguments; not used by this function, but meant for workflow tracking with drake.}
}
\value{
Data frame of class dribble including the files that were downloaded
}
\description{
\code{dribble_data} should be obtained using \code{\link[googledrive]{drive_ls}}.
}
\examples{
\dontrun{
library(tidyverse)
library(googledrive)
# Make a temporary drive folder
folder <- drive_mkdir("temp")
# Upload example data
files <- map(c(drive_example("chicken.csv"), drive_example("chicken.txt")),
            drive_upload, path = folder)
# Download all files in the folder
folder_contents <- drive_ls("temp")
download_all_files_in_folder(folder_contents, tempdir(), overwrite = TRUE)
}
}
