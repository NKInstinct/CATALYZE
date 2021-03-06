% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sce_prepare.R
\name{sce_prepare_inputs}
\alias{sce_prepare_inputs}
\title{Prepare input data for the CATALYST pipeline}
\usage{
sce_prepare_inputs(parent_dir, pattern, recursive, sampleIDs, reference)
}
\arguments{
\item{parent_dir}{Directory where the fcs files used to build the sce object
can be found. Note that since I exclusively use CATALYST on
infinityflow-processed data, this function does not curretly support
transforming or compensating the input fcs data, since that is handled by
infinityflow directly. May update in the future if this is needed, but in
that case it's probably better to have a second function that can take a
gatingset as input and return an sce (and so all this is handled in the
gatingset creation steps).}

\item{pattern}{String or regexp to match within parent_dir to find the .fcs
files needed to build the sce.}

\item{recursive}{Boolean. Should .fcs files be searched for recursively?}

\item{sampleIDs}{Named character vector where each name should be a unique
identifier for a sample (in the same order that list.files(parent_dir,
pattern, recursive) would give you), and each entry is the condition group
that those samples belong to. See examples below.}

\item{reference}{String that must match one of the groups from sampleIDs.
Which condition group from sampleIDs is the reference group?}
}
\value{
A list containing the flowSet, panel, and metadata files. Note that
the marker class in the panel MUST STILL BE EDITED BY HAND before
proceeding!!!
}
\description{
Prepare input data for the CATALYST pipeline
}
\examples{
path_to_fcs <- system.file("extdata", package = "CATALYZE")

inputs <- sce_prepare_inputs(path_to_fcs,
                             pattern = "Vitro",
                             recursive = FALSE,
                             sampleIDs = c("Vitro1" = "GroupA",
                                           "Vitro2" = "GroupB"),
                             reference = "GroupA")

inputs$panel <- tibble::tibble(inputs$panel,
                       c(NA_character_,
                         NA_character_,
                         NA_character_,
                         NA_character_,
                         NA_character_,
                         NA_character_,
                         "type",
                         "state",
                         "state",
                         "type",
                         "state",
                         NA_character_))

}
