#' Prepare input data for the CATALYST pipeline
#'
#' @param parent_dir Directory where the fcs files used to build the sce object
#'   can be found. Note that since I exclusively use CATALYST on
#'   infinityflow-processed data, this function does not curretly support
#'   transforming or compensating the input fcs data, since that is handled by
#'   infinityflow directly. May update in the future if this is needed, but in
#'   that case it's probably better to have a second function that can take a
#'   gatingset as input and return an sce (and so all this is handled in the
#'   gatingset creation steps).
#' @param pattern String or regexp to match within parent_dir to find the .fcs
#'   files needed to build the sce.
#' @param recursive Boolean. Should .fcs files be searched for recursively?
#' @param sampleIDs Named character vector where each name should be a unique
#'   identifier for a sample (in the same order that list.files(parent_dir,
#'   pattern, recursive) would give you), and each entry is the condition group
#'   that those samples belong to. See examples below.
#' @param reference String that must match one of the groups from sampleIDs.
#'   Which condition group from sampleIDs is the reference group?
#'
#' @return A list containing the flowSet, panel, and metadata files. Note that
#'   the marker class in the panel MUST STILL BE EDITED BY HAND before
#'   proceeding!!!
#' @examples
#' path_to_fcs <- system.file("extdata", package = "CATALYZE")
#'
#' inputs <- sce_prepare_inputs(path_to_fcs,
#'                              pattern = "Vitro",
#'                              recursive = FALSE,
#'                              sampleIDs = c("Vitro1" = "GroupA",
#'                                            "Vitro2" = "GroupB"),
#'                              reference = "GroupA")
#'
#' inputs$panel <- tibble::tibble(inputs$panel,
#'                        c(NA_character_,
#'                          NA_character_,
#'                          NA_character_,
#'                          NA_character_,
#'                          NA_character_,
#'                          NA_character_,
#'                          "type",
#'                          "state",
#'                          "state",
#'                          "type",
#'                          "state",
#'                          NA_character_))
#'
#' @export
sce_prepare_inputs <- function(parent_dir,
                               pattern,
                               recursive,
                               sampleIDs,
                               reference){

  files <- list.files(path = parent_dir,
                      pattern = pattern,
                      recursive = recursive,
                      full.names = TRUE)

  fs <- flowCore::read.flowSet(files)

  panel <- dplyr::left_join(
    tibble::enframe(flowCore::colnames(fs), name = NULL, value = "fcs_colname"),
    tibble::enframe(flowCore::markernames(fs), name = "fcs_colname", value = "antigen")
  )

  metadata <- tibble::tibble(file_name = flowCore::sampleNames(fs),
                             sample_id = names(sampleIDs),
                             condition = sampleIDs)

  metadata$condition <- forcats::fct_relevel(metadata$condition, reference)

  inputs <- list(fs = fs, panel = panel, metadata = metadata)

  message("Please edit the following items before proceeding: \n
          - edit inputs$panel to contain a column called marker_class that specifies
          whether a given marker is 'type', 'state', 'both', or NA_character_ \n
          - confirm that antigen names are correct in inputs$panel \n
          - confirm that the sample_id and conditions in inputs$metadata match their
          file names. This will not be confirmed after building sce and can mess up
          your data")

  return(inputs)
}
