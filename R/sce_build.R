#' Build sce object from prepared inputs
#'
#' This is just a wrapper around CATALYST::prepData that includes defaults for
#' flow cytometry data and allows you to pass a single inputs object to quickly
#' build the object without needing to look up all the extra args. Also plots
#' the MDS by default so you can quickly check if there are sample problems.
#'
#' @param inputs Inputs as prepared by calling sce_prepare_inputs AND THEN
#'   EDITING THE PANEL.
#' @param plotMDS Boolean specifying whether an MDS plot should be generated for
#'   this sce.
#' @param factors Character vector of factors to include in the experiment.
#'   Defaults to "condition" for backwards compatability but all future ones
#'   should use c("condition", "conditionCHAR").
#'
#' @return an SCE object ready to go into the CATALYST pipeline.
#'
#' @export
sce_build <- function(inputs, plotMDS = TRUE, factors = "condition"){
  sce <- CATALYST::prepData(inputs$fs,
                            inputs$panel,
                            inputs$md,
                            cofactor = 150,
                            md_cols = list(file = "file_name",
                                           sample = "sample_id",
                                           factors = factors),
                            FACS = TRUE)

  CATALYST::pbMDS(sce,
                  by = "sample_id",
                  color_by = "sample_id")

  return(sce)
}
