#' Draw a differential state volcano plot
#'
#' sce_volcano draws a separate volcano plot for every cluster specified in the
#' annotated clusters of the sce object after running sce_diff_state.
#'
#' @param diffState The formatted differential state tibble produced by sce_diff_state
#' @param cutoff the significance cutoff (using p_adj) you want to use for labeling
#' @param coords a named list givin xlim and ylim passed to ggplot
#' @param filter_cluster a character vector naming specific cluster_ids that you
#'   want removed from the sce_diff_state results prior to plotting (usually
#'   "unknown" samples that likely represent debris or dead cells).
#'
#' @return a ggplot object for each cluster used in sce_diff_state.
#' @examples
#'
#' # Since most sce_diff_state objects will be a double-nested list (the first
#' # level containing the different sce objects and the second containing the
#' # different comparisons), you will likely want to call this one with two levels
#' # of map, like so:
#'
#' \dontrun{
#' # don't run automatically, needs a massive sce which is too big to provide in
#' this package.
#'   map(diffStates,
#'       ~map(..1,
#'            sce_volcano,
#'            cutoff = 0.05,
#'            coords = list(xlim = c(-2, 2), ylim = c(0, 3))))
#' }
#'
#' @importFrom rlang .data
#'
#' @export
sce_volcano <- function(diffState, cutoff, coords = NULL, filter_cluster = NULL){
  if(!is.null(filter_cluster)){
    for(i in seq_along(filter_cluster))
      diffState <- dplyr::filter(diffState, !stringr::str_detect(.data$cluster_id,
                                                                 filter_cluster[[i]]))
  }

  gg <- ggplot2::ggplot(dplyr::mutate(diffState,
                                      marker_id = stringr::str_remove_all(.data$marker_id,
                                                                          ".XGBoost")),
         ggplot2::aes(.data$logFC, -log10(.data$p_val), label = .data$marker_id)) +
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted") +
    ggrepel::geom_text_repel(data = dplyr::filter(diffState, .data$p_adj < cutoff) %>%
                      dplyr::mutate(marker_id = stringr::str_remove_all(.data$marker_id,
                                                                        ".XGBoost"))) +
    ggplot2::facet_wrap(~.data$cluster_id) +
    ggpubr::theme_pubr()

  if(!is.null(coords)){
    gg <- gg +
      ggplot2::coord_cartesian(xlim = coords$xlim, ylim = coords$ylim)
  }

  return(gg)
}
