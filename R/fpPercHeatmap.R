#' @title fpPercHeatmap
#'
#' @description Draws heatmaps of the percentages of all fragments in
#'   a ROI-sample combination in each footprint pattern.
#'
#' @details Draws heatmaps of the percentages of all fragments in a
#'   ROI-sample combination in each
#' footprint pattern supplied (for example: "tf", "open", "upNuc",
#'   "Nuc", "downNuc"). The rows of the heatmaps are split by ROI group.
#'
#' @param footprint_percentages A tibble where each column
#'   corresponds to a sample-footprint percentage and each row to a ROI,
#'   with the rows clustered by similarity.
#' @param breaks A list of vectors indicating numeric breaks used
#'   in (\code{ColorRamp2}) to define the heatmap color gradient,
#'   with one element per pattern (usually 5, or 3 if the nucleosome
#'   patterns have been combined).
#' @param plotcols A character vector of 5 colors to be used for
#'   the heatmaps of the 5 footprint
#' patterns ("tf", "open", "upNuc", "Nuc", "downNuc"), or 3
#'   colors if the nucleosome patterns have been combined.
#'
#' @return Heatmaps of the percentages of all fragments in a
#'   ROI-sample combination in each footprint pattern.
#'
#' @examples
#' NomeData <- createExampleData()
#' NomeData <- footprintCalc(NomeData)
#' NomeData <- footprintQuant(NomeData)
#' footprint_percentages <- footprintPerc(NomeData)
#' fpPercHeatmap(footprint_percentages)
#'
#' @importFrom ComplexHeatmap Heatmap
#' @importFrom circlize colorRamp2
#' @importFrom stringr str_extract
#' @importFrom methods is
#'
#' @export
fpPercHeatmap <- function(footprint_percentages, breaks =
    rep(list(c(0, 50, 100)), 5), plotcols = c("#236467",
    "#AA9B39", "#822B56", "#822B26", "#822B99")) {

    # check footprint_percentages argument for correct class
    stopifnot("`footprint_percentages` must be a data.frame or tibble" =
        is(footprint_percentages, "data.frame") |
            is(footprint_percentages, "tibble"))

    # check that footprint_percentages hast columns ROI and ROIgroup
    stopifnot("'footprint_percentages' column 1 must be called ROI" =
        colnames(footprint_percentages)[1] == "ROI")
    stopifnot("'footprint_percentages' column 2 must be called ROIgroup" =
        colnames(footprint_percentages)[2] == "ROIgroup")

    # extract patterns
    patterns <- unique(str_extract(colnames(footprint_percentages)
        [3:ncol(footprint_percentages)], "^[^_]+"))

    # initialize empty heatmap list
    ht_list <- NULL

    # draw heatmaps
    for (i in seq_along(patterns)) {
        ht_list <- ht_list + Heatmap(
            matrix = as.matrix(footprint_percentages[, grep(
                paste0("^", patterns[i]),
                colnames(footprint_percentages)
            )]),
            border = TRUE,
            col = colorRamp2(
                breaks = breaks[[i]],
                colors = c("white", plotcols[i], "black")
            ),
            cluster_rows = FALSE,
            column_title = patterns[i],
            name = patterns[i],
            cluster_columns = FALSE,
            show_column_names = TRUE,
            show_row_names = FALSE,
            na_col = "grey",
            use_raster = FALSE,
            heatmap_legend_param = list(title = patterns[i]),
            split = footprint_percentages$ROIgroup
        )
    }
    return(ht_list)
}
