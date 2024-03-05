#' dinoR
#'
#' dinoR tests for significant differences in NOMe-seq
#' footprints between two conditions, using genomic regions
#' of interest (ROI) centered around a landmark, for example
#' a transcription factor (TF) motif. This package takes
#' NOMe-seq data (GCH methylation/protection) in the form
#' of a Ranged Summarized Experiment as  input. dinoR can
#' be used to group sequencing fragments into 3 or 5
#' categories representing characteristic footprints
#' (TF bound, nculeosome bound, open chromatin),plot the
#' percentage of fragments in each category in a heatmap,
#' or averaged across different ROI groups, for example,
#' containing a common TF motif. It is designed to compare
#' footprints between two sample groups, using edgeR's
#' quasi-likelihood methods on the total fragment counts
#' per ROI, sample, and footprint category.
#'
#' @author Michaela Schwaiger
#'
#' @keywords internal
"_PACKAGE"
