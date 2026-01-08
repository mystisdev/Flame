# =============================================================================
# FLAME gSNPense Functions
# =============================================================================
#
# API wrapper function for the gSNPense API (gprofiler2 package).
# Used by SNPsInputSession for converting SNP IDs to gene information.
#
# API Documentation: https://biit.cs.ut.ee/gprofiler/snpense
#
# Dependencies:
# - gprofiler2 package
#
# =============================================================================

#' Convert SNP IDs to gene information using gSNPense
#'
#' Wraps gprofiler2::gsnpense() to convert dbSNP IDs to gene mappings.
#'
#' @param snp_ids Character vector. dbSNP IDs (e.g., "rs123", "rs456").
#' @param filter_na Logical. Whether to filter out NA results. Default TRUE.
#' @return Data frame with SNP-to-gene mappings, or NULL on error.
#'
#' @examples
#' \dontrun{
#' result <- gsnpense_convert(c("rs11734132", "rs7961894", "rs4430796"))
#' }
gsnpense_convert <- function(snp_ids, filter_na = TRUE) {
  tryCatch({
    if (length(snp_ids) == 0) {
      cat("[gsnpense_convert] No SNP IDs provided\n")
      return(NULL)
    }

    result <- gprofiler2::gsnpense(snp_ids, filter_na = filter_na)

    if (is.null(result) || nrow(result) == 0) {
      cat("[gsnpense_convert] No results returned from gsnpense\n")
      return(NULL)
    }

    return(result)
  }, error = function(e) {
    cat(paste("[gsnpense_convert] gsnpense_convert error:", conditionMessage(e), "\n"))
    return(NULL)
  })
}
