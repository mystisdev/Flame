getWebgestaltBackgroundSize <- function(organism = "hsapiens", referenceSet = "genome_protein-coding") {
  url <- sprintf("https://www.webgestalt.org/api/reference?organism=%s&referenceSet=%s", organism, referenceSet)
  x <- read.csv(url(url), header=F)
  return(length(x$V1))
}


# =============================================================================
# WebGestaltStrategy - Tool Strategy Implementation
# =============================================================================

WebGestaltStrategy <- R6::R6Class("WebGestaltStrategy",

  inherit = ToolStrategy,

  public = list(
    initialize = function() {
      super$initialize("WebGestalt")
    },

    run = function(inputList, organism, backgroundList, params) {
      # Get datasource codes
      datasources <- as.character(DATASOURCES_CODES[["WEBGESTALT"]][params$datasources])
      datasources <- datasources[!is.na(datasources)]

      if (length(datasources) == 0) {
        return(NULL)
      }

      # Get organism short name (WebGestalt uses different name for dog)
      organismName <- ORGANISMS[ORGANISMS$taxid == organism, ]$short_name
      if (organismName == "clfamiliaris") {
        organismName <- "cfamiliaris"
      }

      # Determine namespace
      namespace <- if (!is.null(params$namespace) && params$namespace != "USERINPUT") {
        "entrezgene"
      } else {
        "genesymbol"
      }

      # Determine significance method
      metric <- params$metric
      if (metric == "top") {
        sigMethod <- "top"
        fdrMethod <- "BH"
      } else {
        sigMethod <- "fdr"
        fdrMethod <- metric
      }

      # Determine background
      if (is.null(backgroundList)) {
        referenceSet <- "genome"
        referenceGene <- NULL
        referenceGeneType <- NULL
      } else {
        referenceSet <- NULL
        referenceGene <- backgroundList
        referenceGeneType <- namespace
      }

      # Call WebGestalt API
      result <- suppressWarnings(WebGestaltR::WebGestaltR(
        organism = organismName,
        enrichDatabase = datasources,
        interestGene = inputList,
        interestGeneType = namespace,
        referenceGene = referenceGene,
        referenceGeneType = referenceGeneType,
        referenceSet = referenceSet,
        sigMethod = sigMethod,
        fdrMethod = fdrMethod,
        fdrThr = as.numeric(params$threshold),
        topThr = 100,
        isOutput = FALSE,
        hostName = "https://www.webgestalt.org/"
      ))

      # Calculate background size before validation (need organismName var)
      backgroundSize <- if (is.null(backgroundList)) {
        getWebgestaltBackgroundSize(organism = organismName)
      } else {
        length(backgroundList)
      }

      if (!isResultValid(result)) {
        return(NULL)
      }

      # Parse result (includes link attachment inline to avoid timing issues)
      result <- private$parseResult(result, length(inputList), params$datasources)

      # Filter by datasources using params (no global dependency)
      if (!is.null(params$datasources) && length(params$datasources) > 0) {
        result <- result[result$Source %in% params$datasources, ]
      }

      if (nrow(result) == 0) {
        return(NULL)
      }

      # Return structured result (no global writes)
      return(list(
        result = result,
        backgroundSize = backgroundSize,
        rawResult = NULL
      ))
    },

    convertIDs = function(geneList, organism, targetNamespace) {
      return(geneList)
    },

    getValidDatasources = function(organism) {
      return(names(DATASOURCES_CODES[["WEBGESTALT"]]))
    },

    getDefaultMetric = function(hasBackground) {
      return("BH")
    }
  ),

  private = list(
    parseResult = function(result, numInputs, selectedDatasources) {
      if (is.null(result$database)) {
        result$database <- as.character(DATASOURCES_CODES[["WEBGESTALT"]][selectedDatasources])
      }
      result$database <- unlistDatasourceCodes(result$database, DATASOURCES_CODES[["WEBGESTALT"]])

      if (is.null(result$userId)) {
        result$userId <- result$overlapId
      }
      result$userId <- gsub(";", ",", result$userId)
      result$querySize <- numInputs

      # Preserve original Term_ID before creating linked version
      termIdNoLinks <- result$geneSet

      # Create linked Term_IDs if links are available
      if (!is.null(result$link)) {
        linkedTermId <- paste0(
          "<a href='", result$link, "' target='_blank'>",
          result$geneSet, "</a>"
        )
        # Handle DISGENET special case (same as attachWebgestaltLinks)
        if ("DISGENET" %in% result$database) {
          disgenetMask <- result$database == "DISGENET"
          linkedTermId[disgenetMask] <- paste0(
            "<a href='https://www.disgenet.org/search/0/",
            result$geneSet[disgenetMask], "/' target='_blank'>",
            result$geneSet[disgenetMask], "</a>"
          )
        }
        result$geneSet <- linkedTermId
      }

      result <- result[, c(
        "database", "geneSet", "description", "pValue",
        "size", "querySize", "overlap", "userId"
      )]
      colnames(result) <- ENRICHMENT_DF_COLNAMES
      result <- mapKEGGIds(result)

      # Add Term_ID_noLinks column (formatResultTable checks for this)
      result$Term_ID_noLinks <- termIdNoLinks

      return(result)
    }
  )
)

# Register the strategy
toolRegistry$register("functional", "WebGestalt", WebGestaltStrategy$new())