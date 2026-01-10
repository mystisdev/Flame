# =============================================================================
# FLAME ORA Enrichment Session
# =============================================================================
#
# Concrete implementation of EnrichmentSession for Over-Representation Analysis.
# Each instance represents one ORA enrichment run and owns ALL its data.
#
# Lifecycle:
# 1. Created by EnrichmentFormSession on submit
# 2. execute() runs full enrichment flow:
#    - Gene conversion
#    - Strategy execution via toolRegistry
#    - Result transformation
#    - DB link attachment
# 3. insertUI() adds tab to results panel
# 4. server() sets up observers for plots/tables
# 5. cleanup() destroys observers when tab closes
#
# Data Ownership (Option B - no globals):
# - Results stored in private$.results (NOT enrichmentResults global)
# - Background size in private$.backgroundSize
# - Raw API response in private$.rawApiResponse
# - Arena edgelists in private$.arenaEdgelists
#
# Dependencies:
# - enrich-session-base.R (for EnrichmentSession)
# - core-tool_registry.R (for toolRegistry)
# - func-tabGeneration.R (for UI generation, temporarily)
#
# =============================================================================

#' ORA Enrichment Session Class
#'
#' Handles Over-Representation Analysis (ORA) enrichment runs.
#' Owns all its data - results are stored in session, NOT globals.
#'
#' @section Results Storage:
#' Results are stored in private$.results (NOT globals).
#' Access via getResults(), hasResults().
#'
ORAEnrichmentSession <- R6::R6Class(
 "ORAEnrichmentSession",
  inherit = EnrichmentSession,

  public = list(
    #' @field background Optional background gene list (AnalyteList)
    background = NULL,

    #' Initialize an ORA Enrichment Session
    #' @param id Character. Unique session ID.
    #' @param runId Character. Run ID for tab panel.
    #' @param uniqueId Integer. Unique counter for Shiny IDs.
    #' @param displayNumber Integer. Display number for tab title.
    #' @param toolName Character. Tool name.
    #' @param organism Integer. Organism taxid.
    #' @param input AnalyteList. Input gene list.
    #' @param background AnalyteList. Optional background gene list.
    #' @param parameters List. Enrichment parameters (datasources, threshold, etc.).
    initialize = function(id, runId, uniqueId, displayNumber, toolName, organism,
                          input, background = NULL, parameters = list()) {
      super$initialize(
        id = id,
        runId = runId,
        uniqueId = uniqueId,
        displayNumber = displayNumber,
        toolName = toolName,
        organism = organism,
        input = input,
        parameters = parameters
      )
      self$background <- background
      private$.arenaEdgelists <- list()
    },

    #' Execute the ORA enrichment
    #'
    #' Full enrichment flow:
    #' 1. Convert gene IDs to tool-specific format
    #' 2. Call toolRegistry strategy
    #' 3. Transform results (add columns, format)
    #' 4. Sort by -log10Pvalue
    #' 5. Attach database links
    #' 6. Store everything in private fields
    #'
    #' @return Invisible self for chaining
    execute = function() {
      # Get parameters
      params <- private$.parameters
      namespace <- params$namespace

      # Step 1: Convert gene IDs to tool-specific format
      geneIds <- private$.input$getIds()
      inputConversionTable <- private$convertGeneIds(geneIds, namespace)

      if (is.null(inputConversionTable) || nrow(inputConversionTable) == 0) {
        warning(sprintf("Gene conversion failed for %s", self$toolName))
        return(invisible(self))
      }

      # Store conversion table and namespace
      private$.conversionTable <- inputConversionTable
      resolvedNamespace <- attr(inputConversionTable, "namespace")
      private$.parameters$namespace <- if (!is.null(resolvedNamespace)) resolvedNamespace else namespace

      # Convert background if provided
      backgroundConversionTable <- NULL
      convertedBackgroundIds <- NULL
      if (!is.null(self$background)) {
        backgroundIds <- self$background$getIds()
        backgroundConversionTable <- private$convertGeneIds(backgroundIds, namespace)
        if (!is.null(backgroundConversionTable)) {
          convertedBackgroundIds <- backgroundConversionTable$target
          private$.backgroundConversionTable <- backgroundConversionTable
        }
      }

      # Step 2: Call toolRegistry strategy
      convertedInputIds <- inputConversionTable$target

      # Check if we have a strategy
      if (!toolRegistry$hasStrategy("functional", self$toolName)) {
        warning(sprintf("No strategy registered for %s", self$toolName))
        return(invisible(self))
      }

      strategyResult <- tryCatch({
        strategy <- toolRegistry$get("functional", self$toolName)
        strategy$run(
          convertedInputIds,
          self$organism,
          convertedBackgroundIds,
          params
        )
      }, error = function(e) {
        warning(sprintf("Enrichment failed for %s: %s", self$toolName, e$message))
        NULL
      })

      if (is.null(strategyResult) || is.null(strategyResult$result) ||
          nrow(strategyResult$result) == 0) {
        return(invisible(self))
      }

      # Store raw API response (needed for Manhattan plot)
      private$.rawApiResponse <- strategyResult$rawResult
      private$.backgroundSize <- strategyResult$backgroundSize

      # Step 3: Transform results
      results <- transformEnrichmentResultTable(strategyResult$result)

      # Step 4: Sort by -log10Pvalue descending
      results <- results[order(-results$`-log10Pvalue`), ]

      # Store Term_ID_noLinks before link attachment
      if (is.null(results$Term_ID_noLinks)) {
        results$Term_ID_noLinks <- results$Term_ID
      }

      # Step 5: Attach database links
      results <- private$attachDBLinks(results)

      # Step 6: Store final results
      private$.results <- results

      # TEMPORARY: Also write to global for backwards compatibility with plot code
      # TODO: Remove this when plot code migrates to OutputSessions (Part 3)
      enrichmentResults[[self$id]] <<- results

      invisible(self)
    },

    #' Rollback converted names to original input symbols
    #'
    #' Converts Positive Hits from tool-specific IDs back to original gene symbols.
    #' Called when user selects "Original input names" option.
    #'
    #' @return Character vector of IDs to use for no-hit calculation
    rollbackNames = function() {
      if (!self$hasResults()) return(character(0))

      conversionTable <- private$.conversionTable
      if (is.null(conversionTable)) return(character(0))

      results <- private$.results

      # Check if all Positive Hits are empty (safety check)
      allEmpty <- all(results$`Positive Hits` == "" | is.na(results$`Positive Hits`))
      if (allEmpty) {
        return(conversionTable$input)
      }

      # Rollback the names
      results <- tidyr::separate_rows(results, `Positive Hits`, sep = ",\\s*")
      results <- merge(results, conversionTable,
                       by.x = "Positive Hits", by.y = "target")
      results <- results[, !(names(results) %in% c("Positive Hits", "name"))]
      colnames(results)[match("input", colnames(results))] <- "Positive Hits"
      results <- results %>%
        dplyr::group_by(Term_ID) %>%
        dplyr::mutate(`Positive Hits` = paste(`Positive Hits`, collapse = ","))

      # Reorder columns to match expected structure (required for ALL tools)
      # After merge, column order changes - Positive Hits ends up at the end
      # Must restore to: Source, Term_ID, Function, P-value, -log10Pvalue,
      #                  Term Size, Query size, Intersection Size, Enrichment Score %,
      #                  Positive Hits, Term_ID_noLinks
      expectedCols <- c("Source", "Term_ID", "Function", "P-value",
                        "-log10Pvalue", "Term Size", "Query size",
                        "Intersection Size", "Enrichment Score %",
                        "Positive Hits", "Term_ID_noLinks")
      # Only include columns that exist
      existingCols <- expectedCols[expectedCols %in% colnames(results)]
      results <- results[, existingCols]
      results <- as.data.frame(dplyr::distinct(results))

      # Update stored results
      private$.results <- results

      # TEMPORARY: Sync with global for backwards compatibility
      enrichmentResults[[self$id]] <<- results

      # Return original input IDs for no-hit calculation
      return(conversionTable$input)
    },

    #' Get IDs for no-hit calculation without rollback
    #' @return Character vector of converted IDs
    getConvertedIds = function() {
      conversionTable <- private$.conversionTable
      if (is.null(conversionTable)) return(character(0))
      return(conversionTable$target)
    },

    #' Generate the tab UI content
    #'
    #' Creates the UI for the enrichment results tab.
    #' For Part 2, this delegates to existing generateToolPanelContent().
    #'
    #' @return Shiny UI elements
    ui = function() {
      # For Part 2, delegate to existing tab generation
      # This will be replaced with proper UI in Part 3 (OutputSessions)
      ns <- shiny::NS(self$id)

      # Generate tab content using existing function
      # NOTE: This is temporary - will be refactored in Part 3
      generateToolPanelContent(
        fullRunKey = self$id,
        runId = self$runId,
        toolName = self$toolName,
        uniqueId = self$uniqueId
      )
    },

    #' Set up server logic
    #'
    #' Registers observers for plot generation, table rendering, etc.
    #' For Part 2, this delegates to existing observer registration.
    #'
    #' @param input Shiny input object
    #' @param output Shiny output object
    #' @param parentSession Shiny session object
    server = function(input, output, parentSession) {
      # Use moduleServer for namespacing
      shiny::moduleServer(self$id, function(input, output, session) {
        # Store session references
        private$.moduleSession <- session

        # For Part 2, delegate to existing observer registration
        # This will be refactored in Part 3 (OutputSessions)
        # NOTE: Observers are currently registered externally in func-observers.R
        # We'll migrate them here incrementally

        # Register close tab observer
        private$.observers$close <- shiny::observeEvent(
          input[[paste0("close_", self$runId)]],
          {
            # Trigger cleanup - will be called by EnrichmentFormSession
            # For now, just log
            message(sprintf("Close requested for session: %s", self$id))
          },
          ignoreInit = TRUE
        )
      })
    },

    #' Check if this session has a background list
    #' @return Logical
    hasBackground = function() {
      !is.null(self$background)
    },

    #' Print summary
    print = function() {
      cat(sprintf("<ORAEnrichmentSession> %s\n", self$id))
      cat(sprintf("  Tool: %s\n", self$toolName))
      cat(sprintf("  Organism: %s\n", self$organism))
      cat(sprintf("  Input: %d genes\n", private$.input$size()))
      if (self$hasBackground()) {
        cat(sprintf("  Background: %d genes\n", self$background$size()))
      }
      cat(sprintf("  Has results: %s\n", self$hasResults()))
      if (self$hasResults()) {
        cat(sprintf("  Result rows: %d\n", nrow(private$.results)))
      }
      invisible(self)
    }
  ),

  private = list(
    # Module session reference
    .moduleSession = NULL,

    # Background conversion table (separate from input conversion table)
    .backgroundConversionTable = NULL,

    #' Convert gene IDs to tool-specific format
    #'
    #' @param geneList Character vector of gene symbols
    #' @param namespace Target namespace (or NULL for default)
    #' @return Data frame with (input, target, name) columns, or NULL on failure
    convertGeneIds = function(geneList, namespace) {
      # Determine default namespace if not specified
      if (is.null(namespace) || namespace == "Default namespace") {
        namespace <- private$getDefaultTargetNamespace()
      }

      conversionTable <- tryCatch({
        if (namespace == "USERINPUT") {
          # No conversion needed - pass through
          data.frame(
            input = geneList,
            target = geneList,
            name = geneList,
            stringsAsFactors = FALSE
          )
        } else if (self$toolName == "STRING") {
          # STRING: Use STRING's get_string_ids API
          private$stringConvert(geneList)
        } else if (self$toolName == "PANTHER") {
          # PANTHER: Use PANTHER's geneinfo API
          private$pantherConvert(geneList)
        } else if (self$toolName == "GeneCodis") {
          # GeneCodis: No conversion needed
          data.frame(
            input = geneList,
            target = geneList,
            name = geneList,
            stringsAsFactors = FALSE
          )
        } else {
          # gProfiler, WebGestalt, enrichR: Use g:Profiler conversion
          private$gprofilerConvert(geneList, namespace)
        }
      }, error = function(e) {
        warning(sprintf("Gene conversion error for %s: %s", self$toolName, e$message))
        NULL
      })

      # Attach namespace as attribute
      if (!is.null(conversionTable)) {
        attr(conversionTable, "namespace") <- namespace
      }

      conversionTable
    },

    #' Get default target namespace for this tool
    getDefaultTargetNamespace = function() {
      shortName <- ORGANISMS[ORGANISMS$taxid == self$organism, ]$short_name
      switch(
        self$toolName,
        "STRING" = "ENSP",
        "gProfiler" = "USERINPUT",
        "WebGestalt" = "ENTREZGENE_ACC",
        "PANTHER" = "PANTHER_ACC",
        "GeneCodis" = "USERINPUT",
        "enrichR" = {
          if (shortName == "scerevisiae" || shortName == "dmelanogaster")
            "USERINPUT"
          else
            "ENTREZGENE"
        },
        "USERINPUT"  # Default fallback
      )
    },

    #' STRING-specific gene conversion via STRING API
    stringConvert = function(geneList) {
      url <- "https://string-db.org/api/json/get_string_ids"
      params <- list(
        "identifiers" = paste0(geneList, collapse = "%0d"),
        "species" = self$organism
      )
      request <- httr::POST(url, body = params)
      if (httr::status_code(request) == 200) {
        result <- jsonlite::fromJSON(rawToChar(httr::content(request, "raw")))
        if (is.data.frame(result) && nrow(result) > 0) {
          result <- result[, c("queryItem", "stringId", "preferredName")]
          colnames(result) <- c("input", "target", "name")
          return(result)
        }
      }
      NULL
    },

    #' PANTHER-specific gene conversion
    pantherConvert = function(geneList) {
      url <- "https://pantherdb.org/services/oai/pantherdb/geneinfo"
      params <- list(
        "geneInputList" = paste0(geneList, collapse = ","),
        "organism" = self$organism
      )
      request <- httr::POST(url, body = params, encode = "form")
      if (httr::status_code(request) == 200) {
        pantherResponse <- jsonlite::fromJSON(rawToChar(httr::content(request, "raw")))
        if ("search" %in% names(pantherResponse) && "mapped_genes" %in% names(pantherResponse$search)) {
          mappedGenes <- pantherResponse$search$mapped_genes$gene
          if (length(mappedGenes) > 0) {
            return(data.frame(
              input = mappedGenes$mapped_id_list,
              target = mappedGenes$accession,
              name = mappedGenes$sf_name,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
      NULL
    },

    #' gProfiler-based gene conversion
    gprofilerConvert = function(geneList, namespace) {
      organismShortName <- ORGANISMS[ORGANISMS$taxid == self$organism, ]$short_name
      result <- gprofiler2::gconvert(
        geneList,
        organism = organismShortName,
        target = namespace,
        mthreshold = 1,
        filter_na = TRUE
      )
      if (!is.null(result) && nrow(result) > 0) {
        result <- result[, c("input", "target", "name")]
        return(result)
      }
      NULL
    },

    #' Attach database links to Term_ID column
    #'
    #' Modifies the Term_ID column to include hyperlinks to external databases.
    #' @param df Data frame with enrichment results
    #' @return Modified data frame with hyperlinked Term_IDs
    attachDBLinks = function(df) {
      if (is.null(df) || nrow(df) == 0) return(df)

      # Gene Ontology - stopChar=":" prevents matching "GOSLIM:*"
      df <- private$attachLinksToDF(df, "GO", "https://www.ebi.ac.uk/QuickGO/term/", stopChar = ":")
      df <- private$attachLinksToDF(df, "GOSLIM", "https://www.ebi.ac.uk/QuickGO/term/", stopChar = ":")

      # Protein domains and classifications
      df <- private$attachLinksToDF(df, "INTERPRO", "https://www.ebi.ac.uk/interpro/entry/InterPro/")
      df <- private$attachLinksToDF(df, "PFAM", "https://www.ebi.ac.uk/interpro/entry/pfam/")
      df <- private$attachLinksToDF(df, "UNIPROT", "https://www.uniprot.org/keywords/")
      df <- private$attachLinksToDF(df, "PANTHERPC", "https://pantherdb.org/panther/category.do?categoryAcc=")

      # Pathways
      df <- private$attachLinksToDF(df, "PANTHER Pathways", "http://www.pantherdb.org/pathway/pathDetail.do?clsAccession=")
      df <- private$attachLinksToDF(df, "REAC", "https://reactome.org/content/detail/")
      df <- private$attachLinksToDF(df, "WP", "https://www.wikipathways.org/index.php/Pathway:")
      df <- private$attachLinksToDF(df, "BioPlanet", "https://tripod.nih.gov/bioplanet/detail.jsp?pid=", urlSuffix = "&target=pathway")

      # Disease and phenotype ontologies
      df <- private$attachLinksToDF(df, "DO", "http://www.informatics.jax.org/disease/")
      df <- private$attachLinksToDF(df, "HP", "https://monarchinitiative.org/")
      df <- private$attachLinksToDF(df, "OMIM", "https://www.omim.org/entry/")
      df <- private$attachLinksToDF(df, "ORPHA", "https://www.orpha.net/consor/cgi-bin/OC_Exp.php?Lng=GB&Expert=", gSub = "ORPHA:")
      df <- private$attachLinksToDF(df, "WBP", "https://wormbase.org/species/all/phenotype/")
      df <- private$attachLinksToDF(df, "WBBT", "https://wormbase.org/species/all/anatomy_term/")
      df <- private$attachLinksToDF(df, "MGI", "https://www.informatics.jax.org/vocab/mp_ontology/")

      # Tissue ontologies
      df <- private$attachLinksToDF(df, "BTO", "https://www.ebi.ac.uk/ols/ontologies/bto/terms?iri=http%3A%2F%2Fpurl.obolibrary.org%2Fobo%2FBTO_", gSub = "BTO:")

      # Regulatory elements
      df <- private$attachLinksToDF(df, "TF", "http://gene-regulation.com/cgi-bin/pub/databases/transfac/search.cgi?species=Homo_sapiens&factor=")
      df <- private$attachLinksToDF(df, "CollecTRI", "https://www.genecards.org/cgi-bin/carddisp.pl?gene=")
      df <- private$attachLinksToDF(df, "MIRNA", "https://www.mirbase.org/textsearch.shtml?q=", gSub = "MIRNA:")

      # Pharmacogenomics
      df <- private$attachLinksToDF(df, "PharmGKB", "https://www.clinpgx.org/chemical/")

      # KEGG (special handling)
      df <- private$attachKEGGLinksToDF(df)

      # DISGENET special handling
      df <- private$attachDISGENETLinks(df)

      df
    },

    #' Attach links to a specific datasource in the data frame
    attachLinksToDF = function(df, sourceId, url, stopChar = "$", gSub = NULL, urlSuffix = "") {
      pattern <- paste0("^", sourceId, stopChar)
      matches <- grepl(pattern, df$Source)

      if (any(matches)) {
        linksVector <- df$Term_ID[matches]
        gSubLinksVector <- if (!is.null(gSub)) gsub(gSub, "", linksVector) else linksVector

        df$Term_ID[matches] <- paste0(
          "<a href='", url, gSubLinksVector, urlSuffix, "' target='_blank'>",
          linksVector, "</a>"
        )
      }
      df
    },

    #' Attach KEGG-specific links with optional gene highlighting
    attachKEGGLinksToDF = function(df) {
      keggMatches <- grepl("^KEGG$", df$Source)
      if (!any(keggMatches)) return(df)

      keggName <- ORGANISMS[ORGANISMS$taxid == self$organism, ]$kegg_name
      if (is.na(keggName)) return(df)  # No KEGG support for this organism

      linksVector <- df$Term_ID[keggMatches]

      if (self$toolName == "STRING") {
        # STRING provides organism-specific KEGG IDs already
        df$Term_ID[keggMatches] <- paste0(
          "<a href='https://www.kegg.jp/kegg-bin/show_pathway?",
          linksVector, "' target='_blank'>", linksVector, "</a>"
        )
      } else {
        # Traditional tools - need to add organism prefix
        df$Term_ID[keggMatches] <- paste0(
          "<a href='https://www.kegg.jp/kegg-bin/show_pathway?",
          gsub("KEGG:|map", keggName, linksVector),
          "' target='_blank'>", linksVector, "</a>"
        )
      }
      df
    },

    #' Attach DISGENET-specific links
    attachDISGENETLinks = function(df) {
      disgenetMatches <- df$Source == "DISGENET"
      if (any(disgenetMatches)) {
        df$Term_ID[disgenetMatches] <- paste0(
          "<a href='https://www.disgenet.org/search/0/",
          df$Term_ID_noLinks[disgenetMatches],
          "/' target='_blank'>",
          df$Term_ID_noLinks[disgenetMatches],
          "</a>"
        )
      }
      df
    }
  )
)
