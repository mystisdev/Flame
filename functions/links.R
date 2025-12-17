attachTextMiningDBLinks <- function(df) {
  df$ID_noLINKS <- df$ID 
  df$ID[grep("^ENS", df$ID_noLINKS)] <- 
    paste0(
      "<a href='",
      sprintf('https://www.ensembl.org/id/%s',
              df$ID_noLINKS[grep("^ENS", df$ID_noLINKS)]),
      "' target = '_blank'>",
      df$ID_noLINKS[grep("^ENS", df$ID_noLINKS)],
      "</a>"
    )
  df$ID[grep("^hsa-", df$ID_noLINKS)] <-
    paste0(
      "<a href='",
      sprintf('https://www.mirbase.org/textsearch.shtml?q=%s',
              df$ID_noLINKS[grep("^hsa-", df$ID_noLINKS)]),
      "' target = '_blank'>",
      df$ID_noLINKS[grep("^hsa-", df$ID_noLINKS)],
      "</a>"
    )
  return(df)
}

attachVariantTableLinks <- function(df) {
  df$rs_id_noLinks <- df$rs_id
  df$ensgs_noLinks <- df$ensgs
  df$ensgs[grep("^ENS", df$ensgs_noLinks)] <- 
    paste0(
      "<a href='",
      sprintf('https://www.ensembl.org/id/%s',
              df$ensgs_noLinks[grep("^ENS", df$ensgs_noLinks)]),
      "' target = '_blank'>",
      df$ensgs_noLinks[grep("^ENS", df$ensgs_noLinks)],
      "</a>"
    )
  df$rs_id[grep("^rs", df$rs_id_noLinks)] <- 
    paste0(
      "<a href='",
      sprintf('https://www.ncbi.nlm.nih.gov/snp/%s',
              df$rs_id_noLinks[grep("^rs", df$rs_id_noLinks)]),
      "' target = '_blank'>",
      df$rs_id_noLinks[grep("^rs", df$rs_id_noLinks)],
      "</a>"
    )
  df <- subset(df, select = -c(rs_id_noLinks, ensgs_noLinks))
  return(df)
  
}

attachDBLinks <- function(resultKey = NULL) { # Transfac HPA CORUMLinks, unavailable
  if (is.null(resultKey)) resultKey <- currentType_Tool

  # Gene Ontology - stopChar=":" prevents matching "GOSLIM:*"
  attachLinks("GO", "https://www.ebi.ac.uk/QuickGO/term/", stopChar = ":", resultKey = resultKey)
  # GO Slim terms use same GO term IDs, just curated subset
  attachLinks("GOSLIM", "https://www.ebi.ac.uk/QuickGO/term/", stopChar = ":", resultKey = resultKey)

  # Protein domains and classifications
  attachLinks("INTERPRO", "https://www.ebi.ac.uk/interpro/entry/InterPro/", resultKey = resultKey)
  attachLinks("PFAM", "https://www.ebi.ac.uk/interpro/entry/pfam/", resultKey = resultKey)
  attachLinks("UNIPROT", "https://www.uniprot.org/keywords/", resultKey = resultKey)
  attachLinks("PANTHERPC", "https://pantherdb.org/panther/category.do?categoryAcc=", resultKey = resultKey)

  # Pathways
  attachLinks("PANTHER Pathways", "http://www.pantherdb.org/pathway/pathDetail.do?clsAccession=", resultKey = resultKey)
  attachLinks("REAC", "https://reactome.org/content/detail/", resultKey = resultKey)
  attachLinks("WP", "https://www.wikipathways.org/index.php/Pathway:", resultKey = resultKey)
  attachLinks("BioPlanet", "https://tripod.nih.gov/bioplanet/detail.jsp?pid=", urlSuffix = "&target=pathway", resultKey = resultKey)

  # Disease and phenotype ontologies
  attachLinks("DO", "http://www.informatics.jax.org/disease/", resultKey = resultKey)
  attachLinks("HP", "https://monarchinitiative.org/", resultKey = resultKey)
  attachLinks("OMIM", "https://www.omim.org/entry/", resultKey = resultKey)
  attachLinks("ORPHA", "https://www.orpha.net/consor/cgi-bin/OC_Exp.php?Lng=GB&Expert=", gSub = "ORPHA:", resultKey = resultKey)
  attachLinks("WBP", "https://wormbase.org/species/all/phenotype/", resultKey = resultKey)
  attachLinks("WBBT", "https://wormbase.org/species/all/anatomy_term/", resultKey = resultKey)
  attachLinks("MGI", "https://www.informatics.jax.org/vocab/mp_ontology/", resultKey = resultKey)

  # Tissue ontologies
  attachLinks("BTO", "https://www.ebi.ac.uk/ols/ontologies/bto/terms?iri=http%3A%2F%2Fpurl.obolibrary.org%2Fobo%2FBTO_", gSub = "BTO:", resultKey = resultKey)

  # Regulatory elements
  attachLinks("TF", "http://gene-regulation.com/cgi-bin/pub/databases/transfac/search.cgi?species=Homo_sapiens&factor=", resultKey = resultKey)
  attachLinks("CollecTRI", "https://www.genecards.org/cgi-bin/carddisp.pl?gene=", resultKey = resultKey)
  attachLinks("MIRNA", "https://www.mirbase.org/textsearch.shtml?q=", gSub = "MIRNA:", resultKey = resultKey)

  # Pharmacogenomics and drug perturbations
  attachLinks("PharmGKB", "https://www.clinpgx.org/chemical/", resultKey = resultKey)
  # LINCS: No external links available - terms displayed as plain text

  attachKEGGLinks(resultKey)
}

attachLinks <- function(sourceId, url, stopChar = "$", gSub = NULL, urlSuffix = "", resultKey = NULL) {
  if (is.null(resultKey)) resultKey <- currentType_Tool

  linksVector <-
    enrichmentResults[[resultKey]][grepl(
      paste0("^", sourceId, stopChar), enrichmentResults[[resultKey]]$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    gSubLinksVector <- linksVector
    if (!is.null(gSub))
      gSubLinksVector <- gsub(gSub, "", linksVector)
    enrichmentResults[[resultKey]][grepl(
      paste0("^", sourceId, stopChar), enrichmentResults[[resultKey]]$Source), ]$Term_ID <<-
      paste0(
        "<a href='", url, gSubLinksVector, urlSuffix, "' target='_blank'>",
        linksVector, "</a>"
      )
  }
}

MAX_KEGG_HIGHLIGHTED_GENES <- 10

attachKEGGLinks <- function(resultKey = NULL) {
  if (is.null(resultKey)) resultKey <- currentType_Tool

  # KEGG organism codes and supported organisms reference:
  # https://www.kegg.jp/kegg/tables/br08606.html
  # This table contains all KEGG-supported organisms and their corresponding
  # 3-4 letter organism codes (kegg_name) used in pathway URLs

  tempEnrichmentDF <-
    enrichmentResults[[resultKey]][grepl(
      "^KEGG$", enrichmentResults[[resultKey]]$Source
    ), ][, c("Source", "Positive Hits", "Term_ID")]

  if (nrow(tempEnrichmentDF) > 0) {
    shortName <- ORGANISMS[ORGANISMS$taxid == currentOrganism, ]$short_name
    keggName <- ORGANISMS[ORGANISMS$taxid == currentOrganism, ]$kegg_name

    if (currentEnrichmentTool == "STRING") {
      # STRING already provides organism-specific KEGG IDs (e.g., "eco00260", "hsa05224")
      # No kegg_name needed, STRING already provides the organism kegg name (e.g., "hsa") in the Term_ID

      if (!is.na(shortName)) {
        # Add gene highlighting using g:Profiler conversion
        conversionTable <- createEntrezAccConversionTable(tempEnrichmentDF, shortName)
        if (!is.null(conversionTable)) {
          tempEnrichmentDFWithEntrezAcc <-
            convertPositiveHitsToEntrezAcc(tempEnrichmentDF, conversionTable)
          linksVector <- tempEnrichmentDFWithEntrezAcc$Term_ID
          geneHighlights <- gsub(",", "+", tempEnrichmentDFWithEntrezAcc$`Positive Hits EntrezAcc`)
          # Limit highlighting per pathway to avoid "Request-URI Too Long" errors
          geneCounts <- sapply(strsplit(geneHighlights, "\\+"), length)
          urlSuffix <- ifelse(geneCounts <= MAX_KEGG_HIGHLIGHTED_GENES,
                              paste0("+", geneHighlights),
                              "")
        } else {
          linksVector <- tempEnrichmentDF$Term_ID
          urlSuffix <- ""  # No gene highlighting if conversion fails
        }
      } else {
        # No g:Profiler support - pathway links only
        linksVector <- tempEnrichmentDF$Term_ID
        urlSuffix <- ""  # No gene highlighting
      }

      enrichmentResults[[resultKey]][grepl(
        "^KEGG$", enrichmentResults[[resultKey]]$Source), ]$Term_ID <<-
        paste0(
          "<a href='https://www.kegg.jp/kegg-bin/show_pathway?",
          linksVector, urlSuffix,
          "' target='_blank'>", linksVector, "</a>"
        )

    } else {
      # Traditional tools (gProfiler, GeneCodis, etc.)
      if (!is.na(keggName)) {
        # Need keggName for organism-specific pathway URLs

        # Try gene highlighting if organism supported by gProfiler
        if (!is.na(shortName)) {
          conversionTable <- createEntrezAccConversionTable(tempEnrichmentDF, shortName)
          if (!is.null(conversionTable)) {
            # Conversion succeeded - create links WITH gene highlighting
            tempEnrichmentDFWithEntrezAcc <-
              convertPositiveHitsToEntrezAcc(tempEnrichmentDF, conversionTable)
            linksVector <- tempEnrichmentDFWithEntrezAcc$Term_ID
            geneHighlights <- gsub(",", "+", tempEnrichmentDFWithEntrezAcc$`Positive Hits EntrezAcc`)
            # Limit highlighting per pathway to avoid "Request-URI Too Long" errors
            geneCounts <- sapply(strsplit(geneHighlights, "\\+"), length)
            urlSuffix <- ifelse(geneCounts <= MAX_KEGG_HIGHLIGHTED_GENES,
                                paste0("+", geneHighlights),
                                "")
          } else {
            # Conversion failed - create links WITHOUT highlighting
            linksVector <- tempEnrichmentDF$Term_ID
            urlSuffix <- ""
          }
        } else {
          # No gProfiler support - create links WITHOUT highlighting
          linksVector <- tempEnrichmentDF$Term_ID
          urlSuffix <- ""
        }

        # Create KEGG links (with or without gene highlighting)
        enrichmentResults[[resultKey]][grepl(
          "^KEGG$", enrichmentResults[[resultKey]]$Source), ]$Term_ID <<-
          paste0(
            "<a href='https://www.kegg.jp/kegg-bin/show_pathway?",
            gsub("KEGG:|map", keggName, linksVector),
            urlSuffix,
            "' target='_blank'>", linksVector, "</a>"
          )
      }
      # If organism lacks kegg_name, no KEGG links created (can't construct URL)
    }
  }
}

createEntrezAccConversionTable <- function(tempEnrichmentDF, shortName) {
  inputToConvert <- unique(unlist(strsplit(paste(
    tempEnrichmentDF[grepl("^KEGG$", tempEnrichmentDF$Source), ]$`Positive Hits`,
    collapse = ","), ",")))

  conversionTable <- calculateConversionTable(inputToConvert, shortName)
  
  if (!is.null(conversionTable)) {
    conversionTable <- dplyr::distinct(conversionTable[, c("input", "target")])
    colnames(conversionTable)[1] <- "Positive Hits"
  }
  return(conversionTable)
}

calculateConversionTable <- function(inputToConvert, shortName) {
  if (currentNamespace == "ENTREZGENE_ACC" &&
      input$functional_enrichment_inputConversion == "Converted input names")
    conversionTable <- data.frame(
      "input" = inputToConvert,
      "target" = inputToConvert
    )
  else {
    # Try to convert using gProfiler
    # If organism not supported by gProfiler, return NULL (KEGG links will work without gene highlighting)
    conversionTable <- tryCatch({
      gprofiler2::gconvert(inputToConvert,
                          organism = shortName,
                          target = "ENTREZGENE_ACC")
    }, error = function(e) {
      return(NULL)
    })
  }
  return(conversionTable)
}

convertPositiveHitsToEntrezAcc <- function(tempEnrichmentDF, conversionTable) {
  tempEnrichmentDF <- tidyr::separate_rows(tempEnrichmentDF,
                                           `Positive Hits`, sep = ",")
  tempEnrichmentDF <- plyr::join(tempEnrichmentDF, conversionTable,
                                  type = "left", by = "Positive Hits")
  tempEnrichmentDF <-
    tempEnrichmentDF[, !(names(tempEnrichmentDF) %in% c("Positive Hits", "Source"))]
  colnames(tempEnrichmentDF)[match("target", colnames(tempEnrichmentDF))] <-
    "Positive Hits EntrezAcc"
  tempEnrichmentDF <- tempEnrichmentDF %>%
    dplyr::group_by(Term_ID) %>%
    dplyr::mutate(`Positive Hits EntrezAcc` = paste(`Positive Hits EntrezAcc`,
                                                    collapse = ","))
  tempEnrichmentDF <- dplyr::distinct(tempEnrichmentDF)
  tempEnrichmentDF <- plyr::join(enrichmentResults[[currentType_Tool]][grepl(
    "^KEGG$", enrichmentResults[[currentType_Tool]]$Source), ],
    tempEnrichmentDF, type = "left", by = "Term_ID")
  return(tempEnrichmentDF)
}

attachWebgestaltLinks <- function(links) {
  enrichmentResults[[currentType_Tool]]$Term_ID_noLinks <<- 
    enrichmentResults[[currentType_Tool]]$Term_ID
  enrichmentResults[[currentType_Tool]]$Term_ID <<-
    paste0(
      "<a href='",
      links,
      "' target='_blank'>",
      enrichmentResults[[currentType_Tool]]$Term_ID,
      "</a>"
    )
  # quick and dirty fix for changed DISGENET links
  if("DISGENET" %in% enrichmentResults[[currentType_Tool]]$Source) {
    enrichmentResults[[currentType_Tool]][enrichmentResults[[currentType_Tool]]$Source == "DISGENET",]$Term_ID <<-
      paste0(
        "<a href='https://www.disgenet.org/search/0/",
        enrichmentResults[[currentType_Tool]][enrichmentResults[[currentType_Tool]]$Source == "DISGENET",]$Term_ID_noLinks,
        "/' target='_blank'>",
        enrichmentResults[[currentType_Tool]][enrichmentResults[[currentType_Tool]]$Source == "DISGENET",]$Term_ID_noLinks,
        "</a>"
      )  
  }
}
