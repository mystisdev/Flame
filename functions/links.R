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

attachDBLinks <- function() { # Transfac HPA CORUMLinks, unavailable
  # Gene Ontology - stopChar=":" prevents matching "GOSLIM:*"
  attachLinks("GO", "https://www.ebi.ac.uk/QuickGO/term/", stopChar = ":")
  # GO Slim terms use same GO term IDs, just curated subset
  attachLinks("GOSLIM", "https://www.ebi.ac.uk/QuickGO/term/", stopChar = ":")

  # Protein domains and classifications
  attachLinks("INTERPRO", "https://www.ebi.ac.uk/interpro/entry/InterPro/")
  attachLinks("PFAM", "https://www.ebi.ac.uk/interpro/entry/pfam/")
  attachLinks("UNIPROT", "https://www.uniprot.org/keywords/")
  attachLinks("PANTHERPC", "https://pantherdb.org/panther/category.do?categoryAcc=")

  # Pathways
  attachLinks("PANTHER", "http://www.pantherdb.org/pathway/pathDetail.do?clsAccession=")
  attachLinks("REAC", "https://reactome.org/content/detail/")
  attachLinks("WP", "https://www.wikipathways.org/index.php/Pathway:")

  # Disease and phenotype ontologies
  attachLinks("DO", "http://www.informatics.jax.org/disease/")
  attachLinks("HP", "https://monarchinitiative.org/")
  attachLinks("ORPHA", "https://www.orpha.net/consor/cgi-bin/OC_Exp.php?Lng=GB&Expert=", gSub = "ORPHA:")
  attachLinks("WBP", "https://wormbase.org/species/all/phenotype/")
  attachLinks("WBBT", "https://wormbase.org/species/all/anatomy_term/")
  attachLinks("MGI", "https://www.informatics.jax.org/vocab/mp_ontology/")

  # Tissue ontologies
  attachLinks("BTO", "https://www.ebi.ac.uk/ols/ontologies/bto/terms?iri=http%3A%2F%2Fpurl.obolibrary.org%2Fobo%2FBTO_", gSub = "BTO:")

  # Regulatory elements
  attachLinks("MIRNA", "https://www.mirbase.org/textsearch.shtml?q=", gSub = "MIRNA:")

  attachKEGGLinks()
}

attachLinks <- function(sourceId, url, stopChar = "$", gSub = NULL) {
  linksVector <-
    enrichmentResults[[currentType_Tool]][grepl(
      paste0("^", sourceId, stopChar), enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID
  if (length(linksVector) > 0) {
    gSubLinksVector <- linksVector
    if (!is.null(gSub))
      gSubLinksVector <- gsub(gSub, "", linksVector)
    enrichmentResults[[currentType_Tool]][grepl(
      paste0("^", sourceId, stopChar), enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID <<-
      paste0(
        "<a href='", url, gSubLinksVector, "' target='_blank'>",
        linksVector, "</a>"
      )
  }
}

attachKEGGLinks <- function() {
  # KEGG organism codes and supported organisms reference:
  # https://www.kegg.jp/kegg/tables/br08606.html
  # This table contains all KEGG-supported organisms and their corresponding
  # 3-4 letter organism codes (kegg_name) used in pathway URLs

  tempEnrichmentDF <-
    enrichmentResults[[currentType_Tool]][grepl(
      "^KEGG$", enrichmentResults[[currentType_Tool]]$Source
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
          urlSuffix <- paste0("+", geneHighlights)
        } else {
          linksVector <- tempEnrichmentDF$Term_ID
          urlSuffix <- ""  # No gene highlighting if conversion fails
        }
      } else {
        # No g:Profiler support - pathway links only
        linksVector <- tempEnrichmentDF$Term_ID
        urlSuffix <- ""  # No gene highlighting
      }

      enrichmentResults[[currentType_Tool]][grepl(
        "^KEGG$", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID <<-
        paste0(
          "<a href='https://www.kegg.jp/kegg-bin/show_pathway?",
          linksVector, urlSuffix,
          "' target='_blank'>", linksVector, "</a>"
        )

    } else {
      # Traditional tools (gProfiler, etc.) - need both short_name AND kegg_name
      if (!is.na(shortName) && !is.na(keggName)) {
        conversionTable <- createEntrezAccConversionTable(tempEnrichmentDF, shortName)
        if (!is.null(conversionTable)) {
          tempEnrichmentDFWithEntrezAcc <-
            convertPositiveHitsToEntrezAcc(tempEnrichmentDF, conversionTable)

          linksVector <- tempEnrichmentDFWithEntrezAcc$Term_ID
          enrichmentResults[[currentType_Tool]][grepl(
            "^KEGG$", enrichmentResults[[currentType_Tool]]$Source), ]$Term_ID <<-
            paste0(
              "<a href='https://www.kegg.jp/kegg-bin/show_pathway?",
              gsub("KEGG:|map", keggName, linksVector),
              "+", gsub(",", "+", tempEnrichmentDFWithEntrezAcc$`Positive Hits EntrezAcc`),
              "' target='_blank'>", linksVector, "</a>"
            )
        }
      }
      # If organism lacks short_name OR kegg_name, no KEGG links are created
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
  else
    conversionTable <- gprofiler2::gconvert(inputToConvert,
                                            organism = shortName,
                                            target = "ENTREZGENE_ACC")
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
