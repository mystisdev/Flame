# =============================================================================
# FLAME Infrastructure Configuration
# =============================================================================
#
# This file contains centralized enums and configuration for the FLAME app.
# All tools, paradigms, datasources, and other identifiers are defined here
# to ensure consistency and enable autocomplete across the codebase.
#
# Note: This config coexists with existing config variables (in config-*.R)
# during the migration to R6 classes.
# =============================================================================

# =============================================================================
# ENUMS - Centralized identifiers for type safety and autocomplete
# =============================================================================

#' Analyte Type
#' @description Valid types of biological analytes that can be analyzed
AnalyteType <- list(
  GENE = "GENE",
  METABOLITE = "METABOLITE",
  MIRNA = "MIRNA"
)

#' Score Direction
#' @description Indicates whether higher or lower scores are better for ranked analytes
ScoreDirection <- list(
  ASCENDING = "ASCENDING",
  DESCENDING = "DESCENDING"
)

#' Module IDs
#' @description Shiny module namespace IDs - must match between UI and server
#' Used by: ui.R, view-input.R (UI) and server.R (session constructors)
#'
#' Naming convention:
#'   - INPUT_X matches file input-session-X.R
#'   - LISTMGMT_X matches file listmgmt-session-*.R
#'   - UTILITIES_X matches file utilities-session-*.R
ModuleIds <- list(
  # Input sessions (input-session-*.R)
  INPUT_LIST = "input_list",
  INPUT_VOLCANO = "input_volcano",
  INPUT_REDUCTION = "input_reduction",
  INPUT_SNPS = "input_snps",
  INPUT_TEXTMINING = "input_textmining",
  # List management sessions (listmgmt-session-*.R)
  LISTMGMT_MANAGER = "listmgmt_manager",
  LISTMGMT_SETOPS = "listmgmt_setops",
  # Utility sessions (utilities-session-*.R)
  UTILITIES_CONVERSION = "gconvert",
  UTILITIES_ORTHOLOGY = "gorth",
  UTILITIES_NETWORK = "string_network",
  # Enrichment sessions (enrich-*.R)
  ENRICH_FORM = "enrich_form"
)

#' Paradigm Identifier
#' @description Types of enrichment analysis
ParadigmId <- list(
  ORA = "ORA",
  GSEA = "GSEA",
  TOPOLOGY = "TOPOLOGY"
)

#' Tool Identifier
#' @description Available enrichment tools
ToolId <- list(
  GPROFILER = "gProfiler",
  STRING = "STRING",
  ENRICHR = "enrichR",
  WEBGESTALT = "WebGestalt",
  PANTHER = "PANTHER",
  GENECODIS = "GeneCodis"
)

#' Output Type
#' @description Available visualization outputs
#' Heatmaps and Networks share the same 3 relationship types:
#' - TERM_GENE: Terms connected to their genes
#' - TERM_TERM: Terms connected by similarity
#' - GENE_GENE: Genes connected by shared terms
OutputType <- list(
  BARCHART = "Barchart",
  SCATTER = "Scatter",
  DOTPLOT = "DotPlot",
  HEATMAP_TERM_GENE = "Heatmap_TermGene",
  HEATMAP_TERM_TERM = "Heatmap_TermTerm",
  HEATMAP_GENE_GENE = "Heatmap_GeneGene",
  NETWORK_TERM_GENE = "Network_TermGene",
  NETWORK_TERM_TERM = "Network_TermTerm",
  NETWORK_GENE_GENE = "Network_GeneGene",
  MANHATTAN = "Manhattan"
)

# =============================================================================
# PARADIGMS - What each analysis type needs and produces
# =============================================================================
#
# Fields:
#   name              - Display name for the paradigm
#   requiredInputClass - R6 class name required (e.g., "RankedAnalyteList"), NULL for any
#   outputs           - Character vector of OutputType values this paradigm supports
#   requiresBackground - Whether a background gene set is required
#
# =============================================================================

PARADIGMS <- list()

PARADIGMS[[ParadigmId$ORA]] <- list(
  name = "Over-Representation Analysis",
  requiredInputClass = NULL,
  outputs = c(
    OutputType$BARCHART,
    OutputType$SCATTER,
    OutputType$DOTPLOT,
    OutputType$HEATMAP_TERM_GENE,
    OutputType$HEATMAP_TERM_TERM,
    OutputType$HEATMAP_GENE_GENE,
    OutputType$NETWORK_TERM_GENE,
    OutputType$NETWORK_TERM_TERM,
    OutputType$NETWORK_GENE_GENE,
    OutputType$MANHATTAN
  ),
  requiresBackground = FALSE
)

# NOTE: GSEA is not yet implemented. Fields set to NULL as placeholders.
PARADIGMS[[ParadigmId$GSEA]] <- list(
  name = "Gene Set Enrichment Analysis",
  requiredInputClass = NULL,
  outputs = NULL,
  requiresBackground = NULL
)

# NOTE: Topology is not yet implemented. Fields set to NULL as placeholders.
PARADIGMS[[ParadigmId$TOPOLOGY]] <- list(
  name = "Topology-Based Enrichment",
  requiredInputClass = NULL,
  outputs = NULL,
  requiresBackground = NULL
)

# =============================================================================
# TOOLS - Consolidated tool configuration
# =============================================================================
#
# Each tool has the following fields:
#
#   name                  - Display name shown in UI
#   paradigms             - Character vector of ParadigmId values this tool supports
#
#   datasources           - Character vector of datasource display names (e.g., "GO:MF", "KEGG")
#                           For tools with organism-specific datasources, use organismDatasources instead
#   datasourceCodes       - Named list mapping display names to API codes
#                           Example: list("GO:MF" = "geneontology_Molecular_Function")
#   organismDatasources   - Named list of datasources per organism short_name (for enrichR, GeneCodis)
#   organismDatasourceCodes - Named list of datasource codes per organism (for enrichR)
#
#   namespaces            - Named list of gene ID namespace options
#                           Example: list("Entrez Gene Name" = "ENTREZGENE")
#   namespacesSpecial     - Named list of namespaces per special organism (overrides namespaces)
#
#   metrics               - Named list of significance metric options
#                           Example: list("False discovery rate" = "fdr")
#   defaultMetricGenome   - Default metric when using genome background
#   defaultMetricBackground - Default metric when using custom background
#
#   supportsBackground    - Whether tool supports custom background gene sets
#   hasManhattanPlot      - Whether tool provides Manhattan plot data
#
#   organisms             - Reserved for organism list (loaded separately from RDS files)
#
# =============================================================================

TOOLS <- list()

# -----------------------------------------------------------------------------
# gProfiler
# -----------------------------------------------------------------------------
TOOLS[[ToolId$GPROFILER]] <- list(
  name = "g:Profiler",
  paradigms = c(ParadigmId$ORA),

  datasources = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP",
                  "TF", "MIRNA", "CORUM", "HPA", "HP"),

  datasourceCodes = NULL,

  namespaces = list(
    "User Input" = "USERINPUT",
    "ENSEMBL Gene ID" = "ENSG",
    "ENSEMBL Protein ID" = "ENSP",
    "ENSEMBL Transcript ID" = "ENST",
    "Entrez Gene Name" = "ENTREZGENE",
    "Entrez Gene Accession" = "ENTREZGENE_ACC",
    "Entrez Gene Transcript Name" = "ENTREZGENE_TRANS_NAME",
    "UniProt Gene Name" = "UNIPROT_GN",
    "UniProt Accession" = "UNIPROT_GN_ACC",
    "UniProt Archive" = "UNIPARC",
    "RefSeq Protein Accession" = "REFSEQ_PEPTIDE_ACC",
    "RefSeq mRNA" = "REFSEQ_MRNA",
    "RefSeq mRNA Accession" = "REFSEQ_MRNA_ACC",
    "RefSeq Non-coding RNA Accession" = "REFSEQ_NCRNA_ACC",
    "EMBL Accession" = "EMBL",
    "ChEMBL" = "CHEMBL",
    "WIKIGENE ID" = "WIKIGENE"
  ),

  metrics = list(
    "g:SCS threshold" = "gSCS",
    "False discovery rate" = "fdr",
    "Bonferroni correction" = "bonferroni"
  ),

  defaultMetricGenome = "fdr",
  defaultMetricBackground = "bonferroni",
  supportsBackground = TRUE,
  hasManhattanPlot = TRUE,

  organisms = NULL
)

# -----------------------------------------------------------------------------
# STRING
# -----------------------------------------------------------------------------
TOOLS[[ToolId$STRING]] <- list(
  name = "STRING",
  paradigms = c(ParadigmId$ORA),

  datasources = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP",
                  "INTERPRO", "PFAM", "UNIPROT", "DO", "BTO", "HP", "PUBMED"),

  datasourceCodes = list(
    "GO:MF" = "Function",
    "GO:CC" = "Component",
    "GO:BP" = "Process",
    "KEGG" = "KEGG",
    "REAC" = "RCTM",
    "WP" = "WikiPathways",
    "INTERPRO" = "InterPro",
    "PFAM" = "Pfam",
    "UNIPROT" = "Keyword",
    "DO" = "DISEASES",
    "BTO" = "TISSUES",
    "HP" = "HPO",
    "PUBMED" = "PMID"
  ),

  namespaces = list("ENSEMBL Protein ID" = "ENSP"),

  metrics = list(
    "False discovery rate" = "fdr",
    "P-value" = "p_value"
  ),

  defaultMetricGenome = "fdr",
  defaultMetricBackground = "p_value",
  supportsBackground = TRUE,
  hasManhattanPlot = FALSE,

  organisms = NULL
)

# -----------------------------------------------------------------------------
# enrichR - Has organism-specific datasources
# -----------------------------------------------------------------------------
TOOLS[[ToolId$ENRICHR]] <- list(
  name = "Enrichr",
  paradigms = c(ParadigmId$ORA),

  organismDatasources = list(
    hsapiens = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP", "PANTHER Pathways", "HP"),
    mmusculus = c("GO:MF", "GO:CC", "GO:BP", "WP", "MGI"),
    dmelanogaster = c("GO:MF", "GO:CC", "GO:BP", "WP"),
    drerio = c("GO:MF", "GO:CC", "GO:BP", "WP"),
    scerevisiae = c("GO:MF", "GO:CC", "GO:BP", "WP", "KEGG"),
    celegans = c("GO:MF", "GO:CC", "GO:BP", "WP", "DO", "WBP", "WBBT"),
    btaurus = c("GO:MF", "GO:CC", "GO:BP", "REAC", "WP", "ORPHA", "HP", "MGI")
  ),

  organismDatasourceCodes = list(
    hsapiens = list(
      "GO:MF" = "GO_Molecular_Function_2021",
      "GO:CC" = "GO_Cellular_Component_2021",
      "GO:BP" = "GO_Biological_Process_2021",
      "KEGG" = "KEGG_2016",
      "REAC" = "Reactome_2022",
      "WP" = "WikiPathway_2021_Human",
      "PANTHER Pathways" = "Panther_2016",
      "HP" = "Human_Phenotype_Ontology"
    ),
    mmusculus = list(
      "GO:MF" = "GO_Molecular_Function_2021",
      "GO:CC" = "GO_Cellular_Component_2021",
      "GO:BP" = "GO_Biological_Process_2021",
      "WP" = "WikiPathways_2019_Mouse",
      "MGI" = "KOMP2_Mouse_Phenotypes_2022"
    ),
    dmelanogaster = list(
      "GO:MF" = "GO_Molecular_Function_2018",
      "GO:CC" = "GO_Cellular_Component_2018",
      "GO:BP" = "GO_Biological_Process_2018",
      "WP" = "WikiPathways_2018"
    ),
    drerio = list(
      "GO:MF" = "GO_Molecular_Function_2018",
      "GO:CC" = "GO_Cellular_Component_2018",
      "GO:BP" = "GO_Biological_Process_2018",
      "WP" = "WikiPathways_2018"
    ),
    scerevisiae = list(
      "GO:MF" = "GO_Molecular_Function_2018",
      "GO:CC" = "GO_Cellular_Component_2018",
      "GO:BP" = "GO_Biological_Process_2018",
      "WP" = "WikiPathways_2018",
      "KEGG" = "KEGG_2018"
    ),
    celegans = list(
      "GO:MF" = "GO_Molecular_Function_2018",
      "GO:CC" = "GO_Cellular_Component_2018",
      "GO:BP" = "GO_Biological_Process_2018",
      "WP" = "WikiPathways_2018",
      "DO" = "Human_Diseases_from_WormBase_2018",
      "WBP" = "Phenotypes_WormBase_2018",
      "WBBT" = "Anatomic_Associations_WormBase_2018"
    ),
    btaurus = list(
      "GO:MF" = "GO_Molecular_Function_2021",
      "GO:CC" = "GO_Cellular_Component_2021",
      "GO:BP" = "GO_Biological_Process_2021",
      "REAC" = "Reactome_2016",
      "WP" = "WikiPathway_2021_Human",
      "ORPHA" = "Orphanet_Augmented_2021",
      "HP" = "Human_Phenotype_Ontology",
      "MGI" = "MGI_Mammalian_Phenotype_Level_4_2019"
    )
  ),

  organismSites = list(
    hsapiens = "Enrichr",
    mmusculus = "Enrichr",
    dmelanogaster = "FlyEnrichr",
    drerio = "FishEnrichr",
    scerevisiae = "YeastEnrichr",
    celegans = "WormEnrichr",
    btaurus = "OxEnrichr"
  ),

  namespaces = list(
    "Entrez Gene Name" = "ENTREZGENE",
    "User Input" = "USERINPUT"
  ),

  namespacesSpecial = list(
    dmelanogaster = list("User Input" = "USERINPUT"),
    scerevisiae = list("User Input" = "USERINPUT")
  ),

  metrics = list("Adjusted P-value" = "adjusted_pvalue"),

  defaultMetricGenome = "adjusted_pvalue",
  defaultMetricBackground = NULL,
  supportsBackground = FALSE,
  hasManhattanPlot = FALSE,

  organisms = NULL
)

# -----------------------------------------------------------------------------
# WebGestalt
# -----------------------------------------------------------------------------
TOOLS[[ToolId$WEBGESTALT]] <- list(
  name = "WebGestalt",
  paradigms = c(ParadigmId$ORA, ParadigmId$GSEA),

  datasources = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP",
                  "PANTHER Pathways", "DISGENET", "OMIM", "GLAD4U_DISEASE",
                  "DRUGBANK", "GLAD4U_DRUG", "HP"),

  datasourceCodes = list(
    "GO:MF" = "geneontology_Molecular_Function_noRedundant",
    "GO:CC" = "geneontology_Cellular_Component_noRedundant",
    "GO:BP" = "geneontology_Biological_Process_noRedundant",
    "KEGG" = "pathway_KEGG",
    "REAC" = "pathway_Reactome",
    "WP" = "pathway_Wikipathway",
    "PANTHER Pathways" = "pathway_Panther",
    "DISGENET" = "disease_Disgenet",
    "OMIM" = "disease_OMIM",
    "GLAD4U_DISEASE" = "disease_GLAD4U",
    "DRUGBANK" = "drug_DrugBank",
    "GLAD4U_DRUG" = "drug_GLAD4U",
    "HP" = "phenotype_Human_Phenotype_Ontology"
  ),

  namespaces = list(
    "Entrez Gene Accession" = "ENTREZGENE_ACC",
    "User Input" = "USERINPUT"
  ),

  metrics = list(
    "Benjamini-Hochberg" = "BH",
    "Benjamini-Yekutieli" = "BY",
    "Holm" = "holm",
    "Hochberg" = "hochberg",
    "Hommel" = "hommel",
    "Bonferroni adjustment" = "bonferroni",
    "Top 100" = "top"
  ),

  defaultMetricGenome = "BH",
  defaultMetricBackground = "top",
  supportsBackground = TRUE,
  hasManhattanPlot = FALSE,

  organisms = NULL
)

# -----------------------------------------------------------------------------
# PANTHER
# -----------------------------------------------------------------------------
TOOLS[[ToolId$PANTHER]] <- list(
  name = "PANTHER",
  paradigms = c(ParadigmId$ORA),

  datasources = c("GO:MF", "GO:CC", "GO:BP", "REAC",
                  "GOSLIM:MF", "GOSLIM:CC", "GOSLIM:BP",
                  "PANTHER Pathways", "PANTHERPC"),

  datasourceCodes = list(
    "GO:MF" = "GO:0003674",
    "GO:CC" = "GO:0005575",
    "GO:BP" = "GO:0008150",
    "GOSLIM:MF" = "ANNOT_TYPE_ID_PANTHER_GO_SLIM_MF",
    "GOSLIM:CC" = "ANNOT_TYPE_ID_PANTHER_GO_SLIM_CC",
    "GOSLIM:BP" = "ANNOT_TYPE_ID_PANTHER_GO_SLIM_BP",
    "REAC" = "ANNOT_TYPE_ID_REACTOME_PATHWAY",
    "PANTHER Pathways" = "ANNOT_TYPE_ID_PANTHER_PATHWAY",
    "PANTHERPC" = "ANNOT_TYPE_ID_PANTHER_PC"
  ),

  namespaces = list("PANTHER Accession" = "PANTHER_ACC"),

  metrics = list(
    "False discovery rate" = "FDR",
    "P-value" = "NONE",
    "Bonferroni" = "BONFERRONI"
  ),

  defaultMetricGenome = "FDR",
  defaultMetricBackground = "NONE",
  supportsBackground = TRUE,
  hasManhattanPlot = FALSE,

  organisms = NULL
)

# -----------------------------------------------------------------------------
# GeneCodis - Has organism-specific datasources
# -----------------------------------------------------------------------------
TOOLS[[ToolId$GENECODIS]] <- list(
  name = "GeneCodis",
  paradigms = c(ParadigmId$ORA),

  organismDatasources = list(
    hsapiens = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP", "PANTHER Pathways",
                 "HP", "OMIM", "MGI", "BioPlanet", "PharmGKB", "LINCS",
                 "CollecTRI", "MIRNA"),
    mmusculus = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP", "PANTHER Pathways",
                  "MGI", "CollecTRI", "MIRNA"),
    rnorvegicus = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP", "PANTHER Pathways",
                    "MGI", "CollecTRI", "MIRNA"),
    celegans = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP", "PANTHER Pathways"),
    dmelanogaster = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP", "PANTHER Pathways"),
    drerio = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP", "PANTHER Pathways", "MGI"),
    clfamiliaris = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP", "PANTHER Pathways"),
    ggallus = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP", "PANTHER Pathways"),
    btaurus = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP", "PANTHER Pathways"),
    sscrofa = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC"),
    athaliana = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "WP", "PANTHER Pathways"),
    osativa = c("GO:MF", "GO:CC", "GO:BP", "KEGG"),
    scerevisiae = c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP", "PANTHER Pathways"),
    ecoli = c("GO:MF", "GO:CC", "GO:BP", "KEGG")
  ),

  datasourceCodes = list(
    "GO:MF" = "GO_MF",
    "GO:CC" = "GO_CC",
    "GO:BP" = "GO_BP",
    "KEGG" = "KEGG",
    "REAC" = "Reactome",
    "WP" = "WikiPathways",
    "PANTHER Pathways" = "Panther",
    "HP" = "HPO",
    "OMIM" = "OMIM",
    "MGI" = "MGI",
    "BioPlanet" = "BioPlanet",
    "PharmGKB" = "PharmGKB",
    "LINCS" = "LINCS",
    "CollecTRI" = "CollecTRI",
    "MIRNA" = "miRTarBase"
  ),

  namespaces = list("User Input" = "USERINPUT"),

  metrics = list("False discovery rate" = "fdr"),

  defaultMetricGenome = "fdr",
  defaultMetricBackground = "fdr",
  supportsBackground = TRUE,
  hasManhattanPlot = FALSE,

  organisms = NULL
)

# =============================================================================
# ADDITIONAL NAMESPACES - Extra gene ID namespaces for specific organisms
# =============================================================================
#
# Some organisms have special gene ID formats that need to be available
# in addition to the standard namespaces. These are prepended to the
# namespace dropdown when the organism is selected.
#
# Note: This only defines AVAILABILITY, not UI defaults. Users select
# whichever namespace they need. Wrong selections result in graceful
# failures (empty results with warning).
#
# =============================================================================

ADDITIONAL_NAMESPACES <- list(
  amellifera = list(
    gProfiler = list("BeeBase ID" = "BEEBASE")
  ),
  dmelanogaster = list(
    gProfiler = list("FlyBase Gene ID" = "FLYBASE_GENE_ID")
  )
)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Get tools that support a given paradigm
#' @param paradigmId A paradigm identifier from ParadigmIds
#' @return Character vector of tool IDs
getToolsForParadigm <- function(paradigmId) {
  names(Filter(function(t) paradigmId %in% t$paradigms, TOOLS))
}

#' Get datasources for a tool, optionally filtered by organism
#' @param toolId A tool identifier from ToolIds
#' @param organism Optional organism short_name for organism-specific tools
#' @return Character vector of datasource identifiers
getDatasourcesForTool <- function(toolId, organism = NULL) {
  tool <- TOOLS[[toolId]]
  if (is.null(tool)) return(NULL)

  if (!is.null(tool$organismDatasources) && !is.null(organism)) {
    return(tool$organismDatasources[[organism]])
  }

  return(tool$datasources)
}

#' Get datasource code for API call
#' @param toolId A tool identifier from ToolIds
#' @param datasource A datasource display name
#' @param organism Optional organism for organism-specific tools
#' @return The API code for the datasource, or the datasource itself if no mapping
getDatasourceCode <- function(toolId, datasource, organism = NULL) {
  tool <- TOOLS[[toolId]]
  if (is.null(tool)) return(datasource)

  if (!is.null(tool$organismDatasourceCodes) && !is.null(organism)) {
    codes <- tool$organismDatasourceCodes[[organism]]
    if (!is.null(codes) && !is.null(codes[[datasource]])) {
      return(codes[[datasource]])
    }
  }

  if (!is.null(tool$datasourceCodes) && !is.null(tool$datasourceCodes[[datasource]])) {
    return(tool$datasourceCodes[[datasource]])
  }

  return(datasource)
}

#' Get outputs available for a paradigm
#' @param paradigmId A paradigm identifier from ParadigmIds
#' @return Character vector of output types
getOutputsForParadigm <- function(paradigmId) {
  paradigm <- PARADIGMS[[paradigmId]]
  if (is.null(paradigm)) return(NULL)
  return(paradigm$outputs)
}

#' Get default metric for a tool
#' @param toolId A tool identifier from ToolIds
#' @param hasBackground Whether a custom background is being used
#' @return The default metric code
getDefaultMetric <- function(toolId, hasBackground = FALSE) {
  tool <- TOOLS[[toolId]]
  if (is.null(tool)) return(NULL)

  if (hasBackground && !is.null(tool$defaultMetricBackground)) {
    return(tool$defaultMetricBackground)
  }

  return(tool$defaultMetricGenome)
}

#' Check if an organism has additional namespaces for a tool
#' @param organismShortName The organism short_name
#' @param toolId The tool identifier
#' @return Logical
hasAdditionalNamespaces <- function(organismShortName, toolId = NULL) {
  if (is.null(toolId)) {
    return(organismShortName %in% names(ADDITIONAL_NAMESPACES))
  }
  orgConfig <- ADDITIONAL_NAMESPACES[[organismShortName]]
  if (is.null(orgConfig)) return(FALSE)
  return(toolId %in% names(orgConfig))
}

#' Get additional namespaces for an organism and tool
#' @param organismShortName The organism short_name
#' @param toolId The tool identifier
#' @return Named list of additional namespaces, or NULL if none
getAdditionalNamespaces <- function(organismShortName, toolId) {
  orgConfig <- ADDITIONAL_NAMESPACES[[organismShortName]]
  if (is.null(orgConfig)) return(NULL)
  return(orgConfig[[toolId]])
}
