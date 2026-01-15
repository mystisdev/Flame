# =============================================================================
# FLAME Configuration
# =============================================================================
#
# Single source of truth for all configuration in the FLAME app.
# Consolidates enums, tool configs, UI constants, and helper functions.
#
# Contents:
#   1. ENUMS - Type-safe identifiers (ToolId, ParadigmId, etc.)
#   2. PARADIGMS - Analysis paradigm configurations
#   3. TOOLS - Complete tool configurations (datasources, namespaces, metrics)
#   4. ORGANISMS - Organism data (loaded from RDS)
#   5. UI CONSTANTS - Colors, layouts, tab names, display structures
#   6. ENRICHMENT TYPES - Enrichment type configurations
#   7. HELPER FUNCTIONS - Config access functions
#
# =============================================================================

# =============================================================================
# 1. ENUMS - Centralized identifiers for type safety and autocomplete
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
  NETWORK_GENE_GENE = "Network_GeneGene"
)

# =============================================================================
# 2. PARADIGMS - What each analysis type needs and produces
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
    OutputType$NETWORK_GENE_GENE
  ),
  requiresBackground = FALSE
)

PARADIGMS[[ParadigmId$GSEA]] <- list(
  name = "Gene Set Enrichment Analysis",
  requiredInputClass = NULL,
  outputs = NULL,
  requiresBackground = NULL
)

PARADIGMS[[ParadigmId$TOPOLOGY]] <- list(
  name = "Topology-Based Enrichment",
  requiredInputClass = NULL,
  outputs = NULL,
  requiresBackground = NULL
)

# =============================================================================
# 2b. OUTPUT_TYPES_CONFIG - Maps OutputType to implementation details
# =============================================================================
#
# Bridges semantic OutputType enum values to:
#   - class: R6 class name (string) for registry lookup
#   - containerId: Container div ID suffix
#   - key: Storage key in session's .outputSessions list
#   - tabTitle: Display title (for top-level tabs)
#   - group: Parent tab group (NULL for top-level, "Heatmap"/"Network" for grouped)
#   - subTabTitle: Title within group (%s replaced with uiTermKeyword)

OUTPUT_TYPES_CONFIG <- list()

# Top-level tabs
OUTPUT_TYPES_CONFIG[[OutputType$BARCHART]] <- list(
  class = "BarchartOutputSession",
  containerId = "barchart_container",
  key = "barchart",
  tabTitle = "Barchart",
  group = NULL
)

OUTPUT_TYPES_CONFIG[[OutputType$SCATTER]] <- list(
  class = "ScatterOutputSession",
  containerId = "scatterPlot_container",
  key = "scatter",
  tabTitle = "Scatter Plot",
  group = NULL
)

OUTPUT_TYPES_CONFIG[[OutputType$DOTPLOT]] <- list(
  class = "DotPlotOutputSession",
  containerId = "dotPlot_container",
  key = "dotplot",
  tabTitle = "Dot Plot",
  group = NULL
)

# Heatmap group (3 sub-tabs)
OUTPUT_TYPES_CONFIG[[OutputType$HEATMAP_TERM_GENE]] <- list(
  class = "Heatmap1OutputSession",
  containerId = "heatmap1_container",
  key = "heatmap1",
  group = "Heatmap",
  subTabTitle = "%s Vs Genes"
)

OUTPUT_TYPES_CONFIG[[OutputType$HEATMAP_TERM_TERM]] <- list(
  class = "Heatmap2OutputSession",
  containerId = "heatmap2_container",
  key = "heatmap2",
  group = "Heatmap",
  subTabTitle = "%s Vs %s"
)

OUTPUT_TYPES_CONFIG[[OutputType$HEATMAP_GENE_GENE]] <- list(
  class = "Heatmap3OutputSession",
  containerId = "heatmap3_container",
  key = "heatmap3",
  group = "Heatmap",
  subTabTitle = "Genes Vs Genes"
)

# Network group (3 sub-tabs)
OUTPUT_TYPES_CONFIG[[OutputType$NETWORK_TERM_GENE]] <- list(
  class = "Network1OutputSession",
  containerId = "network1_container",
  key = "network1",
  group = "Network",
  subTabTitle = "%s Vs Genes"
)

OUTPUT_TYPES_CONFIG[[OutputType$NETWORK_TERM_TERM]] <- list(
  class = "Network2OutputSession",
  containerId = "network2_container",
  key = "network2",
  group = "Network",
  subTabTitle = "%s Vs %s"
)

OUTPUT_TYPES_CONFIG[[OutputType$NETWORK_GENE_GENE]] <- list(
  class = "Network3OutputSession",
  containerId = "network3_container",
  key = "network3",
  group = "Network",
  subTabTitle = "Genes Vs Genes"
)

# =============================================================================
# 3. TOOLS - Consolidated tool configuration
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
#   defaultNamespace      - Default namespace for this tool
#
#   metrics               - Named list of significance metric options
#                           Example: list("False discovery rate" = "fdr")
#   defaultMetricGenome   - Default metric when using genome background
#   defaultMetricBackground - Default metric when using custom background
#
#   supportsBackground    - Whether tool supports custom background gene sets
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

  defaultNamespace = "USERINPUT",

  metrics = list(
    "g:SCS threshold" = "gSCS",
    "False discovery rate" = "fdr",
    "Bonferroni correction" = "bonferroni"
  ),

  defaultMetricGenome = "fdr",
  defaultMetricBackground = "bonferroni",
  supportsBackground = TRUE
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

  defaultNamespace = "ENSP",

  metrics = list(
    "False discovery rate" = "fdr",
    "P-value" = "p_value"
  ),

  defaultMetricGenome = "fdr",
  defaultMetricBackground = "p_value",
  supportsBackground = TRUE
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

  defaultNamespace = "ENTREZGENE",

  metrics = list("Adjusted P-value" = "adjusted_pvalue"),

  defaultMetricGenome = "adjusted_pvalue",
  defaultMetricBackground = NULL,
  supportsBackground = FALSE
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

  defaultNamespace = "ENTREZGENE_ACC",

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
  supportsBackground = TRUE
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

  defaultNamespace = "PANTHER_ACC",

  metrics = list(
    "False discovery rate" = "FDR",
    "P-value" = "NONE",
    "Bonferroni" = "BONFERRONI"
  ),

  defaultMetricGenome = "FDR",
  defaultMetricBackground = "NONE",
  supportsBackground = TRUE
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

  defaultNamespace = "USERINPUT",

  metrics = list("False discovery rate" = "fdr"),

  defaultMetricGenome = "fdr",
  defaultMetricBackground = "fdr",
  supportsBackground = TRUE
)

# =============================================================================
# 4. ORGANISMS - Loaded from RDS files at package load time
# =============================================================================

ORGANISMS <- tryCatch(
  readRDS(getOrganismsPath("organismsDF.RDS")),
  error = function(e) readRDS("./organisms/organismsDF.RDS")
)

TOOL_ORGANISMS <- tryCatch(
  readRDS(getOrganismsPath("toolOrganismsList.RDS")),
  error = function(e) readRDS("./organisms/toolOrganismsList.RDS")
)

# Special organisms with non-standard tool/namespace preferences
SPECIAL_ORGANISMS <- c("amellifera", "dmelanogaster")

SPECIAL_PREFERRED_TOOL <- list(
  amellifera = ToolId$GPROFILER,
  dmelanogaster = ToolId$ENRICHR
)

SPECIAL_PREFERRED_NAMESPACE <- list(
  amellifera = "BEEBASE",
  dmelanogaster = "USERINPUT"
)

# Additional namespaces available for specific organisms (per-tool)
ADDITIONAL_NAMESPACES <- list(
  amellifera = list(
    gProfiler = list("BeeBase ID" = "BEEBASE")
  ),
  dmelanogaster = list(
    gProfiler = list("FlyBase Gene ID" = "FLYBASE_GENE_ID")
  )
)

# Core namespaces shared across tools (fallback when tool has no specific namespaces)
CORE_NAMESPACES <- list(
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
)

# Organism-specific namespaces (flattened for conversion utility)
ORGANISM_NAMESPACES <- list(
  amellifera = c("BeeBase ID" = "BEEBASE"),
  dmelanogaster = c("FlyBase Gene ID" = "FLYBASE_GENE_ID")
)

# =============================================================================
# 5. UI CONSTANTS - Colors, layouts, display structures
# =============================================================================

# -----------------------------------------------------------------------------
# Tab identification
# -----------------------------------------------------------------------------
TAB_NAMES <- list(
  "ALL" = "all",
  "GO:MF" = "gomf", "GO:CC" = "gocc", "GO:BP" = "gobp",
  "GOSLIM:MF" = "goslimmf", "GOSLIM:CC" = "goslimcc", "GOSLIM:BP" = "goslimbp",
  "KEGG" = "kegg", "REAC" = "reac", "WP" = "wp", "PANTHER Pathways" = "panther",
  "INTERPRO" = "interpro", "PFAM" = "pfam", "UNIPROT" = "uniprot", "PANTHERPC" = "pantherpc",
  "DO" = "do", "DISGENET" = "disgenet", "OMIM" = "omim", "GLAD4U_DISEASE" = "glad4udisease", "ORPHA" = "orpha",
  "DRUGBANK" = "drugbank", "GLAD4U_DRUG" = "glad4udrug",
  "BTO" = "brenda", "WBBT" = "wbbt", "TF" = "tf", "CollecTRI" = "collectri",
  "MIRNA" = "mirna", "CORUM" = "corum",
  "HPA" = "hpa", "HP" = "hp", "WBP" = "wbp", "MGI" = "mgi",
  "BioPlanet" = "bioplanet", "PharmGKB" = "pharmgkb", "LINCS" = "lincs",
  "PUBMED" = "pubmed"
)
TAB_NAMES_CODES <- as.character(TAB_NAMES)
ENRICHMENT_DATASOURCES <- names(TAB_NAMES[TAB_NAMES != "all"])

# -----------------------------------------------------------------------------
# Datasource display structure for dropdowns (grouped)
# -----------------------------------------------------------------------------
DATASOURCES_PRINT <- list(
  'Gene Ontology' = list(
    "Molecular Function (GO:MF)" = "GO:MF",
    "Cellular Component (GO:CC)" = "GO:CC",
    "Biological Process (GO:BP)" = "GO:BP",
    "GO Slim - Molecular Function" = "GOSLIM:MF",
    "GO Slim - Cellular Component" = "GOSLIM:CC",
    "GO Slim - Biological Process" = "GOSLIM:BP"
  ),
  'Biological Pathways' = list(
    "KEGG" = "KEGG", "Reactome" = "REAC",
    "WikiPathways" = "WP", "PANTHER Pathways" = "PANTHER Pathways", "BioPlanet" = "BioPlanet"
  ),
  'Diseases' = list(
    "Disease Ontology" = "DO", "DisGeNET" = "DISGENET",
    "OMIM" = "OMIM", "GLAD4U" = "GLAD4U_DISEASE", "Orphanet" = "ORPHA"
  ),
  'Proteins' = list(
    "Interpro" = "INTERPRO", "PFAM" = "PFAM",
    "UniProt keywords" = "UNIPROT",
    "PANTHER Protein Class" = "PANTHERPC",
    "CORUM" = "CORUM"
  ),
  'Phenotypes' = list(
    "Human Phenotype Ontology" = "HP",
    "MGI Mammalian Phenotype" = "MGI",
    "WormBase Phenotypes" = "WBP"
  ),
  'Tissues' = list(
    "Human Protein Atlas (HPA)" = "HPA",
    "Brenda Tissue Ontology" = "BTO",
    "WormBase Anatomic Associations Ontology" = "WBBT"
  ),
  'Drugs' = list(
    "DrugBank" = "DRUGBANK",
    "GLAD4U" = "GLAD4U_DRUG",
    "PharmGKB" = "PharmGKB",
    "LINCS" = "LINCS"
  ),
  'Regulatory motifs in DNA' = list(
    "TRANSFAC" = "TF", "CollecTRI TFs" = "CollecTRI", "miRTarBase" = "MIRNA"
  ),
  'Literature' = list(
    "PubMed Publications" = "PUBMED"
  )
)

DATASOURCES_DEFAULT_SELECTED <- c("GO:MF", "GO:CC", "GO:BP", "KEGG")

# -----------------------------------------------------------------------------
# Plot configuration
# -----------------------------------------------------------------------------
NETWORK_IDS <- c("network1", "network2", "network3")
HEATMAP_IDS <- c("heatmap1", "heatmap2", "heatmap3")

LAYOUT_CHOICES <- list(
  `layout_with_graphopt` = "Graph Opt",
  `layout_nicely` = "Fruchterman-Reingold",
  `layout_with_kk` = "Kamada-Kawai",
  `layout_with_mds` = "Multi-dimensional Scaling",
  `layout_as_tree` = "Tree",
  `layout_as_star` = "Star",
  `layout_in_circle` = "Circle",
  `layout_on_grid` = "Grid",
  `layout_randomly` = "Random"
)

# -----------------------------------------------------------------------------
# Colors
# -----------------------------------------------------------------------------
GENE_NODE_COLOR <- "#d1e1d9"

DATASOURCE_COLORS <- c(
  "GO:MF" = "#dc3912", "GO:BP" = "#ff9900", "GO:CC" = "#109618",
  "GOSLIM:MF" = "#e85d4a", "GOSLIM:BP" = "#ffb84d", "GOSLIM:CC" = "#4db34d",
  "KEGG" = "#dd4477", "REAC" = "#3366cc", "WP" = "#0099c6", "PANTHER Pathways" = "#634341",
  "INTERPRO" = "#8a5103", "PFAM" = "#b3b000", "UNIPROT" = "#55edeb", "PANTHERPC" = "#8b6f47",
  "DO" = "#f7c8fa", "DISGENET" = "#c0f0a1", "OMIM" = "#edebaf",
  "GLAD4U_DISEASE" = "#9f86d9", "ORPHA" = "#03fcc6", "DRUGBANK" = "#7d4a74", "GLAD4U_DRUG" = "#4a9091",
  "BTO" = "#f0d871", "WBBT" = "#9cb59c", "TF" = "#5574a6", "CollecTRI" = "#A67C52", "MIRNA" = "#22aa99",
  "CORUM" = "#66aa00", "HPA" = "#6633cc", "HP" = "#990099", "WBP" = "#fffd78", "MGI" = "#fc4503",
  "BioPlanet" = "#9370DB", "PharmGKB" = "#FF6B6B", "LINCS" = "#4ECDC4",
  "PUBMED" = "#cc9f9f", "GENE" = GENE_NODE_COLOR
)

# -----------------------------------------------------------------------------
# Table format
# -----------------------------------------------------------------------------
ENRICHMENT_DF_COLNAMES <- c(
  "Source", "Term_ID", "Function", "P-value", "Term Size",
  "Query size", "Intersection Size", "Positive Hits"
)

# -----------------------------------------------------------------------------
# UI fallback strings
# -----------------------------------------------------------------------------
DEFAULT_NAMESPACE_TEXT <- "Default tool namespace conversions"
DEFAULT_METRIC_TEXT <- "Default tool metrics"
UI_TERM_KEYWORD <- list(functional = "functions")

# =============================================================================
# 6. ENRICHMENT TYPES - Configuration for different enrichment paradigms
# =============================================================================

DEFAULT_TOOL <- ToolId$STRING

ENRICHMENT_TYPES_CONFIG <- list(
  functional = list(
    id = "functional",
    label = "Functional Enrichment",
    tabsetPanelId = "toolTabsPanel",
    resultsPanelId = "functionalEnrichmentResultsPanel",
    clearButtonId = "enrich_form-enrichment_all_clear",
    closeEvent = "closeRunTab",
    datasources = ENRICHMENT_DATASOURCES,
    tools = c(ToolId$GPROFILER, ToolId$WEBGESTALT, ToolId$ENRICHR,
              ToolId$PANTHER, ToolId$GENECODIS),
    supportsCombination = TRUE
  )
)

# =============================================================================
# 7. HELPER FUNCTIONS
# =============================================================================

# -----------------------------------------------------------------------------
# Tool helpers
# -----------------------------------------------------------------------------

#' Get tools that support a given paradigm
#' @param paradigmId A paradigm identifier from ParadigmId
#' @return Character vector of tool IDs
getToolsForParadigm <- function(paradigmId) {
  names(Filter(function(t) paradigmId %in% t$paradigms, TOOLS))
}

#' Get tools that support a given organism
#' @param taxid Organism taxid
#' @return Character vector of tool IDs
getToolsForOrganism <- function(taxid) {
  names(which(sapply(TOOL_ORGANISMS, function(tool) taxid %in% tool)))
}

#' Get all tool IDs
#' @return Character vector of tool IDs
getAllToolIds <- function() {
  names(TOOLS)
}

# -----------------------------------------------------------------------------
# Datasource helpers
# -----------------------------------------------------------------------------

#' Get datasources for a tool, optionally filtered by organism
#' @param toolId A tool identifier from ToolId
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
#' @param toolId A tool identifier from ToolId
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

# -----------------------------------------------------------------------------
# Namespace helpers
# -----------------------------------------------------------------------------

#' Get namespaces for a tool
#' @param toolId Tool identifier
#' @param organismShortName Optional organism short name for special namespaces
#' @return Named list of namespaces
getNamespacesForTool <- function(toolId, organismShortName = NULL) {
  tool <- TOOLS[[toolId]]
  if (is.null(tool)) return(NULL)

  # Check for organism-specific overrides
  if (!is.null(tool$namespacesSpecial) && !is.null(organismShortName)) {
    special <- tool$namespacesSpecial[[organismShortName]]
    if (!is.null(special)) return(special)
  }

  # Get base namespaces
  namespaces <- tool$namespaces

  # Add additional namespaces for special organisms
  if (!is.null(organismShortName)) {
    additional <- getAdditionalNamespaces(organismShortName, toolId)
    if (!is.null(additional)) {
      namespaces <- c(additional, namespaces)
    }
  }

  return(namespaces)
}

#' Get default target namespace for a tool
#' @param toolId Tool identifier from ToolId
#' @param organism Optional organism taxid for organism-specific defaults
#' @return Namespace code string
getDefaultTargetNamespace <- function(toolId, organism = NULL) {
  tool <- TOOLS[[toolId]]
  if (is.null(tool)) return(NULL)

  # Check for organism-specific override (enrichR special cases)
  if (!is.null(organism) && toolId == ToolId$ENRICHR) {
    shortName <- ORGANISMS[ORGANISMS$taxid == organism, ]$short_name
    if (shortName %in% c("scerevisiae", "dmelanogaster")) {
      return("USERINPUT")
    }
  }

  return(tool$defaultNamespace)
}

#' Check if an organism has additional namespaces for a tool
#' @param organismShortName The organism short_name
#' @param toolId The tool identifier (optional)
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

# -----------------------------------------------------------------------------
# Metric helpers
# -----------------------------------------------------------------------------

#' Get metrics for a tool
#' @param toolId Tool identifier
#' @return Named list of metrics
getMetricsForTool <- function(toolId) {
  tool <- TOOLS[[toolId]]
  if (is.null(tool)) return(NULL)
  return(tool$metrics)
}

#' Get default metric for a tool
#' @param toolId Tool identifier from ToolId
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

# -----------------------------------------------------------------------------
# Organism helpers
# -----------------------------------------------------------------------------

#' Check if organism is a special organism
#' @param organismShortName Organism short name
#' @return Logical
isSpecialOrganism <- function(organismShortName) {
  organismShortName %in% SPECIAL_ORGANISMS
}

#' Get preferred tool for a special organism
#' @param organismShortName Organism short name
#' @return Tool ID or NULL
getPreferredToolForOrganism <- function(organismShortName) {
  SPECIAL_PREFERRED_TOOL[[organismShortName]]
}

#' Get preferred namespace for a special organism
#' @param organismShortName Organism short name
#' @return Namespace code or NULL
getPreferredNamespaceForOrganism <- function(organismShortName) {
  SPECIAL_PREFERRED_NAMESPACE[[organismShortName]]
}

# -----------------------------------------------------------------------------
# Paradigm/Output helpers
# -----------------------------------------------------------------------------

#' Get outputs available for a paradigm
#' @param paradigmId A paradigm identifier from ParadigmId
#' @return Character vector of output types
getOutputsForParadigm <- function(paradigmId) {
  paradigm <- PARADIGMS[[paradigmId]]
  if (is.null(paradigm)) return(NULL)
  return(paradigm$outputs)
}

# -----------------------------------------------------------------------------
# Enrichment type helpers
# -----------------------------------------------------------------------------

#' Get Enrichment Type Configuration
#' @param type Character string: "functional", etc.
#' @return List containing all configuration properties for the type
getEnrichmentConfig <- function(type) {
  config <- ENRICHMENT_TYPES_CONFIG[[type]]
  if (is.null(config)) {
    stop(paste("Unknown enrichment type:", type))
  }
  return(config)
}

#' Get Valid Datasources for Enrichment Type
#' @param type Character string: "functional", etc.
#' @return Character vector of valid datasource names
getValidDatasources <- function(type) {
  return(getEnrichmentConfig(type)$datasources)
}

#' Check if Enrichment Type Supports Combination
#' @param type Character string: "functional", etc.
#' @return Logical TRUE if combination tab is supported
supportsCombination <- function(type) {
  config <- getEnrichmentConfig(type)
  return(isTRUE(config$supportsCombination))
}

#' Get All Registered Enrichment Types
#' @return Character vector of registered type IDs
getEnrichmentTypes <- function() {
  return(names(ENRICHMENT_TYPES_CONFIG))
}

# =============================================================================
# 9. OUTPUT SESSION CLASS REGISTRY
# =============================================================================
#
# Maps class name strings to actual R6 class objects.
# Initialized lazily (classes must be defined before use).
# Used by ORAEnrichmentSession$createOutputSessions() for dynamic instantiation.

outputSessionClasses <- NULL

#' Initialize the OutputSession class registry
#'
#' Must be called after all output-session-*.R files are loaded.
#' Called from server.R at startup.
#' @export
initOutputSessionClasses <- function() {
  outputSessionClasses <<- list(
    BarchartOutputSession = BarchartOutputSession,
    ScatterOutputSession = ScatterOutputSession,
    DotPlotOutputSession = DotPlotOutputSession,
    Heatmap1OutputSession = Heatmap1OutputSession,
    Heatmap2OutputSession = Heatmap2OutputSession,
    Heatmap3OutputSession = Heatmap3OutputSession,
    Network1OutputSession = Network1OutputSession,
    Network2OutputSession = Network2OutputSession,
    Network3OutputSession = Network3OutputSession
  )
}

#' Get OutputSession class by name
#'
#' @param className String class name (e.g., "BarchartOutputSession")
#' @return R6 class generator
#' @export
getOutputSessionClass <- function(className) {
  if (is.null(outputSessionClasses)) {
    initOutputSessionClasses()
  }
  cls <- outputSessionClasses[[className]]
  if (is.null(cls)) {
    stop(paste("Unknown OutputSession class:", className))
  }
  return(cls)
}
