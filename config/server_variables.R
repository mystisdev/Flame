# Input ####
LISTNAME_NCHAR_LIMIT <- 100
LIST_LIMIT <- 10
TEXTMINING_WORD_LIMIT <- 100000
TEXTMINING_CHAR_LIMIT <- 500000
GENE_LIST_LIMIT <- 3000
OBJECT_SIZE_LIMIT <- 1000000 # bytes, 1MB
STRING_LIMIT <- 500
RANDOM_GENES_NUMBER <- 200 # currently 674 max # enrichR::genes790 
GENE_LIST_PREFIX <- "gene_list"
# Volcano
VOLCANO_COLORS <- c("default" = "#000000",
                    "overexpressed" = "#eb6e6c", "underexpressed" = "#64e4ed")
VOLCANO_PREFIX <- "volcano"
# 2D Reduction
REDUCTION_PREFIX <- "reduction"
REDUCTION_NA_COLOR <- "#4D4D4D"  # Dark grey for NA values
REDUCTION_COLORSCALE_CONTINUOUS <- list(
  c(0, "#2166AC"), c(0.5, "#F7F7F7"), c(1, "#B2182B")  # Blue-white-red gradient
)
REDUCTION_DEFAULT_SIZE <- 6  # Default marker size in pixels
REDUCTION_SIZE_RANGE <- c(3, 9)  # Min and max size for scaled markers
REDUCTION_NA_SIZE <- 1  # Size for NA values (below minimum)
REDUCTION_CATEGORICAL_THRESHOLD <- 20  # Max unique values to treat as categorical color (used in render.R)
REDUCTION_MARKER_OPACITY <- 0.7

# Dot Plot ####
DOTPLOT_SIZE_MIN <- 4
DOTPLOT_SIZE_MAX <- 25
DOTPLOT_ENTRY_HEIGHT_PX <- 22
DOTPLOT_COLORSCALE <- "Viridis"  # Built-in Plotly scale

# Organisms ####
ORGANISMS <- readRDS("./organisms/organismsDF.RDS")
TOOL_ORGANISMS <- readRDS("./organisms/toolOrganismsList.RDS")
SPECIAL_ORGANISMS <- c("amellifera", "dmelanogaster")
SPECIAL_PREFERRED_TOOL <- list()
SPECIAL_PREFERRED_TOOL[["amellifera"]] <- "gProfiler"
SPECIAL_PREFERRED_TOOL[["dmelanogaster"]] <- "enrichR"
SPECIAL_PREFERRED_NAMESPACE <- list()
SPECIAL_PREFERRED_NAMESPACE[["amellifera"]] <- "BEEBASE"
SPECIAL_PREFERRED_NAMESPACE[["dmelanogaster"]] <- "USERINPUT"

# Enrichment ####
ENRICHMENT_TYPES <- c("functional", "literature")
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
    "CORUM"= "CORUM"
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
  )
)
DATASOURCES_DEFAULT_SELECTED <- list("GO:MF", "GO:CC", "GO:BP", "KEGG")
DATASOURCES <- list() # gProfiler here, the rest through an init.R function
DATASOURCES[["GPROFILER"]] <- c("GO:MF", "GO:CC", "GO:BP",
                              "KEGG", "REAC", "WP",
                              "TF", "MIRNA", "CORUM", "HPA", "HP")
DATASOURCES[["STRING"]] <- c("GO:MF", "GO:CC", "GO:BP",
                            "KEGG", "REAC", "WP",
                            "INTERPRO", "PFAM", "UNIPROT",
                            "DO", "BTO", "HP", "PUBMED")
DATASOURCES[["PANTHER"]] <- c("GO:MF", "GO:CC", "GO:BP", "REAC",
                              "GOSLIM:MF", "GOSLIM:CC", "GOSLIM:BP",
                              "PANTHER Pathways", "PANTHERPC")
DATASOURCES[["GENECODIS"]] <- c("GO:MF", "GO:CC", "GO:BP",
                                "KEGG", "REAC", "WP", "PANTHER Pathways",
                                "HP", "OMIM", "MGI",
                                "BioPlanet", "PharmGKB", "LINCS",
                                "CollecTRI", "MIRNA")  # Homo sapiens (taxid: 9606)
DATASOURCES[["MMUSCULUS_GENECODIS"]] <- c("GO:MF", "GO:CC", "GO:BP",
                                          "KEGG", "REAC", "WP", "PANTHER Pathways",
                                          "MGI", "CollecTRI", "MIRNA")  # Mus musculus (taxid: 10090)
DATASOURCES[["RNORVEGICUS_GENECODIS"]] <- c("GO:MF", "GO:CC", "GO:BP",
                                            "KEGG", "REAC", "WP", "PANTHER Pathways",
                                            "MGI", "CollecTRI", "MIRNA")  # Rattus norvegicus (taxid: 10116)
DATASOURCES[["CELEGANS_GENECODIS"]] <- c("GO:MF", "GO:CC", "GO:BP",
                                         "KEGG", "REAC", "WP", "PANTHER Pathways")  # Caenorhabditis elegans (taxid: 6239)
DATASOURCES[["DMELANOGASTER_GENECODIS"]] <- c("GO:MF", "GO:CC", "GO:BP",
                                              "KEGG", "REAC", "WP", "PANTHER Pathways")  # Drosophila melanogaster (taxid: 7227)
DATASOURCES[["DRERIO_GENECODIS"]] <- c("GO:MF", "GO:CC", "GO:BP",
                                       "KEGG", "REAC", "WP", "PANTHER Pathways",
                                       "MGI")  # Danio rerio (taxid: 7955)
DATASOURCES[["CLFAMILIARIS_GENECODIS"]] <- c("GO:MF", "GO:CC", "GO:BP",
                                             "KEGG", "REAC", "WP", "PANTHER Pathways")  # Canis lupus familiaris (taxid: 9615)
DATASOURCES[["GGALLUS_GENECODIS"]] <- c("GO:MF", "GO:CC", "GO:BP",
                                        "KEGG", "REAC", "WP", "PANTHER Pathways")  # Gallus gallus (taxid: 9031)
DATASOURCES[["BTAURUS_GENECODIS"]] <- c("GO:MF", "GO:CC", "GO:BP",
                                        "KEGG", "REAC", "WP", "PANTHER Pathways")  # Bos taurus (taxid: 9913)
DATASOURCES[["SSCROFA_GENECODIS"]] <- c("GO:MF", "GO:CC", "GO:BP",
                                        "KEGG", "REAC")  # Sus scrofa (taxid: 9823)
DATASOURCES[["ATHALIANA_GENECODIS"]] <- c("GO:MF", "GO:CC", "GO:BP",
                                          "KEGG", "WP", "PANTHER Pathways")  # Arabidopsis thaliana (taxid: 3702)
DATASOURCES[["OSATIVA_GENECODIS"]] <- c("GO:MF", "GO:CC", "GO:BP", "KEGG")  # Oryza sativa Japonica Group (taxid: 39947)
DATASOURCES[["SCEREVISIAE_GENECODIS"]] <- c("GO:MF", "GO:CC", "GO:BP",
                                            "KEGG", "REAC", "WP", "PANTHER Pathways")  # Saccharomyces cerevisiae S288C (taxid: 559292)
DATASOURCES[["ECOLI_GENECODIS"]] <- c("GO:MF", "GO:CC", "GO:BP", "KEGG")  # Escherichia coli str. K-12 substr. MG1655 (taxid: 511145)
DATASOURCES_CODES <- list()
DATASOURCES_CODES[["STRING"]] <- list(
  "GO:MF" = "Function", "GO:CC" = "Component", "GO:BP" = "Process",
  "KEGG" = "KEGG", "REAC" = "RCTM", "WP" = "WikiPathways",
  "INTERPRO" = "InterPro", "PFAM" = "Pfam", "UNIPROT" = "Keyword",
  "DO" = "DISEASES", "BTO" = "TISSUES", "HP" = "HPO",
  "PUBMED" = "PMID"
)
DATASOURCES_CODES[["PANTHER"]] <- list(
  "GO:MF" = "GO:0003674", "GO:CC" = "GO:0005575", "GO:BP" = "GO:0008150",
  "GOSLIM:MF" = "ANNOT_TYPE_ID_PANTHER_GO_SLIM_MF",
  "GOSLIM:CC" = "ANNOT_TYPE_ID_PANTHER_GO_SLIM_CC",
  "GOSLIM:BP" = "ANNOT_TYPE_ID_PANTHER_GO_SLIM_BP",
  "REAC" = "ANNOT_TYPE_ID_REACTOME_PATHWAY",
  "PANTHER Pathways" = "ANNOT_TYPE_ID_PANTHER_PATHWAY",
  "PANTHERPC" = "ANNOT_TYPE_ID_PANTHER_PC"
)
DATASOURCES_CODES[["GENECODIS"]] <- list(
  "GO:MF" = "GO_MF", "GO:CC" = "GO_CC", "GO:BP" = "GO_BP",
  "KEGG" = "KEGG", "REAC" = "Reactome", "WP" = "WikiPathways",
  "PANTHER Pathways" = "Panther", "HP" = "HPO", "OMIM" = "OMIM", "MGI" = "MGI",
  "BioPlanet" = "BioPlanet", "PharmGKB" = "PharmGKB", "LINCS" = "LINCS",
  "CollecTRI" = "CollecTRI", "MIRNA" = "miRTarBase"
)
DATASOURCES_CODES[["WEBGESTALT"]] <- list(
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
)
DATASOURCES_CODES[["ENRICHR"]] <- list(
  "GO:MF" = "GO_Molecular_Function_2021",
  "GO:CC" = "GO_Cellular_Component_2021",
  "GO:BP" = "GO_Biological_Process_2021",
  "KEGG" = "KEGG_2016", # "KEGG_2021_Human" -> doesn't return Ids
  "REAC" = "Reactome_2022",
  "WP" = "WikiPathway_2021_Human",
  "PANTHER Pathways" = "Panther_2016",
  "HP" = "Human_Phenotype_Ontology"
)
DATASOURCES_CODES[["MOUSE_ENRICHR"]] <- list(
  "GO:MF" = "GO_Molecular_Function_2021",
  "GO:CC" = "GO_Cellular_Component_2021",
  "GO:BP" = "GO_Biological_Process_2021",
  "WP" = "WikiPathways_2019_Mouse",
  "MGI" = "KOMP2_Mouse_Phenotypes_2022"
)
DATASOURCES_CODES[["FLY_ENRICHR"]] <-
  DATASOURCES_CODES[["FISH_ENRICHR"]] <- list(
    "GO:MF" = "GO_Molecular_Function_2018",
    "GO:CC" = "GO_Cellular_Component_2018",
    "GO:BP" = "GO_Biological_Process_2018",
    "WP" = "WikiPathways_2018"
  )
DATASOURCES_CODES[["YEAST_ENRICHR"]] <- list(
  "GO:MF" = "GO_Molecular_Function_2018",
  "GO:CC" = "GO_Cellular_Component_2018",
  "GO:BP" = "GO_Biological_Process_2018",
  "WP" = "WikiPathways_2018",
  "KEGG" = "KEGG_2018"
)
DATASOURCES_CODES[["WORM_ENRICHR"]] <- list(
  "GO:MF" = "GO_Molecular_Function_2018",
  "GO:CC" = "GO_Cellular_Component_2018",
  "GO:BP" = "GO_Biological_Process_2018",
  "WP" = "WikiPathways_2018",
  "DO" = "Human_Diseases_from_WormBase_2018",
  "WBP" = "Phenotypes_WormBase_2018",
  "WBBT" = "Anatomic_Associations_WormBase_2018"
)
DATASOURCES_CODES[["OX_ENRICHR"]] <- list(
  "GO:MF" = "GO_Molecular_Function_2021",
  "GO:CC" = "GO_Cellular_Component_2021",
  "GO:BP" = "GO_Biological_Process_2021",
  "REAC" = "Reactome_2016",
  "WP" = "WikiPathway_2021_Human",
  "ORPHA" = "Orphanet_Augmented_2021",
  "HP" = "Human_Phenotype_Ontology",
  "MGI" = "MGI_Mammalian_Phenotype_Level_4_2019"
)
NAMESPACES[["GPROFILER"]] <- c("User Input" = "USERINPUT", NAMESPACES[["CORE"]])
NAMESPACES[["WEBGESTALT"]] <- list(
  "Entrez Gene Accession" = "ENTREZGENE_ACC",
  "User Input" = "USERINPUT"
)
NAMESPACES[["ENRICHR"]] <- list(
  "Entrez Gene Name" = "ENTREZGENE",
  "User Input" = "USERINPUT"
)
NAMESPACES[["FLY_ENRICHR"]] <-
  NAMESPACES[["YEAST_ENRICHR"]] <- list(
  "User Input" = "USERINPUT"
)
DEFAULT_NAMESPACE_TEXT <- "Default tool namespace conversions"
METRICS <- list()
METRICS[["GPROFILER"]] <- list(
  "g:SCS threshold" = "gSCS",
  "False discovery rate" = "fdr",
  "Bonferroni correction" = "bonferroni"
)
METRICS[["WEBGESTALT"]] <- list(
  "Benjamini-Hochberg" = "BH",
  "Benjamini-Yekutieli" = "BY",
  "Holm" = "holm",
  "Hochberg" = "hochberg",
  "Hommel" = "hommel",
  "Bonferroni adjustment" = "bonferroni",
  "Top 100" = "top"
)
METRICS[["ENRICHR"]] <- list("Adjusted P-value" = "adjusted_pvalue")
METRICS[["STRING"]] <- list("False discovery rate", "P-value")
METRICS[["PANTHER"]] <- list("False discovery rate", "P-value", "Bonferroni")
METRICS[["GENECODIS"]] <- list("False discovery rate" = "fdr")
DEFAULT_METRIC_TEXT <- "Default tool metrics"

DEFAULT_METRICS_GENOME <- list(
  "GPROFILER" = "fdr",
  "WEBGESTALT" = "BH",
  "ENRICHR" = "adjusted_pvalue",
  "STRING" = "False discovery rate",
  "PANTHER" = "False discovery rate",
  "GENECODIS" = "fdr"
)

DEFAULT_METRICS_USERBACKGROUND <- list(
  "GPROFILER" = "bonferroni",
  "WEBGESTALT" = "top",
  "STRING" = "P-value",
  "PANTHER" = "P-value",
  "GENECODIS" = "fdr"
)

ENRICHMENT_DF_COLNAMES <- c(
  "Source", "Term_ID", "Function", "P-value", "Term Size",
  "Query size", "Intersection Size", "Positive Hits"
)

# Plots ####
ALL_PLOT_IDS <- c(NETWORK_IDS, HEATMAP_IDS, "barchart", "scatterPlot", "dotPlot")
DEFAULT_SLIDER_VALUE <- 50
MAX_SLIDER_VALUE <- 200
SINGLE_BAR_HEIGHT_PX <- 18
MIN_BAR_HEIGHT_PX <- 200
EDGE_WIDTH_MIN <- 0.1
EDGE_WIDTH_MAX <- 3

# Interoperability ####
POST_REQUEST_PATH <- 'tmp/'
ARENA_API_LINK <- "https://bib.fleming.gr/bib/api/arena3dweb" #"http://127.0.0.1:8080/api/arena3dweb"
ARENA_LAYER_SPACING_PIXELS <- 300
ARENA_Y_Z_SAMPLING_LIMIT <- 410
