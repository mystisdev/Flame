# Enrichment ####
DEFAULT_TOOL <- "STRING"
DEFAULT_TOOL_UPPER <- toupper(DEFAULT_TOOL)

# Plots ####
PLOT_TABNAMES <- c("Barchart", "Dot Plot", "Scatter Plot", "Manhattan", "Heatmap", "Network")
# Network 
LEGEND_ITEMS <- list(
  list("GO:MF", "GO:BP", "GO:CC","UNIPROT"),
  list("KEGG", "REAC", "WP", "PANTHER Pathways"),
  list("DO", "DISGENET", "OMIM", "GLAD4U_DISEASE", "ORPHA"),
  list("DRUGBANK", "GLAD4U_DRUG", "INTERPRO", "PFAM", "PUBMED"),
  list("BTO", "WBBT", "TF", "CollecTRI", "MIRNA", "CORUM"),
  list("HPA", "HP", "WBP", "MGI", "GENE")
)
VIS_NET_HEIGHT <- "850px"

# About ####
YEAR <- substr(Sys.Date(), 1, 4)
