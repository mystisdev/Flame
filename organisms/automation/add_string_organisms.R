#!/usr/bin/env Rscript

# Phase 1: Add STRING support alongside existing tools
# This script adds STRING organism support to FLAME without removing aGOtool

# Load existing data
cat("Loading existing organism data...\n")
organismsDF <- readRDS("organisms/organismsDF.RDS")
toolOrganisms <- readRDS("organisms/toolOrganismsList.RDS")

# Load STRING taxids
cat("Reading STRING species data...\n")
string_data <- read.delim("organisms/automation/string_species.v12.0.txt",
                         header = TRUE, stringsAsFactors = FALSE)
string_taxids <- string_data$X.taxon_id

# Verify all STRING organisms exist in master list
missing_orgs <- setdiff(string_taxids, organismsDF$taxid)
if (length(missing_orgs) > 0) {
  stop("Found ", length(missing_orgs), " STRING organisms not in organismsDF")
}

# Add STRING support
toolOrganisms$STRING <- string_taxids

# Save updated toolOrganismsList
cat("Saving updated organism support data...\n")
saveRDS(toolOrganisms, "organisms/toolOrganismsList.RDS")

cat("Successfully added STRING support for", length(string_taxids), "organisms\n")
cat("Current tool support counts:\n")
for (tool in names(toolOrganisms)) {
  cat("  ", tool, ": ", length(toolOrganisms[[tool]]), " organisms\n")
}