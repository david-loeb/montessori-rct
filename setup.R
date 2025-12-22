#==============================================================================#
# Setup for renv and Folder Structure for Data and Output                      #
#==============================================================================#

#* This script sets up the renv; see the README "Setup" section for details. 
#* It also creates the file structure needed to store data and output.

# Uncomment & run code below if setting up via option 3 (see README instructions)
# setwd('insert/path/to/current/folder/here')
# if (!requireNamespace('renv')) install.packages('renv')
# renv::activate()

renv::restore(prompt = FALSE)  # Install packages to complete creating renv

dirs <- c(  # Create folders for data & output storage
  'data', 'data/imputed', 'data/imputed/pk3', 'data/imputed/pk4',
  'data/imputed/moderators', 'data/imputed/no_rank_choice', 
  'data/imputed/propensity_scores', 'data/imputed/alt_covars',
  'data/imputed/alt_covars/bl_y_only', 'data/imputed/alt_covars/bl_y_all',
  'data/imputed/alt_covars/age_ctrl', 'output', 'output/equivalence',
  'output/pk', 'output/k', 'output/k/sensitivity', 'output/k/moderators',
  'output/k/exploratory', 'output/tables_figures'
)
for (dir in dirs) dir.create(here::here(dir))