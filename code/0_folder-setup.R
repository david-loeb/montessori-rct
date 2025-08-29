#==============================================================================#
# Create Folder Structure for Data and Output                                  #
#==============================================================================#

#* This file creates the folders necessary to store data and output.

#* The `pak` package handles package installation & updating. It will install
#* any packages not already installed. If you already have a package installed,
#* if there is an update available, it will ask if you want to update. If you
#* already have the latest version installed, it will do nothing. You can add an
#* `ask = FALSE` argument to `pak::pkg_install()` if you want to auto-update 
#* packages rather than being asked. You can also set a specific folder to 
#* install the packages to if you don't want to install them in your general 
#* library. See http://pak.r-lib.org for more details.

if (!require('pak')) install.packages('pak')  # Installs pak if needed
pak::pkg_install('this.path')
setwd(this.path::here())  # Set working directory to current folder

dir.create('../data')  # Create folders for storing data and output
dir.create('../data/imputed')
dir.create('../data/imputed/pk3')
dir.create('../data/imputed/pk4')
dir.create('../data/imputed/moderators')
dir.create('../data/imputed/no_rank_choice')
dir.create('../data/imputed/propensity_scores')
dir.create('../data/imputed/alt_covars')
dir.create('../data/imputed/alt_covars/bl_y_only')
dir.create('../data/imputed/alt_covars/bl_y_all')
dir.create('../data/imputed/alt_covars/age_ctrl')
dir.create('../output')  
dir.create('../output/equivalence')
dir.create('../output/pk')
dir.create('../output/k')
dir.create('../output/k/sensitivity')
dir.create('../output/k/moderators')
dir.create('../output/k/exploratory')
dir.create('../output/tables_figures')