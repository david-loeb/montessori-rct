# Montessori RCT Replication Code

The code in this repo enables replication of all analyses in the [public Montessori preschool evaluation RCT](https://osf.io/vuc42/).

## Downloading code files

To download the code files in this repo, click the green "Code" button toward the top of this page and then click "Download ZIP." (You can of course also clone the repo if you are familiar with Git.)

## Code files overview

The numeric prefix for each script indicates the general order in which scripts should be run. The '0_folder-setup.R' script must be run first to create the folder structure required to store data and output. The other '0' and '1' scripts are run at the beginning of the subsequent scripts as needed, so you will not need to open and run those scripts directly. The '2' scripts perform multiple imputation and save imputed datasets. You will need to run these directly before conducting analyses that use the multiply-imputed data. The '3' scripts perform all analyses, broken into four categories: 'equivalence' examines equivalence between treatment and control groups; 'impact' has the main impact analyses; 'impact_sensitivity' has sensitivity analyses for the main impact analyses; and 'impact_exploratory' has exploratory analyses that follow up on the main impact analyses. The '4' file is a Quarto file that produces results tables and figures (more details below) and the '\_quarto.yml' file sets its output folder.

## Running the code

To reiterate, '0_folder-setup.R' must be run first to create the necessary data and output storage folders. You only need to run this script one time.

For the other scripts, the first step to running the code is always to run the setup directly below the script title, where package installation and loading, data and function loading, and any other setup is handled. The code is then broken into sections, with section and sub-section hierarchies denoted by the number of `#` at the beginning of the section title. All code necessary to run a given section is contained within that section. To run code in a sub-section, make sure to first run all code in its "parent" section(s).

## Data

The data needed for replication is available at the project's [Open Science Framework page](https://osf.io/cp8xg/files/osfstorage). Download the data and save it in the 'data' folder that the folder setup script creates.

## Software requirements

### R

The R programming language must be installed to run the scripts. We recommend also installing RStudio, a user-friendly software that greatly eases working with R. Both are free and can be [downloaded here](https://posit.co/download/rstudio-desktop/).

### Quarto

Rendering the tables and figures requires Quarto, a free scientific publishing software. Quarto comes bundled with RStudio, and you can simply click "Render" if you open the Quarto file in RStudio to generate the PDF output. If you don't want to download RStudio, Quarto can be downloaded on its own [here](https://quarto.org/docs/get-started/).

### Blimp

Multiple imputation is handled with the free Blimp software. Blimp is called through R using the {rblimp} package. Blimp can be [downloaded here](https://www.appliedmissingdata.com/blimp).

## R package installation

The code uses a number of external user-created R packages. The {pak} R package is used in each script to handle the installation and updating of the other external packages. It will install any packages not already installed. If you already have a package installed, if there is a new version available, it will ask if you want to update. If you already have the latest version installed, it will do nothing. You can also set a specific folder to install the packages to if you don't want to install them in your general R package library. See the [{pak} documentation site](http://pak.r-lib.org) for more details.

The {pacman} package is used instead in the table and figure generation Quarto script for simplicity. It installs any packages not already installed and otherwise leaves packages as-is. It also loads all packages. If you would like more control over package installation, feel free to switch to {pak} or another installation method in this document.

## Code comments

I left comments by any code that I thought could use explanation. To prevent excessive commenting, I generally comment on code only the first time it appears. If you are confused about a piece of code and it has no comments, search for similar code earlier in the script. I also include explanations of setup code at the beginning of the first script where the code appears and leave it unexplained in subsequent scripts.

## Feedback is welcome!

Please let me know if you run into any issues while using this code. You can open an issue here or email the study team.