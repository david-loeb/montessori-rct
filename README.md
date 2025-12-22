# Montessori RCT Replication Code

The code in this repo enables replication of all analyses in the [public Montessori preschool evaluation RCT](https://www.pnas.org/doi/10.1073/pnas.2506130122).

## Downloading files

To download the files in this repo, click the green "Code" button toward the top of this page and then click "Download ZIP." (You can of course also clone the repo if you are familiar with Git.)

## Setup

A one-time setup process must be done to install the packages needed to run the code and create the folders needed to store data and output. The project uses the renv framework to install the correct versions of packages in a project-specific package library rather than the system-wide library on your computer.

There are three options for completing this setup:

### Option 1: RStudio

- Double click the "montessori-rct.Rproj" file. This will open the project in RStudio and automatically begin the process of setting up renv.
- Then run the "setup.R" script either by (a) running `source('setup.R')` in the console or (b) opening the file and running the script. This will install the packages and create the folders.

### Option 2: VS Code / Positron

- Open the project folder from within VS Code or Positron. This will automatically begin the process of setting up renv.
- Then run the "setup.R" script either by (a) running `source('setup.R')` in the console or (b) opening the file and running the script. This will install the packages and create the folders.

### Option 3: Manual renv setup

- Open the "setup.R" script.
- Uncomment the call to `setwd()`, insert the file path the project folder on your computer, and run.
- Uncomment and run the next line of code; it checks to see if you have the {renv} package installed on your computer and, if not, installs it. (This is the only system-wide package installation required.)
- Uncomment `renv::activate()` and run.
- Restart your R session.
- Run the rest of the "setup.R" script.

### Activating renv in future sessions

Each time you work with the project code, renv must be activated so that the session uses the project-specific package library. To do this, you just take the same initial step that you took in the setup, i.e. either

- double click "montessori-rct.Rproj",
- open the project from within VS Code or Positron,
- or `setwd()` to the project folder on your computer, run `renv::activate()`, and restart the R session.

## Code files overview

All project code is located in the "code" folder. The numeric prefix for each script indicates the general order in which scripts should be run. The '0' and '1' scripts are run at the beginning of the subsequent scripts as needed, so you will not need to open and run those scripts directly. The '2' scripts perform multiple imputation and save imputed datasets. You will need to run these directly before conducting analyses that use the multiply-imputed data. The '3' scripts perform all analyses, broken into four categories: 'equivalence' examines equivalence between treatment and control groups; 'impact' has the main impact analyses; 'impact_sensitivity' has sensitivity analyses for the main impact analyses; and 'impact_exploratory' has exploratory analyses that follow up on the main impact analyses. The '4' files are Quarto files (more details below) that produce results tables and figures, and the '\_quarto.yml' file sets their output folder.

## Running the code

The first step to running the code is always to run the setup directly below the script title, where package, data and function loading and any other setup is handled. The code is then broken into sections, with section and sub-section hierarchies denoted by the number of `#` at the beginning of the section title. All code necessary to run a given section is contained within that section. To run code in a sub-section, make sure to first run all code in its "parent" section(s).

## Data

The data needed for replication is available at the project's [Open Science Framework page](https://osf.io/cp8xg/overview). Download the data and save it in the 'data' folder that the setup script creates.

## Software requirements

### R

The R programming language must be installed to run the scripts. We recommend also installing RStudio, a user-friendly software for working with R. Both are free and can be [downloaded here](https://posit.co/download/rstudio-desktop/).

### Quarto

Rendering the tables and figures requires Quarto, a free scientific publishing software. Quarto comes bundled with RStudio, and you can simply click "Render" if you open the Quarto file in RStudio to generate the PDF output. If you don't want to download RStudio, Quarto can be downloaded on its own [here](https://quarto.org/docs/get-started/).

### Blimp

Multiple imputation is handled with the free Blimp software. Blimp is called through R using the {rblimp} package. Blimp can be [downloaded here](https://www.appliedmissingdata.com/blimp).

## Code comments

I left comments by any code that I thought could use explanation. To prevent excessive commenting, I generally comment on code only the first time it appears. If you are confused about a piece of code and it has no comments, search for similar code earlier in the script. I also include explanations of setup code at the beginning of the first script where the code appears and leave it unexplained in subsequent scripts.

## Feedback is welcome!

Please let me know if you run into any issues while using this code. You can open an issue here or email the study team.