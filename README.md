This is the repository for A Latent Variable Approach to Measuring and Explaining Peace Agreement Strength. It contains only the raw data files and the code used to generate the results. If you wish to download a version with all of the models already run, please visit the [replication archive](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VUY8UI) at Dataverse.

# Replicating the Results

The code to prepare the analysis dataset, execute the analysis, and compile the PDFs of the paper and supplemental information can be run by either typing opening a unix-like shell in the project directory and typing `make` or opening the `.Rproj` file in RStudio and clicking "Build All" (this will work on Windows if you have rtools installed). The output will be printed to the shell and saved to `log.txt`.

# Platform Dependency

The results in the paper are known to be exactly replicable only on CentOS 7, v7.3.1611, R version 3.5.0, rstan v2.18.2, StanHeaders 2.18.1. Other operating systems, R versions, and/or package versions will give substantively similar, but numerically different results.

# Required Packages

The following packages are required for this code to run properly, and will be
automatically installed by 01 Setup.R if not already present:

- knitr 1.22
- tidyverse 1.2.1
- reshape2 1.4.3
- dplyr 0.8.0.1
- plyr 1.8.4
- ggridges 0.5.1
- ggrepel 0.8.0
- corrplot 0.84
- gridExtra 2.3
- data.table 1.12.0
- rio 0.5.16
- countrycode 1.1.0
- lubridate 1.7.4
- WDI 2.6.0
- RJSONIO 1.3-1.1
- doRNG 1.7.1
- mice 3.4.0
- rstan 2.18.2
- StanHeaders 2.18.1
- bayesplot 1.6.0
- coda 0.19-2
- ggmcmc 1.2
- texreg 1.36.23
- xtable 1.8-3

All R code was run on CentOS Linux v7.3.1611. Full computing environment information is available for data preparation and analyses in log.txt. Full computing environment information is available for PDF compilation in the Supplemental Information (Section G).

Note that rstan requires the installation of a C++ toolchain (Xcode Command Lines Tools on MacOS, Rtools on Windows, and g++ >= 4.9 or clang++ >= 3.4 on Linux), so any missing packages are set to install from source, which may fail without appropriate compilation tools. In addition, installing rstan on CentOS requires the modification of the makevars file. Script 01 Setup.R does this automatically if it detects that the platform is CentOS. You may need to manually run the code in lines 21-27 if you are using a different version of Linux that still requires adjustments to R's makevars.

# Data Sources

The following datasets are used to create the analysis dataset:

- UCDP ACD (2013 version for compatibility with peace agreements data)
- UCDP Peace Agreements Data
- CWM dataset (August 2014 version **converted to CSV due to errors in dates**)
- Authors' coding of whether a mediator is a regional organization
- TIES data (**with [corrections](http://bapat.web.unc.edu/files/2018/09/TIESCorrection.txt), converted to CSV**)
- Relative Political Capacity data (2015 version)
- World Bank WDI (extracted from World Bank servers with WDI package in R)
- Polity IV data (2015 version)
- International Military Intervention data (merged version 1947-2005)

# R Scripts

The following R scripts are used to prepare the analysis dataset and run analyses:

- `00 Execution.R` - runs scripts 01-03
- `01 Setup.R` - loads required packages, install missing packages
- `02 Data Access.R` - reads in data files used to construct analysis dataset
- `03 Data Manipulation.R` - transforms data for analysis
- `04 Analysis.R` - carries out IRT analyses

Scripts 01-03 are intended to be run in succession in the same R session. This can be accomplished by running 00 Execution.R or via the Makefile. Script 04 is intended to be run in its own R session. This can be accomplished by running it manually, but the Makefile will automatically do so. This script takes approximately 6 hours to run on the CentOS machine used to generate the results in the paper.

The following R scripts provide accessory functions:

- `mcmcreg.R` - function to create Bayesian regression tables
- `themew_rw.R` - custom blank ggplot2 theme

# Stan Models

The analyses use the following Stan models:

- `IRT.stan` - Bayesian item response theory model
- `IRT Full Probability.stan` - Bayesian full probability item response theory model
- `Linear Model.stan` - Bayesian linear model

# PDF Compilation

The following files create the paper and supplemental information:

- `Paper.Rnw`
- `SI.Rnw`
- `cup_PSRM.cls` - PSRM style file
- `Measurement.bib` - bibtex file for references

They can be manually compiled by opening the `.Rnw` files in RStudio and clicking "compile PDF", or via the Makefile.
