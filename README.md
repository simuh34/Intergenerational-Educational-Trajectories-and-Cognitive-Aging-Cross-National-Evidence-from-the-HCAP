# Intergenerational Education Trajectories and Cognitive Aging: Cross-National Evidence from the Harmonized Cognitive Assessment Protocol

### OVERVIEW

This repository contains the replication code for: Intergenerational Education Trajectories and Cognitive Aging: Cross-National Evidence from the Harmonized Cognitive Assessment Protocol.

The analysis covers five major international longitudinal studies:
* **HRS**: Health and Retirement Study (USA)
* **ELSA**: English Longitudinal Study of Ageing (England)
* **MHAS**: Mexican Health and Aging Study (Mexico)
* **CHARLS**: China Health and Retirement Longitudinal Study (China)
* **LASI**: Longitudinal Aging Study in India (India)

==================================================
 
### REPOSITORY STRUCTURE

REPOSITORY STRUCTURE

The repository is organized by country-specific study folders and core scripts for cross-national visualization and sensitivity testing:

COUNTRY FOLDERS

CHARLS/  : Scripts and metadata for the China Health and Retirement Longitudinal Study
ELSA/    : Scripts and metadata for the English Longitudinal Study of Ageing
HRS/     : Scripts and metadata for the US Health and Retirement Study
LASI/    : Scripts and metadata for the Longitudinal Aging Study in India
MHAS/    : Scripts and metadata for the Mexican Health and Aging Study

CORE ANALYSIS AND VISUALIZATION SCRIPTS

harmonized education-plot.R         : Global harmonization and education visualization
percentile rank of education-plot.R : Relative education ranking analysis across cohorts
sensitivity analysis.R              : Robustness checks and sensitivity protocols

==================================================
 
### DATA AVAILABILITY

The analysis utilizes harmonized data across international cohorts within the Harmonized Cognitive Assessment Protocol (HCAP) framework.

* **HRS**: https://hrs.isr.umich.edu
* **ELSA**: https://www.elsa-project.ac.uk
* **MHAS**: https://www.mhasweb.org
* **CHARLS**: http://charls.pku.edu.cn
* **LASI**: https://lasi-dad.org

==================================================
 
### SOFTWARE REQUIREMENTS

Analyses were conducted in R. To ensure compatibility with the provided scripts, the following is required:

* **R version**: 4.2.0 or higher
* **Core Packages**: `tidyverse`, `haven`, `survey`, `lme4` 

==================================================
 
### CONTACT

For questions about code or dashboard, contact the corresponding author.
