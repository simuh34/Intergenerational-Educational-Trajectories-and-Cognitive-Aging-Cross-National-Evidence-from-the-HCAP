# Intergenerational-Educational-Trajectories-and-Cognitive-Aging-Cross-National-Evidence-from-the-HCAP

### Data & Country-Specific Analysis
These folders contain the specific files, datasets, or scripts corresponding to each of the Harmonized Cognitive Assessment Protocol (HCAP) studies:
* **`CHARLS/`** : China Health and Retirement Longitudinal Study (China)
* **`ELSA/`** : English Longitudinal Study of Ageing (England)
* **`HRS/`** : Health and Retirement Study (USA)
* **`LASI/`** : Longitudinal Aging Study in India (India)
* **`MHAS/`** : Mexican Health and Aging Study (Mexico)

* ### Global Analysis Scripts (R)
The root directory contains the R scripts used for cross-national plotting and sensitivity checks:
* **`harmonized education-plot.R`**: R script used to generate visualizations for harmonized education levels across the different cohorts.
* **`percentile rank of education-plot.R`**: R script to visualize the within-country education rankings (comparing participants' education relative to their parents).
* **`sensitivity analysis.R`**: R script containing the code for conducting sensitivity analyses to validate the main regression findings.

## Requirements
To run the scripts in this repository, you will need **R** installed on your system, along with standard data manipulation and visualization packages (e.g., `ggplot2`, `dplyr`).
