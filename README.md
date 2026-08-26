# Craniofacial Missing Data and Imputation Assessment

## Overview

This repository contains the analysis for the Statistician Technical Assessment on missing data and imputation in craniofacial measurements.

The analysis includes:
- investigation of variable- and subject-level missingness;
- visualisation of missing-data patterns;
- assessment of possible missingness mechanisms;
- median and k-nearest-neighbour (kNN) imputation;
- comparison of kNN neighbourhood sizes (k = 3, 4, and 5);
- validation using artificially masked observed values;
- comparison of RMSE and MAE;
- assessment of distribution and correlation preservation; and
- generation of the final completed imputed dataset.

The final selected method was kNN with k = 3.

## Software

The analysis was conducted in R.

Recommended:
- R 4.5.2
- RStudio 

## R Dependencies

The following R packages are required:

- dplyr
- tidyr
- ggplot2
- stringr

Install the required packages if necessary:

install.packages(c("dplyr", "tidyr", "ggplot2", "stringr"))

## Input File

The original dataset is:

data/Craniofacial Data.csv

The dataset contains:
- 24 subjects;
- Patient ID, gender, and age;
- 152 craniofacial measurement variables.

Patient ID, gender, and age are not imputed.


## Reproducing the Analysis

1. Download or clone the repository.

2. Open the project folder in R or RStudio.

3. Install the required R packages if they are not already installed.

4. Ensure the original input file is located at:

   data/Craniofacial Data.csv

5. Run:

   analysis/craniofacial_imputation_analysis.R

   from the beginning to the end.

6. The script will:
   - examine the amount and pattern of missing data;
   - investigate associations between missingness and available subject characteristics;
   - generate missingness and correlation plots;
   - perform median and kNN imputation;
   - compare k = 3, 4, and 5;
   - validate the methods using artificially masked observed values;
   - calculate RMSE and MAE;
   - assess distribution and correlation preservation; and
   - generate the completed kNN-imputed dataset.

7. A fixed random seed of 123 is used when generating the artificial validation mask to ensure reproducibility.

## Main Output

The final completed dataset is:

data/Craniofacial_imputed_kNN_k3.csv

The selected kNN method uses k = 3. Originally observed measurement values are retained and only missing craniofacial measurements are imputed.

Diagnostic figures and validation tables are provided in the `outputs` folder.

## Notes

The original dataset contains genuine missing values. For validation, approximately 10% of observed values within variables containing real missingness are temporarily masked. The true values of these artificially masked observations are retained for calculation of RMSE and MAE.

The final recommendation is intended for exploratory analysis. Further validation using a larger independent dataset would be required before use in a clinical or automated production system.





