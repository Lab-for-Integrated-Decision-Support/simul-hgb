# Simultaneously Acquired Hgb Labs

The objective of this work is to determine the clinical and analytic accuracy of simultaneously-acquired hemoglobin values from CBCs, blood gas analyzers, and POC devices (iStat). This is a multi-center, federated project with by-site extraction and analysis; no data were shared between institutions. At each site (CHOP and URMC), data were extracted and exported to a standard format for analysis in this public repo.

## Input Format

The input common data model should include the following:

**Environment Variables:**

+ `PICU_LAB_DATA_PATH` - The path (without trailing `/`) to the data files.
+ `PICU_LAB_IMG_PATH` - The path (without trailing `/`) to the image output directory.
+ `PICU_LAB_IN_FILE` - The name of the RData file (with the extension) containing the below data frames.
+ `PICU_LAB_SITE_NAME` - The site name used to identify output files for subsequent aggregation

**Cohort Data Frame:**

Name: `cohort.df`

Columns:

| Name | Type | Description |
|------|------|-------------|
| `ENC_KEY` | char | Unique key for each ICU encounter |
| `PAT_KEY` | char | Unique (random) key for each patient |
| `HOSP_KEY` | char | Unique (random) key for each hospital encounter |
| `SEX` | char | `Male`, `Female`, or `Unknown` |
| `AGE_ICU_IN` | numeric | Age (in days) at ICU admission |
| `ICU_LOS` | numeric | ICU length of stay (hours) |
| `DEPT` | char | `PICU` or `CICU` |
| `DISCH_DISP_SRC` | char | The source system name for discharge disposition |
| `DISCH_DISP` | char | `Survived` or `Died` |

**Labs Data Frame:**

Name: `labs.df`

Columns:

| Name | Type | Description |
|------|------|-------------|
| `ENC_KEY` | char | Unique key linking lab to ICU encounter |
| `AGE_PROC` | numeric | Age (in days) at the time of lab collection |
| `ORDER_PROC_KEY` | numeric | Unique key for each ordered procedure |
| `RESULT_DT` | datetime | Timestamp that the lab resulted |
| `COLLECTED_DT` | datetime | Timestamp that the lab was collected |
| `PROC_SRC` | char | The source system name for the procedure |
| `PROC_NAME` | char | `CBC`, `BG`, or `ISTAT` |
| `COMP_SRC` | char | The source name for the component |
| `COMP_NAME` | char | `Hgb`, `pH`, `Bicarb`, `Gluc`, `iCal`, `Lactate` |
| `NUM_VAL` | numeric | Numeric value of the lab |
| `VALUE_SRC` | char | The source order value |

Important notes about the cohort and labs:

- Cohorts should be limited to the IRB-approved cohort dates. Because ICU admission dates are not included, we cannot verify this is accurate. This should occur during pre-processing.
- Cohorts should be limited to appropriately aged patients per IRB restrictions. This is also not verified during analysis.
- Lab procedures and components should be mapped to the standard terms. Only those labs matching the standard `PROC_NAME` and `COMP_NAME` terms should be included.
- Labs should be filtered to occur during the date range of the ICU encounter. Because ICU admission dates are not included, we cannot verify this in subsequent steps.
- All timestamps should be either character strings (`%Y-%m-%d %H:%M`) or `POSIXct` types, with timezone set to `UTC`

## Analysis

Analysis proceeds first independently on each site, then we aggregate the data files from each site to generate the final figures. Note that each Markdown file has functions written in chunks. These functions were pulled out to the package following development and testing.

### Site-wise Analysis

#### Cohort Description

Begin with `01_Cohort_Description.Rmd` which loads both data frames and displays information to populate Table 1. This markdown ends by looking for an optimum cutoff value by which to call labs "simultaneous". It writes out the following:

+ `<SITE>_thresholds_<PN-1>-<PN-2>.rData` - Contains both raw data and a ggplot list of the simultaneous value thresholds for this site. 

The functions in this markdown are:

+ `labValueDescriptions`: Displays graph and summary for lab values by PROC and COMP
+ `timeThresholdGraph`: Joins CN across PNs and plot collected time difference by cutoff

#### Analytic Accuracy

Next, move on to the Analytic analysis which can be found in `02_Analytic_Accuracy.Rmd`. This markdown creates the paired dataset by specifying the cutoff time and whether we include repeated measures from the same patient. This paired dataset is then used to view the data distributions, correlations, Bland-Altman, and time-to-result.

Depending on the PROCs in the dataset, there will be numerous file outputs from this markdown. These include:

+ `<SITE>_pri_cbc_bg_analytic_<date>.rData` - CBC-BG analysis using primary cutoff
+ `<SITE>_pri_cbc_istat_analytic_<date>.rData` - CBC-ISTAT analysis using primary cutoff
+ `<SITE>_single_pt_cbc_bg_analytic_<date>.rData` - CBC-BG with one value per patient
+ `<SITE>_single_pt_cbc_istat_analytic_<date>.rData` - CBC-ISTAT with one value per patient

The functions in this markdown are:

+ `createPairedDataset`: Creates a dataset of paired simultaneous lab values
+ `describePairedDistributions`: Descriptive statistics, QQ plots, density plot, and TTest 
+ `determineCorrelation`: Determine the Pearson Correlation among pairs by DEPT
+ `comparePearsonCorrelations`: Use the Fisher's R to Z transformation and return significance 
+ `performBlandAltmanAnalysis`: Calculates Bland-Altman statistics and generates plots
+ `describeTimeToResult`: Compares and describes the time to result between PROCs
+ `runAllAnalytic`: Runs through all analytic analyses, for sensitivity analyses

#### Clinical Accuracy

Next, move on to Clinical accuracy analysis found in `03_Clinical_Accuracy.Rmd`. In this markdown we create the paired dataset using a copy of the function found in the prior script. (N.B. - although convention dictates that code duplication is never appropriate, in this case it allows us to keep the function within the markdown file during development. After development, the function was pulled out to a package. Care was taken to ensure that both "versions" of the function were kept identical.) The paired dataset is then used to complete error grid analysis, regression, Cohen's Kappa, as well as "testS" of similarity. 

Depending on the PROCs in the dataset, the output data files from this markdown include:

+ `<SITE>_pri_cbc_bg_clinical_<date>.rData` - CBC-BG analysis using primary cutoff
+ `<SITE>_pri_cbc_istat_clinical_<date>.rData` - CBC-ISTAT analysis using primary cutoff
+ `<SITE>_single_pt_cbc_bg_clinical_<date>.rData` - CBC-BG with one value per patient
+ `<SITE>_single_pt_cbc_istat_clinical_<date>.rData` - CBC-ISTAT with one value per patient

The functions in this markdown are:

+ `createPairedDataset`: Creates a dataset of paired simultaneous lab values (**duplicated**)
+ `calculateErrorGrid` : Calculates points within each area of Error Grid and plots
+ `gatherCovariates` : Creates pivoted data frame of covariate labs for each pair
+ `displayCovariateStats` : Displays statistics on covariates in the data frame
+ `joinImputeRegress` : Join covars and pairs, impute NA values, regress, and report results
+ `calculateCohenKappa` : Calculates the Cohen Kappa statistic for two vectors of CN values
+ `transfusionConfusionMatrix` : Creates a 2x2 confusion matrix for a pair of cutoffs and direction
+ `calculateThresholdROC` : Calculates an ROC and P-R based on Transfusion "Test"
+ `runAllClinical` : Runs through all clinical accuracy tasks, for sensitivity analysis

#### Temporal Analysis

Lastly, we move onto tempral assessments in the `04_Temporal_Analysis.Rmd` file. This is specifically in response to reviewer requests to consider the temporal components of simultaneously-acquired lab values. We again create the paired dataset using a copy of the same function as prior. We then describe repeated measures on the same encounter, and analyze the change in BG vs CBC hemoglobin on an Error Grid in response to reviewer requests.

Depending on the PROCs in the dataset, the output data files from this markdown include:

+ `<SITE>_pri_cbc_bg_temporal_<date>.rData` - CBC-BG analysis using primary cutoff
+ `<SITE>_pri_cbc_istat_temporal_<date>.rData` - CBC-ISTAT analysis using primary cutoff

The functions in this markdown are:

+ `createPairedDataset`: Creates a dataset of paired simultaneous lab values (**duplicated**)
+ `describeRepeatedPairs` : Describes counts of repeated pairs and time differences between pairs
+ `calculateChangeComparison` : Generates Delta Error Grid and Percents by Zone
+ `runAllTemporal` : Runs through all temporal assessments, for sensitivity analysis


### Combined Processing

After sharing (non-PHI) `rData` output files from both sites, we combine sites' data to create publication figures. This is done in the file `10_Combine_Figures.Rmd`. This file has several helper functions to list and load specific variables from `rData` files. Making use of those functions and the `cowplot` package, we generate all of the figures for the publication as well as supplemental figures.

The functions in this markdown are:

+ `.makeFileName` : Helper function to make the file name from site, file, run.date
+ `listVars` : Lists the variables within an rData file
+ `extractVar` : Extracts a variable from an rData file to the Global Env
