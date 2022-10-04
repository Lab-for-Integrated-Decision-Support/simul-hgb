# Simultaneously Acquired Hgb Labs

The objective of this work is to determine the clinical and analytic accuracy of simultaneously-acquired hemoglobin values from CBCs, blood gas analyzers, and POC devices (iStat). This is a multi-center, federated project with by-site extraction and analysis; no data were shared between institutions. At each site (CHOP and URMC), data were extracted and exported to a standard format for analysis in this public repo.

## Input Format

The input common data model should include the following (and this will be repeated in the GitHub repo, to ensure that the input data is correct):

**Environment Variables:**

+ `PICU_LAB_DATA_PATH` - The path (without trailing `/`) to the data files.
+ `PICU_LAB_IMG_PATH` - The path (without trailing `/`) to the image output directory.
+ `PICU_LAB_IN_FILE` - The name of the RData file (with the extension) containing the below data frames.

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

The markdown
