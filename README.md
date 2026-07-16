# FTOLP Data Pipeline

A comprehensive R-based data processing pipeline for the "Future Time Orientation and Life Projects: Narrating the Future" (FTOLP) research project. This pipeline handles survey data from multiple platforms (e.g., LimeSurvey), multiple countries and survey waves, performing data splitting, quality control cleaning, and merging operations.

## 📋 Table of Contents

- [Project Overview](#project-overview)
- [Directory Structure](#directory-structure)
- [Installation](#installation)
- [Configuration](#configuration)
- [Pipeline Workflow](#pipeline-workflow)
- [Scripts Documentation](#scripts-documentation)
- [Dataset Groupings](#dataset-groupings)
- [License](#license)

## 🔍 Project Overview

This pipeline processes survey data from LimeSurvey and other platforms across multiple countries and handles:

- **Data splitting**: Splitting raw survey data into country/wave-specific datasets, normalizing column names, and filtering out test participants.
- **Data cleaning**: Removing invalid responses; detecting straightlining, zigzag patterns, and outliers; applying country-specific filters; and generating audit reports.
- **Data merging**: Merging cleaned datasets, standardizing demographic variables, applying missing value coding, and preparing for analysis.

## 📁 Directory Structure

```
FTOLP_Data_Pipeline/
├── config/
│   └── paths.R                    # Central configuration for paths and dataset groupings
├── src/
│   ├── pipeline/                  # Main processing pipeline scripts (run in order)
│   │   ├── 01_split.R             # Split and preprocess raw data
│   │   ├── 02_clean.R             # Quality control and data cleaning
│   │   └── 03_merge.R             # Merge cleaned datasets
│   ├── utils/                     # Utility functions
│   │   ├── cleaning_functions.R   # Core cleaning utilities (e.g., steps, filters, outlier detection)
│   │   ├── merge_functions.R      # Merging and labeling utilities
├── docs/                          # Additional documentation
├── README.md                      # This file
└── LICENSE                        # MIT License

```

## 🚀 Installation

### Exact Environment Setup with `renv` (Recommended)
> Steps 1–2 are GUI installers. Steps 3 onwards are run in the **R console** (e.g., RStudio or a terminal with `R` started).

1. Install `R 4.5.3`.
    - MacOS (Apple Silicon): [Download](https://cran.r-project.org/bin/macosx/big-sur-arm64/base/R-4.5.3-arm64.pkg)
    - MacOS (Intel): [Download](https://cran.r-project.org/bin/macosx/big-sur-x86_64/base/R-4.5.3-x86_64.pkg)
    - Windows: [Download](https://cran.r-project.org/bin/windows/base/old/4.5.3/R-4.5.3-win.exe)
  
> In principle, any R version ≥ 4.0.0 should work, but we recommend using the exact version to avoid compatibility issues with packages.

2. Install [RStudio](https://posit.co/download/rstudio-desktop/).

3. In RStudio, open this repository as a project, and verify that `R 4.5.3` is the R version being used:
   ```r
   R.version.string
   ```

4. Install `renv` if not already present:
   ```r
   if (!requireNamespace("renv", quietly = TRUE)) {
       install.packages("renv")
   }
   ```

5. Restore the exact package versions from `renv.lock`:
    ```r
    renv::restore()
    ```

    When asked `Do you want to proceed? [Y/n]:`, type `Y` and press Enter.

### Required R Packages
If you prefer to install packages manually, below are the required packages:

```r
install.packages(c(
  "tidyverse",    # Data manipulation
  "haven",        # SPSS file I/O
  "labelled",     # Variable labels
  "readxl",       # Excel files
  "lubridate",    # Date handling
  "writexl",      # Excel output
  "here",         # Path management
  "rstatix",      # Statistical tests
  "PerFit"        # Person-fit analysis
))
```

## ⚙️ Path Configuration
Before running the pipeline, configure your paths in `config/paths.R`:

```r
# Base directory for data storage (change this to your local path)
DATA_ROOT <- "~/Library/CloudStorage/Nextcloud-6161138@soliscom.uu.nl@surfdrive.surf.nl/ftolp_data"

# Data directories
DIR_RAW <- file.path(DATA_ROOT, "raw")
DIR_SPLIT <- file.path(DATA_ROOT, "split")
DIR_CLEAN <- file.path(DATA_ROOT, "clean")
```

Modify `DATA_ROOT` to match your local setup.

## 🔄 Pipeline Workflow

Run scripts in the following order:

### 1. Split Raw Data (`01_split.R`)

**Purpose**: Load and split raw survey files into country/wave-specific datasets with minimal preprocessing.

**Key Operations**:
- Load raw survey data (e.g., sav files from LimeSurvey)
- Normalize certain column names (handle variations like `CAAS_SQ00#` -> `CAAS_#`)
- Filter out test participants
- Define dataset groupings by country/wave
- Export processed .sav files to `DIR_SPLIT`

**Output**: Individual country/wave datasets ready for cleaning

Run the script:

```r
source("src/pipeline/01_split.R")
```

### 2. Clean Data (`02_clean.R`)

**Purpose**: Apply comprehensive quality control filters to the split datasets, with dataset-specific gating logic.

**Cleaning Steps**:
1. **Missing responses**: Remove rows with entire scale blocks missing
2. **Constant answers**: Remove straightlining cases (identical responses across items)
3. **Zigzag patterns**: Remove cases with alternating response patterns
4. **Control items**: Remove cases with invalid attention check responses
5. **Duration filtering**: Remove suspiciously fast completions
6. **Nationality filtering**: Keep only participants with valid nationalities (country-specific patterns)
7. **Mahalanobis distance**: Remove multivariate outliers
8. **Guttman scaling**: Remove cases with poor person-fit on scales

**Output**: 
- Cleaned datasets in `DIR_CLEAN`
- Summary reports with removal counts per step in `DIR_CLEAN`

Run the script:
```r
source("src/pipeline/02_clean.R")
```

### 3. Merge Datasets (`03_merge.R`)

**Purpose**: Consolidate cleaned datasets into a single merged dataset, standardize demographic variables, apply missing value coding, and prepare for analysis.

**Key Operations**:
- Fix and harmonize demographic variables (e.g., `Adults_*` columns)
- Apply missing value coding logic
- Merge across countries/waves
- Export merged dataset in multiple formats (SPSS, CSV, Excel)

Run the script:

```r
source("src/pipeline/03_merge.R")
```

## 📚 Scripts Documentation

### Pipeline Scripts

#### `01_split.R`
- **Function**: Preprocesses raw survey data
- **Key Functions**:
  - `normalize_column_names()`: Standardizes variable names
  - `write_clean()`: Removes all-NA columns before saving
- **Input**: `.sav` files in `DIR_RAW`
- **Output**: Processed `.sav` files in `DIR_SPLIT`

#### `02_clean.R`
- **Function**: Main cleaning pipeline orchestrator
- **Defines**: Multi-step cleaning workflow with dataset-specific gating
- **Uses**: Functions from `cleaning_functions.R`
- **Input**: Processed `.sav` files from step 1 (`DIR_SPLIT`)
- **Output**: Clean datasets + audit reports

#### `03_merge.R`
- **Function**: Merges cleaned datasets
- **Special handling**: 
  - Adults demographic variable (wide ↔ long transformation)
  - Missing value reason codes
- **Input**: Clean `.sav` files from step 2 (`DIR_CLEAN`)
- **Output**: Merged dataset(s) in multiple formats in `DIR_MERGED`

### Utility Scripts

#### `cleaning_functions.R`
Core cleaning utilities:

**Structure Builders**:
- `mk_step()`: Define individual cleaning steps
- `mk_group()`: Group steps with shared logic

**Data Quality Functions**:
- `step_drop_na_block()`: Remove rows with missing scale blocks
- `step_constant_answers()`: Detect straightlining
- `step_detect_zigzag()`: Identify alternating patterns
- `step_check_control()`: Validate attention checks
- `step_filter_age()`: Age-based filtering (default 18-65)
- `step_filter_min_duration()`: Remove fast completions
- `step_remove_foreigners()`: Nationality filtering

**Statistical Outlier Detection**:
- `step_mahalanobis()`: Multivariate outlier detection
- `step_guttman()`: Person-fit analysis (via PerFit)

**Pipeline Execution**:
- `run_cleaning_pipeline()`: Execute steps with gating logic
- `build_wide_summary()`: Generate removal audit reports

#### `merge_functions.R`
Merging and labeling utilities:

- `get_schema()`: Extract variable labels, value labels, and metadata
- `apply_schema_from()`: Apply label schema from one variable to another
- `augment_with_reasons()`: Add missing value reason codes
- **Missing value codes** (SPSS allows max 3 user-missing values per variable):
  - 990: `by_design` — variable not collected in this dataset (padding NA from merge)
  - 991: `technical_error` — known data issue (e.g. NL dataset LPS_v2_6 items 1–97)
  - 999: `missing` — participant did not respond (true missing)


## 📊 Dataset Groupings

Defined in `config/paths.R`:

```r
DATASETS <- list(
  brpt = c("BR_PILOT", "BRPT_277273"),
  cn = c("CN_277273"),
  us = c("US_216254", "US_868141"),
  cn_us_10_min = c("CN_277273", "US_868141"),
  pilot = c("BR_PILOT"),
  # first-stage datasets (all except IT_AUTO, which uses second-stage LPS and FTOS short scales)
  first_stage = c(
    "CN_277273", "IT_277273",
    "BRPT_277273", "SI_277273",  "US_216254", "US_868141"
  ),
  first_stage_brpt = c("BRPT_277273"),
  # list of datasets to remove
  datasets_to_remove = c("NL_999625", "BRPT_999625", "CN_999625", "ES_277273", "IT_999625", "MY_999625")
)
```

**Countries and languages** (see `CLAUDE.md` for the authoritative table):

| Country | Language | Code |
|----------|----------|------|
| Brazil & Portugal (mixed pool) | Portuguese | `BRPT` |
| China | - | `CN` |
| India | English | `IN_EN` |
| India | Hindi | `IN_HI` |
| Indonesia | - | `ID` |
| Israel | Arabic | `IL_AR` |
| Italy | - | `IT` |
| Malaysia | - | `MY` |
| Mexico | - | `MX` |
| Mozambique | - | `MZ` |
| Netherlands | - | `NL` |
| Serbia | - | `RS` |
| Russia | - | `RU` |
| South Africa | - | `ZA` |
| Slovakia | - | `SK` |
| Slovenia | - | `SI` |
| Spain | - | `ES` |
| Türkiye | - | `TR` |
| United States | - | `US` |

**Survey instruments** (examples):
- `FTOS`: Future Time Orientation Scale
- `AS`: Authenticity Scale
- `MiLQ`/`MLQ`: Meaning in Life Questionnaire
- `CIPIP`: Circumplex of Interpersonal Problems
- `LS`: Life Satisfaction
- `Grit`: Grit Scale
- `CAAS`: Career Adapt-Abilities Scale
- `DASS`: Depression, Anxiety and Stress Scale
- `HS`: Hope Scale

## 📄 License

MIT License - see [LICENSE](LICENSE) file for details.
---

## Contact
Do you have questions, suggestions, or remarks? Feel free to make an issue or contact [Qixiang Fang](https://github.com/fqixiang).

<img src="docs/logos/soda.png" alt="SoDa logo" width="250px"/> 