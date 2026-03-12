# FTOLP Data Pipeline

A comprehensive R-based data processing pipeline for the "Narrating the Future" (FTOLP) research project. This pipeline handles LimeSurvey data from multiple countries and survey waves, performing data splitting, quality control cleaning, and merging operations.

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

This pipeline processes survey data from LimeSurvey across multiple countries (Brazil, Portugal, China, USA, and others) and handles:

- **Data preprocessing**: Splitting and normalizing raw survey data
- **Quality control**: Removing invalid responses, detecting straightlining, zigzag patterns, and outliers
- **Data validation**: Checking control items and analyzing response durations
- **Data merging**: Consolidating cleaned datasets across countries and waves

## 📁 Directory Structure

```
FTOLP_Data_Pipeline/
├── config/
│   └── paths.R                    # Central configuration for paths and dataset groupings
├── src/
│   ├── pipeline/                  # Main processing pipeline scripts (run in order)
│   │   ├── 01_split_raw.R         # Split and preprocess raw LimeSurvey data
│   │   ├── 02_clean.R             # Quality control and data cleaning
│   │   └── 03_merge_general.R     # Merge cleaned datasets
│   ├── utils/                     # Utility functions
│   │   ├── cleaning_functions.R   # Core cleaning utilities (steps, filters, outlier detection)
│   │   ├── merge_functions.R      # Merging and labeling utilities
│   │   └── comparison_functions.R # Dataset comparison and validation functions
│   └── analysis/                  # Analysis and diagnostic scripts
│       ├── consolidate_datasets.R # Generate dataset overlap matrices
│       └── duration_analysis.R    # Survey completion time analysis
├── docs/                          # Additional documentation
├── README.md                      # This file
└── LICENSE                        # MIT License

```

## 🚀 Installation

### Prerequisites

- **R** (≥ 4.0.0)
- **RStudio** (recommended)

### Required R Packages

```r
install.packages(c(
  "tidyverse",    # Data manipulation
  "haven",        # SPSS file I/O
  "labelled",     # Variable labels
  "readxl",       # Excel files
  "lubridate",    # Date handling
  "writexl",      # Excel output
  "ggplot2",      # Visualization
  "here",         # Path management
  "rstatix",      # Statistical tests
  "PerFit"        # Person-fit analysis
))
```

## ⚙️ Configuration

### Setup Paths

Before running the pipeline, configure your paths in `config/paths.R`:

```r
# Base project directory
PROJECT_ROOT <- "~/Library/CloudStorage/Nextcloud-6161138@soliscom.uu.nl@surfdrive.surf.nl/Narrating the Future (Bogdan)"

# Data directories
DIR_RAW <- file.path(PROJECT_ROOT, "raw")
DIR_SPLIT <- file.path(PROJECT_ROOT, "split")
DIR_CLEAN <- file.path(PROJECT_ROOT, "clean")
```

Modify `PROJECT_ROOT` to match your local setup.

## 🔄 Pipeline Workflow

Run scripts in the following order:

### 1. Split Raw Data (`01_split_raw.R`)

**Purpose**: Load and preprocess raw .sav files from LimeSurvey

**Key Operations**:
- Load raw survey data
- Normalize column names (handle variations like `IT_IT1` → `IT_1`)
- Filter out test participants
- Define dataset groupings by country/wave
- Export processed .sav files to `LimeSurvey Processed/`

**Output**: Individual country/wave datasets ready for cleaning

**Important**: The script uses absolute paths via the `here` package and configuration files, so it can be run multiple times without path issues.

```r
source("src/pipeline/01_split_raw.R")
```

### 2. Clean Data (`02_clean.R`)

**Purpose**: Apply comprehensive quality control filters

**Cleaning Steps**:
1. **Missing responses**: Remove rows with entire scale blocks missing
2. **Constant answers**: Detect straightlining (identical responses across items)
3. **Zigzag patterns**: Identify alternating response patterns
4. **Control items**: Validate attention check responses
5. **Age filtering**: Keep respondents 18-65 (configurable)
6. **Duration filtering**: Remove suspiciously fast completions
7. **Nationality filtering**: Apply country-specific filters
8. **Mahalanobis distance**: Multivariate outlier detection
9. **Guttman scaling**: Person-fit analysis using PerFit package

**Output**: 
- Cleaned datasets in `LimeSurvey Processed/clean/`
- Summary reports with removal counts per step

```r
source("src/pipeline/02_clean.R")
```

### 3. Merge Datasets (`03_merge_general.R`)

**Purpose**: Consolidate cleaned datasets (in development)

**Key Operations**:
- Fix and harmonize demographic variables (e.g., `Adults_*` columns)
- Apply missing value coding logic
- Merge across countries/waves

**Status**: In development

```r
source("src/pipeline/03_merge_general.R")
```

## 📚 Scripts Documentation

### Pipeline Scripts

#### `01_split_raw.R`
- **Function**: Preprocesses raw LimeSurvey exports
- **Key Functions**:
  - `normalize_column_names()`: Standardizes variable names
  - `write_clean()`: Removes all-NA columns before saving
- **Input**: `.sav` files in `DIR_RAW`
- **Output**: Processed `.sav` files in `DIR_SPLIT`

#### `02_clean.R`
- **Function**: Main cleaning pipeline orchestrator
- **Defines**: Multi-step cleaning workflow with dataset-specific gating
- **Uses**: Functions from `cleaning_functions.R`
- **Input**: Processed `.sav` files from step 1
- **Output**: Clean datasets + audit reports

#### `03_merge_general.R`
- **Function**: Merges cleaned datasets
- **Special handling**: 
  - Adults demographic variable (wide ↔ long transformation)
  - Missing value reason codes
- **Input**: Clean `.sav` files from step 2
- **Output**: Merged dataset(s)

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
- **Missing value codes**:
  - 990: By design
  - 991: Unknown missing
  - 992: Technical error
  - 993: Refused
  - 994: Dubious
  - 995: Nonresponse
  - 999: Not applicable

#### `comparison_functions.R`
Dataset comparison and validation:

- `compare_dfs_compact()`: Comprehensive dataset comparison
  - Column name differences
  - Type mismatches
  - Label inconsistencies
  - Value differences
- Useful for QA and merging preparation

### Analysis Scripts

#### `consolidate_datasets.R`
**Purpose**: Generate comprehensive overlap matrix

Creates three matrices showing:
1. **Existence**: Which columns exist in which datasets
2. **All-NA detection**: Columns with no data
3. **Type tracking**: Data types (including haven-specific types)

**Output**: CSV with visual indicators:
- `✓ type`: Column present with data
- `✓✓ type`: Column present but all NA

**Use case**: Quickly identify which scales were administered in which countries/waves

```r
source("src/analysis/consolidate_datasets.R")
```

#### `duration_analysis.R`
**Purpose**: Statistical analysis of survey completion times

**Key Function**: `analyze_duration_histograms()`

**Features**:
- Per-page duration distributions
- Robust outlier detection (median ± 3×MAD)
- Z-scores based on Median Absolute Deviation (MAD)
- Faceted and individual page plots
- Outlier percentage tables

**Output**: Duration plots and CSV reports

**Note**: After analysis, duration filters were NOT applied to final cleaning

```r
source("src/analysis/duration_analysis.R")
```

## 📊 Dataset Groupings

Defined in `config/paths.R`:

```r
DATASETS <- list(
  br_pt = c("br_pilot", "PTBR_277273", "PTBR_999625"),
  ch = c("CH_277273", "CH_999625"),
  us = c("US_all", "US_216254", "US_868141"),
  first_stage = c("CH_277273", "EN_277273", "ES_277273", 
                  "IT_277273", "PTBR_277273", "SL_277273", 
                  "US_all", "IT_extra", "US_216254", "US_868141")
)
```

**Country codes**:
- `BR/PTBR`: Brazil
- `PT`: Portugal
- `CH`: China
- `US`: United States
- `EN`: English
- `ES`: Spanish (Spain)
- `IT`: Italian (Italy)
- `SL`: Slovenian (Slovenia)

**Survey instruments** (examples):
- `FTOS`: Future Time Orientation Scale
- `AS`: Authenticity Scale
- `MiLQ`/`MLQ`: Meaning in Life Questionnaire
- `CIPIP`: Circumplex of Interpersonal Problems
- `LS`: Life Satisfaction
- `Grit`: Grit Scale
- `CAAS`: Career Adapt-Abilities Scale
- `DASS`: Depression, Anxiety and Stress Scale

## 📄 License

MIT License - see [LICENSE](LICENSE) file for details.
---

## Contact
Do you have questions, suggestions, or remarks? Feel free to contact [Qixiang Fang](https://github.com/fqixiang).

<img src="docs/logos/soda.png" alt="SoDa logo" width="250px"/> 