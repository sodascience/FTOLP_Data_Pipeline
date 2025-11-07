Code overview 
-------------

# File description

## `clean.R`:
- Cleaning pipeline (steps)
- Contains the markup of the cleaning logic
- Generates summary with number of dropouts

## `consolidate.R`:
- Creates comprehensive overlap matrix:
    - Scans all .sav files in directory
    - Generates three matrices:
        - Existence: Which columns exist in which datasets
        - All-NA detection: Flags columns with no data
        - Type tracking: Records data types (including haven-specific types like labelled_spss)
    - Outputs CSV with visual indicators:
        - ✓ type: Column present with data type
        - ✓✓ type: Column present but all NA
- Use case: Quickly identify which scales were administered in which countries/waves
- Useful for merging

## `first_stage_duration.R`:

Response time analysis

Statistical analysis of survey completion times:

`analyze_duration_histograms()`: Creates duration distributions by survey page
Uses robust statistics (median and MAD) to identify outliers

Generates per-page histograms with outlier boundaries (±3×MAD)

Identifies suspiciously fast responses using MAD-based z-scores

Creates both combined faceted plots and individual page plots

Output: Duration plots and CSV tables showing outlier percentages by page.

- plots for analyzing survey duration for some datasets
- we decided to not use any duration filters after all

## `merge_general.R`:
- beginning of merge process
- fixes adults column
- has missing values logic that needs to be applied

## `merge_helper.R` and `merge_helper_extra.R`:
- general helper functions for comparing datasets, labels, etc.
- useful for writing final merge code

## `split raw.R`:
- loads raw .sav files from LimeSurvey
- pre-process raw datasets from livesurvey + other sources
- defines dataset groupings (Brazil/Portugal, China, US, etc.)
- contains all the logic for manipulating the raw data -> datasets to be cleaned

## `utils_cleaning.R`:
All the helper functions for `clean.R`

Core cleaning utility functions

Contains the building blocks for the cleaning pipeline:

Structure Builders:

`mk_step()`: Creates individual cleaning step definitions

`mk_group()`: Groups multiple steps with shared logic

Data Quality Functions:

`step_drop_na_block()`: Removes rows where entire scale blocks are missing
`step_constant_answers()`: Detects and removes "straightlining" (same answer repeated)
`step_detect_zigzag()`: Identifies alternating response patterns with configurable adjacency requirements
`step_check_control()`: Validates attention check items by keeping most common response
`step_filter_age()`: Age-based filtering (default 18-65)
`step_filter_min_duration()`: Removes suspiciously fast completions
`step_remove_foreigners()`: Filters by nationality code

Statistical Outlier Detection:

`step_mahalanobis()`: Multivariate outlier detection using Mahalanobis distance
`step_guttman()`: Person-fit analysis using Guttman scaling (via PerFit package)
Pipeline Execution:

`run_cleaning_pipeline()`: Executes steps with dataset-specific gating logic
`build_wide_summary()`: Generates audit reports showing removals at each step

# Order:

1. `split raw.R`
2. `clean.R`
3. `merge_general.R` (when completed)

