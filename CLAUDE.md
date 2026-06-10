# FTOLP Data Pipeline

R pipeline for the "Future Time Orientation and Life Projects" (FTOLP) research project. Processes multi-country, multi-platform survey data (e.g., SPSS exports from LimeSurvey, Excel files) through three ordered stages: split → clean → merge.

## Repository layout

```
config/
  paths.R                  # All directory paths + DATASETS groupings — must be configured locally
src/
  pipeline/
    01_split.R             # Load raw .sav files, split by country/language, write to DIR_SPLIT
    02_clean.R             # Apply 8 QC filters, write to DIR_CLEAN + audit Excel
    03_merge.R             # Merge all cleaned files, standardize types, write to DIR_MERGED
  utils/
    cleaning_functions.R   # Step/group builders and all filter functions
    merge_functions.R      # Schema extraction, label helpers, reason codes
setup.R                    # One-time install; restores renv packages
archive/                   # Old scripts, docs, analysis files — ignore entirely
```

## Running the pipeline

Scripts are run in order by sourcing them in R (e.g. in RStudio):

```r
source("src/pipeline/01_split.R")
source("src/pipeline/02_clean.R")
source("src/pipeline/03_merge.R")
```

Each script calls `source(here::here("config", "paths.R"))` at the top. The `here` package resolves paths relative to the project root (where the `.Rproj` file lives).

## Configuration (`config/paths.R`)

Must be edited per user. Key variables:

```r
DATA_ROOT      # base data directory (not in this repo — lives on Nextcloud/local disk)
DIR_RAW        # raw LimeSurvey .sav exports
DIR_SPLIT      # output of 01_split.R (country subdirectories: DIR_SPLIT/CH/, DIR_SPLIT/US/, ...)
DIR_CLEAN      # output of 02_clean.R
DIR_EXTERNAL   # external/manually provided files (conditions.sav, duplicates, etc.)
DIR_MERGED     # output of 03_merge.R
DATASETS       # named list of dataset groupings (see below)
```

`DATASETS` is a named list of character vectors used to gate which filters apply to which datasets during cleaning, and which datasets are dropped entirely:

```r
DATASETS$first_stage        # CH_277273, IT_277273, BR_PT_277273, SL_277273, US_216254, US_868141
DATASETS$br_pt              # br_pilot, BR_PT_277273
DATASETS$us                 # US_216254, US_868141
DATASETS$ch_us_10_min       # CH_277273, US_868141 — get extra 10-min duration filter
DATASETS$datasets_to_remove # skipped entirely during 02_clean.R
```

## Dataset naming conventions

### LimeSurvey
Raw LimeSurvey exports are named by survey ID (e.g. `277273.sav`). After `01_split.R`:

- After `01_split.R`, split files are written to `DIR_SPLIT/COUNTRY/COUNTRY_[LANGUAGE]_SURVEYID.sav` (e.g. `DIR_SPLIT/BR/BR_PT_277273.sav`)
- Each row gets a composite string ID: `COUNTRY_[LANGUAGE]_SURVEYID_originalID` (e.g. `BR_PT_277273_1`)
- `dataset` column stores a short country/group code (e.g. `"BR_PT"`)
- After `02_clean.R`, cleaned files are written to `DIR_CLEAN/COUNTRY/COUNTRY_[LANGUAGE]_SURVEYID_CLEAN.sav` (e.g. `DIR_CLEAN/BR/BR_PT_277273_CLEAN.sav`)

Exceptions:
- Not all datasets have language codes in their names (e.g., `"US_216254.sav"` and `"IT_277273.sav"`).
- The pilot Brazilian Portugese dataset is named `"BR_PILOT.sav"` after `01_split.R` and `"BR_PILOT_CLEAN.sav"` after `02_clean.R`. It is the only pilot dataset.

### Other sources
Other raw datasets have no consistent naming convention (e.g., `"Dataset_15.08.2022, RU (1).sav"` and `"Dataset US [students in the University of Oregon].sav"`). 
- After `01_split.R`, they are renamed to `DIR_SPLIT/COUNTRY/COUNTRY_AUTO_[NUMBER].sav` (e.g. `RU_AUTO_1.sav`).
- After `02_clean.R`, they are renamed to `DIR_CLEAN/COUNTRY/COUNTRY_AUTO_[NUMBER]_CLEAN.sav` (e.g. `RU_AUTO_1_CLEAN.sav`).
- `"AUTO"` means that the dataset is not from a known survey export and may have different variables, different variable names, different variable coding and other idiosyncrasies. It is treated independently during cleaning and merging. Most of the "AUTO" datasets are from the second stage, but some are from the first stage. 

## Survey stages
### Pilot
- BR_PILOT.sav

### Stage 1
- BR_PT_277273.sav
- CH_277273.sav
- IT_277273.sav
- IT_AUTO.sav
- SL_277273.sav
- US_216254.sav
- US_868141.sav

### Stage 2
- AR_999625.sav
- ES_999625.sav
- ID_999625.sav
- IL_AR_AUTO.sav
- IN_999625.sav
- IN_EN_824323.sav
- IN_EN_999625.sav
- IT_855796.sav
- MX_999625.sav
- MZ.sav
- NL_AUTO.sav
- RS_AUTO.sav
- RU_999625.sav
- RU_AUTO_1.sav
- RU_AUTO_2.sav
- SA_AUTO.sav
- SK_AUTO.sav
- TR_999625.sav

### Remove
- BR_PT_999625.sav
- CH_999625.sav
- EN_277273.sav
- EN_999625.sav
- ES_277273.sav
- IT_999625.sav
- MS_999625.sav
- NL_999625.sav

## Scale naming convention
Raw column names inconsistently can be inconsistent. `normalize_column_names()` and per-dataset renames in `01_split.R` standardize them:

| Stage | FTOS columns | LPS columns |
|---|---|---|
| Pilot | `FTOS_pilot_1` … `FTOS_pilot_15` | `LPS_pilot_1` … `LPS_pilot_21` |
| First stage (v1) | `FTOS_v1_1` … | `LPS_v1_1` … |
| Second stage (v2) | `FTOS_v2_1` … | `LPS_v2_1` … |

The `_v1`/`_v2` suffix ensures first- and second-stage items do not merge into the same column.

## Cleaning pipeline (`02_clean.R` + `cleaning_functions.R`)

Steps are defined with `mk_step()` / `mk_group()` and executed by `run_cleaning_pipeline()`. Seven filter steps (order matters):

1. Missing response — drops rows with all-NA core scale blocks (FTOS, LPS)
2. Short duration — removes completions below minimum time threshold
3. Constant answers — removes straightlining (identical responses across all items)
4. Zigzag patterns — removes alternating response patterns
5. Mahalanobis distance — removes multivariate outliers across scale items
6. Guttman errors — removes person-fit failures (via `PerFit` package)
7. Attention checks — removes failures on embedded control items (`FTOS_x`, `LPS_x`, `CAAS_x`)

`build_wide_summary()` generates the audit trail (`clean_summary.xlsx`) tracking N removed per step per dataset.

Each `mk_step()` has optional `exclude` and `datasets` arguments to gate which datasets it applies to.

## Merge pipeline (`03_merge.R` + `merge_functions.R`)

Key operations in order:

1. Load all `*_CLEAN.sav` from `DIR_CLEAN` (recursive, all subdirectories)
2. **Gender versioning**: rename `Gender` → `Gender_v1` for pilot/first_stage datasets; `Gender_v2` for all others
3. **Nationality/Citizen split**: 
   - pilot / first_stage / "auto" datasets: `Nationality` = country name string; derive `Citizen` (0/1) by matching against known country patterns
   - second_stage datasets: `Nationality` = multilingual yes/no citizen indicator → decode to `Citizen` (0/1); drop `Nationality`
4. **Column selection**: keep only `relevant_cols` (defined in script — scale items + key demographics)
5. **Type standardization**: resolve type conflicts before `bind_rows()` (categorical → character, scale items → numeric)
6. **Missing value labeling**: apply `labelled_spss()` with `na_values = c(990, 991, 999)`; replace R `NA` with `999`
7. **Pre-merge label normalization**: strip language-specific scale-point labels to prevent `bind_rows()` warnings
8. `bind_rows()` merge
9. **Padding NA labeling** via `label_merge_NAs()`: replace `NA`s introduced by `bind_rows()` with `990` ("by design")
10. Export as `.sav`, `.csv`, `.xlsx` to `DIR_MERGED`

## Missing value codes

These are numeric codes stored in SPSS user-missing slots (`na_values`):

| Code | Name | Meaning |
|---|---|---|
| 990 | `by_design` | Variable not collected in this dataset (padding NA from merge) |
| 991 | `technical_error` | Known data issue (e.g. NL dataset LPS_v2_6 items 1–97) |
| 999 | `missing` | Participant did not respond (true missing) |

SPSS allows max 3 user-missing values per variable — do not add more codes.

## Haven/labelled gotcha

Haven overrides `is.na()` for `labelled_spss` vectors: `is.na(990)` returns `TRUE` even though 990 is a real numeric value. When you need to distinguish true R `NA` from existing missing codes, use `unclass()` first:

```r
unclassed <- unclass(column_data)
is.na(unclassed)  # TRUE only for genuine R NAs
```

This pattern appears in `label_merge_NAs()` and must be preserved if that function is modified.

## Package management

Uses `renv`. To restore the locked environment:
```r
renv::restore()
```

Key packages: `tidyverse`, `haven`, `labelled`, `PerFit`, `writexl`, `here`, `rstatix`.
