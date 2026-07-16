# FTOLP Data Pipeline

R pipeline for the "Future Time Orientation and Life Projects" (FTOLP) research project. Processes multi-country, multi-platform survey data (e.g., SPSS exports from LimeSurvey, Excel files) through three ordered stages: split → clean → merge.

## Repository layout

```
config/
  paths.R                  # All directory paths + DATASETS groupings — must be configured locally
  scales.R                 # Registry of scale column patterns, consumed by 02_clean.R and 03_merge.R
  translations.R           # English translation lookup tables for categorical demographic values
src/
  pipeline/
    01_split.R             # Load raw .sav files, split by country/language, write to DIR_SPLIT
    02_clean.R             # Apply 7 QC filters, write to DIR_CLEAN + audit Excel
    03_merge.R             # Merge all cleaned files, standardize types, write to DIR_MERGED
  utils/
    cleaning_functions.R   # Step/group builders and all filter functions
    merge_functions.R      # SPSS missing-value reason codes
    validation.R           # Loud-failure checks for dataset-name/config drift
tests/
  testthat.R                # Test suite entry point
  testthat/                 # Unit tests (see "Testing" below)
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
DIR_SPLIT      # output of 01_split.R (country subdirectories: DIR_SPLIT/CN/, DIR_SPLIT/US/, ...)
DIR_CLEAN      # output of 02_clean.R
DIR_EXTERNAL   # external/manually provided files (conditions.sav, duplicates, etc.)
DIR_MERGED     # output of 03_merge.R
DATASETS       # named list of dataset groupings (see below)
```

`DATASETS` is a named list of character vectors used to gate which filters apply to which datasets during cleaning, and which datasets are dropped entirely:

```r
DATASETS$first_stage        # CN_277273, IT_277273, BRPT_277273, SI_277273, US_216254, US_868141
DATASETS$brpt               # BR_PILOT, BRPT_277273
DATASETS$us                 # US_216254, US_868141
DATASETS$cn_us_10_min       # CN_277273, US_868141 — get extra 10-min duration filter
DATASETS$datasets_to_remove # skipped entirely during 02_clean.R
```

## Dataset naming conventions

### LimeSurvey
Raw LimeSurvey exports are named by survey ID (e.g. `277273.sav`). After `01_split.R`:

- After `01_split.R`, split files are written to `DIR_SPLIT/COUNTRY/COUNTRY[_LANGUAGE]_SURVEYID.sav` (e.g. `DIR_SPLIT/IL/IL_AR_999625.sav`). Note that LANGUAGE is optional and it mainly applies to datasets collected from Israel and India. Also, COUNTRY refers to the location of data collection, not the nationality of participants. The 2-letter ISO country codes are used (e.g., `BR`, `CN`, `IT`, `US`),
- Each row gets a composite string ID: `COUNTRY_[LANGUAGE]_SURVEYID_originalID` (e.g. `IL_AR_999625_1`)
- `dataset` column stores a short country/group (plus language) code (e.g. `"BRPT"`)
- After `02_clean.R`, cleaned files are written to `DIR_CLEAN/COUNTRY/COUNTRY_[LANGUAGE]_SURVEYID_CLEAN.sav` (e.g. `DIR_CLEAN/IL/IL_AR_999625_CLEAN.sav`)

Exceptions:
- Most datasets do not have language codes in their names (e.g., `"US_216254.sav"` and `"IT_277273.sav"`).
- The pilot Brazilian Portugese dataset is named `"BR_PILOT.sav"` after `01_split.R` and `"BR_PILOT_CLEAN.sav"` after `02_clean.R`. It is the only pilot dataset.
- `BRPT` (e.g. `"BRPT_277273.sav"`) is a single fused code, not a COUNTRY_LANGUAGE compound: it denotes a pool of participants from Brazil and Portugal that cannot be distinguished from each other in the data, so it is written without an underscore between BR and PT.

### Other sources
Other raw datasets have no consistent naming convention (e.g., `"Dataset_15.08.2022, RU (1).sav"` and `"Dataset US [students in the University of Oregon].sav"`). 
- After `01_split.R`, they are renamed to `DIR_SPLIT/COUNTRY/COUNTRY_AUTO_[NUMBER].sav` (e.g. `RU_AUTO_1.sav`).
- After `02_clean.R`, they are renamed to `DIR_CLEAN/COUNTRY/COUNTRY_AUTO_[NUMBER]_CLEAN.sav` (e.g. `RU_AUTO_1_CLEAN.sav`).
- `"AUTO"` means that the dataset is not from a known survey export and may have different variables, different variable names, different variable coding and other idiosyncrasies. It is treated independently during cleaning and merging. Most of the "AUTO" datasets are from the second stage, but some are from the first stage. 

### Countries and languages
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


## Survey stages
The most important difference between the three survey stages is the version of the FTOS and LPS scales used: pilot (version), v1, or v2. Some demographic varialbles such as gender and education may also be operationalised differently. Besides differences across survey stages, different countries may include additional demographic variables and psychological measures. The cleaning and merging pipeline is designed to handle these differences.

The three stages and their corresponding datasets after `01_split.R` are:

### Pilot stage
- BR_PILOT.sav

### Stage 1
- BRPT_277273.sav
- CN_277273.sav
- IT_277273.sav
- IT_AUTO.sav
- SI_277273.sav
- US_216254.sav
- US_868141.sav

### Stage 2
- ES_999625.sav
- ID_999625.sav
- IL_AR_999625.sav
- IL_AR_AUTO.sav
- IN_HI_999625.sav
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
- ZA_AUTO.sav
- SK_AUTO.sav
- TR_999625.sav

### Remove
- BRPT_999625.sav
- CN_999625.sav
- ES_277273.sav
- IT_999625.sav
- MY_999625.sav
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
10. **`source_country` derivation**: `derive_source_country()` extracts the country code (e.g. `CN`, `BRPT`, `IL`) from `source_dataset` by taking the token before the first underscore — the same convention `write_processed()` uses to group split-stage files into `DIR_SPLIT/<country>/` subdirectories. Language-suffixed datasets (`IL_AR`, `IN_HI`, `IN_EN`) collapse to the bare country code; `BR_PILOT` collapses to `BR`; `BRPT` is not split further (it's a single fused code, not a country_language compound). Also added to `lps_goals.sav`.
11. **Categorical translation** (`config/translations.R`): translate `Sex`/`Gender_v1`/`Gender_v2`/`Nationality`/`Origin`/`ImmigrationCountry` to English via `GENDER_TRANSLATIONS`/`COUNTRY_TRANSLATIONS` lookup tables, keeping the original-language value in a parallel `<column>_original` column (e.g. `Sex_original`). Values with no table entry (missing codes `990`/`991`/`999`, and a few free-text `Origin` answers that are full sentences rather than a single country name) pass through unchanged. `Gender_other` is deliberately excluded — it's open-ended free text (self-described gender identity), not a bounded label set.
12. Export as `.sav`, `.csv`, `.xlsx` to `DIR_MERGED`

**Education / Occupation**: `Education` is a 1-10 ordinal question with the same numeric coding in every dataset except NL, RS, and SK (verified against every real dataset's raw value labels) — labels differ by language, but the values line up, so `03_merge.R` re-attaches one canonical English label set (`EDUCATION_LABELS`, see `config/translations.R`) to the unchanged numeric codes rather than remapping values. NL and SK used their own closed-ended scales (8 and 5 levels) — kept as separate `Education_NL`/`Education_SK` columns (own English label sets, `EDUCATION_NL_LABELS`/`EDUCATION_SK_LABELS`), renamed at the `01_split.R` stage so they never merge into the main `Education` column. RS's education question was free text, not closed-ended — kept as `Education_RS` (character, untranslated, same treatment as `Gender_other`). Occupation is 6 binary flags (`Occupation_student`/`grantholder`/`worker`/`jobless`/`retired`/`other`, 0/1) collected the same way in every dataset except Serbia (not collected at all) and Slovakia (`Occupation_SK` — a single 5-category choice, translated via `OCCUPATION_SK_TRANSLATIONS`, kept separate). `Occupation_other` is character free text in most datasets but already a 0/1 flag in a few — normalized to 0/1 (1 = wrote something) before merging; the free text itself isn't retained. Three datasets (IL_AR_AUTO, NL_AUTO, RU_AUTO_1) only ever record an explicit `1` when an occupation checkbox is checked, leaving unchecked boxes as raw `NA` rather than an explicit `0` — `recode_unchecked_occupation_na()` in `01_split.R` recodes that `NA` to `0` before it would otherwise be coded `999` (missing) by the standard pipeline. ZA_AUTO used 5 numbered checkboxes instead of the named flags — mapped onto the standard set (occupation_2 OR occupation_3 → `Occupation_worker`, collapsing the full/part-time distinction).

**LPSgoal\* file**: `LPSgoal#_content` (free-text goal description) and `LPSgoal#_age` (target age) columns are open-ended per-goal items, not part of `relevant_cols`. `extract_lps_goals()` pulls `id` + all `LPSgoal*` columns from each dataset (before step 4's column selection would otherwise drop them) and writes them separately as `lps_goals.sav`/`.csv`/`.xlsx` to `DIR_MERGED`, joinable back to `merged_dataset` via `id`. Unanswered items are left as plain `NA` (this extraction runs before step 6's missing-value coding).

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

## Testing

Run the test suite from the project root:
```r
Rscript -e "testthat::test_dir('tests/testthat')"
```

`01_split.R`/`02_clean.R`/`03_merge.R` are never sourced directly by the tests — they read/write real survey data from `DATA_ROOT` (a Nextcloud-synced path that exists on some machines), so `source()`-ing one runs the real pipeline against real data. `tests/testthat/helper-safe-sourcing.R` extracts only the specific function/object definitions under test using text markers, and `tests/testthat/helper-setup.R` additionally points `DIR_RAW`/`DIR_SPLIT`/`DIR_CLEAN`/`DIR_MERGED` at a nonexistent sandbox directory for the whole test session as a second line of defense. If you add tests that need to exercise more of a pipeline script, extend `helper-safe-sourcing.R` rather than sourcing the script directly.

## Package management

Uses `renv`. To restore the locked environment:
```r
renv::restore()
```

Key packages: `tidyverse`, `haven`, `labelled`, `PerFit`, `writexl`, `here`, `rstatix`, `testthat`.
