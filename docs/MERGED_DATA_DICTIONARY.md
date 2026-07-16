# Merged dataset dictionary

Describes the two output datasets written to `DIR_MERGED` by `src/pipeline/03_merge.R`: **`merged_dataset`** (one row per participant) and **`lps_goals`** (one row per participant, joinable back to `merged_dataset` via `id`). Each is written as `.sav` (SPSS, with full value labels), `.csv`, and `.xlsx` (plain values — see [CSV/XLSX vs. SPSS](#csvxlsx-vs-spss) below).

Generated from a pipeline run on 13,180 participants across 26 datasets / 18 countries. Column names and value labels below are exact; row counts and prevalence numbers are a snapshot and will shift on the next pipeline run.

## Conventions used throughout

### Missing-value codes

Every numeric column uses the same three SPSS user-missing codes (see `src/utils/merge_functions.R`, `reason_codes`):

| Code | Meaning | When it's used |
|---|---|---|
| `990` | By design | The source dataset never collected this variable at all (e.g. `Education_NL` is `990` for every non-Dutch participant) |
| `991` | Technical error | A known data-collection issue. Currently only `LPS_v2_6` for the Netherlands (`NL_AUTO`) participants 1–97, a documented instrument bug |
| `999` | Missing | The participant was asked but didn't answer |

Character columns use the same three codes as literal strings (`"990"`, `"991"`, `"999"`) instead of SPSS user-missing values, since SPSS string variables can't declare user-missing values the normal way.

In the `.sav` files, read with `haven::read_sav(..., user_na = TRUE)` (R) or open directly in SPSS to see these as proper missing values with labels ("by_design"/"technical_error"/"missing") rather than the raw numbers 990/991/999.

### English translation + `_original` columns

`Sex`, `Gender_v1`, `Gender_v2`, `Nationality`, `Origin`, `ImmigrationCountry`, and `Occupation_SK` were collected in whatever language that dataset's raw survey used (mostly Portuguese, plus Arabic, Slovak, Dutch, Italian, Turkish, Russian, Spanish). Each of these has been translated to English for cross-dataset consistency, with the untranslated value kept in a parallel `<column>_original` column (e.g. `Sex_original` holds `"Mulher"` where `Sex` holds `"Female"`). See `config/translations.R` for the full lookup tables.

A few free-text answers (mostly in `Origin`, which is open-ended rather than a closed dropdown) aren't single country names and were left untranslated in both columns rather than guessed — e.g. `"Romania, non ho ancora la cittadinanza italiana..."`.

`Gender_other` and `Education_RS` are genuinely open-ended free text (self-described gender identity; Serbia's education question wasn't closed-ended) and are **not** translated.

### `id` format

`COUNTRY[_LANGUAGE]_SURVEYID_originalID`, e.g. `CN_277273_123`, `IL_AR_999625_45`, `BRPT_277273_7`. The Brazil pilot dataset (`source_dataset == "BR_PILOT_clean"`) uses its raw survey number instead, e.g. `BR_569687_11100001` — `source_country` still resolves correctly to `BR` either way. Unique per row in `merged_dataset`; the same `id` values also appear in `lps_goals` (joinable 1:1), not necessarily in the same row order.

---

## `merged_dataset`

### Identifiers

| Column | Type | Description |
|---|---|---|
| `id` | string | Unique participant identifier, see [id format](#id-format) |
| `source_dataset` | string | Which cleaned source file this row came from, e.g. `"CN_277273_clean"` |
| `source_country` | string | 2-letter (or `BRPT`) country code derived from `source_dataset`, e.g. `"CN"`, `"BRPT"`, `"IL"` (language-suffixed datasets like `IL_AR`/`IN_HI`/`IN_EN` collapse to the bare country code) |

### Demographics

| Column | Type | Description |
|---|---|---|
| `Nationality` / `Nationality_original` | string | Country of nationality, translated to English. `990` for datasets where this wasn't asked as a free country name (second-stage datasets asked a yes/no citizenship question instead — see `Citizen`) |
| `Citizen` | numeric 0/1 | `1` = citizen of the country they were surveyed in, `0` = not. Derived from `Nationality` (pilot/first-stage/"auto" datasets) or a direct yes/no question (second-stage datasets) |
| `Origin` / `Origin_original` | string | Free-text "country where you grew up", translated to English where a single recognizable country name was given. A handful of compound/free-text answers are left untranslated in both columns (e.g. someone who described growing up in two countries) |
| `ImmigrationCountry` / `ImmigrationCountry_original` | string | Country of immigration (if applicable), translated to English |
| `Sex` / `Sex_original` | string | `"Male"` / `"Female"`, translated to English. Only present for pilot/first-stage datasets (second-stage datasets use `Gender_v2` instead — see below) |
| `Gender_v1` / `Gender_v1_original` | string | Gender, from pilot/first-stage datasets. Values: `Male`, `Female`, `Other`, `Prefer not to answer`, `Queer/Non-binary`, `Transgender man`, `Transgender woman` |
| `Gender_v2` / `Gender_v2_original` | string | Gender, from second-stage/"auto" datasets. Values: `Male`, `Female`, `Other`, `Prefer not to answer` |
| `Gender_other` | string | Open-ended "please specify" free text for gender, **not translated** (self-described identity terms, e.g. `"Non binary"`, `"Genderfluide"`, `"Nebinarna oseba"`) |
| `Age` | numeric | Age in years. For `RS_AUTO` (Serbia), the raw question was free text mixing plain ages, birth years, and refusals — birth years were converted to age using the known 2023 submission date, and unparseable text became `999` (missing) rather than guessed |
| `Immigration` | numeric 1/2 | Immigration status indicator (`990` where not collected) |
| `ImmigrationTime_years` | numeric | Years since immigration, range 0–45 |
| `ImmigrationTime_months` | numeric | Months since immigration, range 0–11 |

### Education

Collected as a closed-ended 1–10 ordinal question with **the same numeric coding in every dataset except the Netherlands, Serbia, and Slovakia** — the label text differs by language, but the underlying scale (incomplete/complete × primary/secondary/bachelor's/master's/doctoral) is identical, so those datasets are merged into one `Education` column.

| Column | Type | Description |
|---|---|---|
| `Education` | numeric 1–10 (labelled) | `1` Some primary school, `2` Complete primary school, `3` Some secondary school, `4` Complete secondary school, `5` Some bachelor's degree (or equivalent), `6` Complete bachelor's degree, `7` Some Master's degree, `8` Complete Master's degree, `9` Some Doctoral degree, `10` Complete Doctoral degree. `990` for NL/RS/SK participants (see below) |
| `Education_NL` | numeric 1–8 (labelled) | Netherlands only — Dutch education system, not comparable to the 1–10 scale. `1` Primary school not completed, `2` Primary school completed, `3` Lower secondary/vocational (VMBO/MBO-1), `4` Upper secondary (HAVO/VWO/MBO-2 to MBO-4), `5` Bachelor's degree (HBO, applied sciences), `6` Bachelor's degree (WO, academic), `7` Master's degree, `8` Doctorate. `990` for everyone else |
| `Education_SK` | numeric 1–5 (labelled) | Slovakia only — Slovak education system. `1` Primary school, `2` Secondary school without school-leaving exam, `3` Secondary school with school-leaving exam, `4` Bachelor's degree, `5` Master's degree or higher. `990` for everyone else |
| `Education_RS` | string, free text | Serbia only — the raw question was open-ended, not multiple choice, so it's kept as-is (Serbian, untranslated). Common answers include `"Srednja škola"` (secondary school), `"Fakultet ili akademija (Visoko obrazovanje)"` (university/higher education), `"Osnovna škola"` (primary school), `"Master studije"`, `"Doktorske studije"`; also contains some off-topic free text (job titles, refusals). `990` for everyone else |

### Occupation

Collected as 6 checkbox-style binary flags with **the same structure in every dataset except Serbia (not collected at all) and Slovakia** (`Occupation_SK` — a single categorical choice instead of checkboxes).

| Column | Type | Description |
|---|---|---|
| `Occupation_student` | numeric 0/1 | Currently a student |
| `Occupation_grantholder` | numeric 0/1 | Holds a research/study grant |
| `Occupation_worker` | numeric 0/1 | Employed. (South Africa's dataset distinguished full-time/part-time employment; both were combined into this one flag) |
| `Occupation_jobless` | numeric 0/1 | Unemployed |
| `Occupation_retired` | numeric 0/1 | Retired |
| `Occupation_other` | numeric 0/1 | Selected "other" / wrote in a free-text occupation not covered above. The free-text content itself isn't retained in this dataset |
| `Occupation_SK` / `Occupation_SK_original` | string | Slovakia only, translated to English. Values: `Employed / self-employed`, `Unemployed`, `Student`, `Disability pension`, `Old-age pension / retired`. `990` for everyone else |

These are not mutually exclusive (a participant could be both a student and employed) — don't expect them to sum to the group total.

### Psychological / life-projects scales

All scale items are numeric Likert-type responses; missing-value codes (`990`/`991`/`999`) apply the same way as everywhere else. Item counts and response ranges below are as observed in the current data (a participant only sees the scale items relevant to their survey version, so `990` "by design" is expected and common for scales that only ran in some countries/waves).

| Prefix | Scale | Items | Range | Notes |
|---|---|---|---|---|
| `FTOS_v1_#` | Future Time Orientation Scale (first-stage wording) | 18 | 1–7 | Core scale for the study |
| `FTOS_v2_#` | Future Time Orientation Scale (second-stage wording) | 18 | 1–7 | |
| `FTOS_pilot_#` | Future Time Orientation Scale (pilot wording) | 15 | 1–5 | |
| `LPS_v1_#` | Life Projects Scale (first-stage wording) | 22 | 1–7 | Core scale for the study |
| `LPS_v2_#` | Life Projects Scale (second-stage wording) | 10 | 1–7 | `LPS_v2_6` has 25 rows coded `991` (technical error) for early NL participants — see [Missing-value codes](#missing-value-codes) |
| `LPS_pilot_#` | Life Projects Scale (pilot wording) | 21 | 1–5 | |
| `Psy_DGI#` / `DGI_#` | Dispositional Goal Instrumentality | 7 | 1–5 | Two raw column-name variants merged into one pattern |
| `Psy_LOT#` | Life Orientation Test | 6 | 1–5 | Brazil pilot only |
| `IPIP_#` | Big Five personality inventory | 25 | 1–5 | |
| `MLQ_#` | Meaning in Life Questionnaire | 10 | 1–7 | |
| `AS_#` | Authenticity Scale | 12 | 1–7 | |
| `GRIT_#` | Grit Scale (perseverance) | 10 | 1–5 | |
| `CAAS_#` | Career Adapt-Abilities Scale | 24 | 1–5 | |
| `CAAS_S_#` | Career Adapt-Abilities Scale, South African variant | 12 | 1–5 | South Africa (`ZA_AUTO`) only |
| `ES_#` | Existential Scale | 16 | 1–5 | |
| `ESW_PS#` | Existential Scale – Work | 11 | 1–5 | |
| `HS_#` | HS scale | 8 | 1–5 | |
| `DASS_#` | Depression Anxiety Stress Scales | 21 | 1–4 | |
| `IT_#` | Italian Time Perspective scale | 17 | 1–5 | |
| `DMF_#` | Decision Making Fluency | 14 | 1–5 | |
| `BRS_#` | Brief Resilience Scale | 6 | 1–5 | China only |
| `FS_SES#` | Flourishing Scale | 8 | 1–7 | China only |
| `Psy_CIPIP#` | Circumplex of Interpersonal Problems | 24 | 1–5 | `BRPT_277273` only |

Attention-check items embedded in the raw surveys (`FTOS_x`, `LPS_x`, `CAAS_x`) were used during cleaning (`02_clean.R`) to drop participants who failed them, and are **not** present in this merged file.

---

## `lps_goals`

A separate file (9,732 rows, one per participant who has at least one goal recorded), because `LPSgoal#_content`/`LPSgoal#_age` are open-ended per-goal items rather than a fixed set of columns like everything else. Join back to `merged_dataset` on `id`.

| Column | Type | Description |
|---|---|---|
| `id` | string | Same participant identifier as `merged_dataset.id` |
| `source_dataset` | string | Same as `merged_dataset.source_dataset` |
| `source_country` | string | Same as `merged_dataset.source_country` |
| `LPSgoal1_content` … `LPSgoal15_content` | string | Free-text description of the participant's Nth life goal (up to 15 goals; most participants have far fewer) |
| `LPSgoal1_age` … `LPSgoal15_age` | numeric | Target age the participant gave for reaching that goal |

Unlike `merged_dataset`, unanswered goal slots here are left as plain `NA` rather than coded `999`/`990` — this file is extracted before the main missing-value coding step, since it isn't part of the same fixed-column schema.

---

## CSV/XLSX vs. SPSS

The `.csv` and `.xlsx` versions contain the same values as `.sav`, but without SPSS value-label metadata — numeric columns like `Education` or the psychological scales will show raw numbers (`1`–`10`, `990`/`991`/`999`, etc.) rather than the labels described in this document. Use the `.sav` file (opened in SPSS, or read with `haven::read_sav(..., user_na = TRUE)` in R) if you want labels attached, or refer to the tables above to interpret the raw values in the `.csv`/`.xlsx` files directly.
