# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# scales.R - Registry of scale column patterns and scale/dataset combinations
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# PURPOSE: Single source of truth for "what does each scale's column pattern
#          look like", consumed by:
#            - 02_clean.R's constant-answer and zigzag filters (via
#              CONSTANT_ANSWER_SCALES / ZIGZAG_SCALES + build_scale_steps())
#            - 02_clean.R's atypical-pattern (Mahalanobis/Guttman) filter's
#              scale_patterns_cn_us / scale_patterns_it_brpt_si lists
#            - 03_merge.R's numerical_cols (which scale columns survive the
#              pre-merge column selection)
#
# WHY THIS MATTERS: before this registry existed, the same scale's column
# pattern was hand-copied into 3-4 different places in 02_clean.R/03_merge.R.
# That let two real bugs survive undetected: the "LS"/BRS pattern
# ("^LS_BRS\\d+$") never matched the actual post-normalization column name
# ("BRS_#"), and "FS" ("^FS_\\d+$") never matched the actual "FS_SES#"
# columns - in both cases the check silently never ran, in two separate
# hand-copied places each. Fixing SCALE_PATTERNS once fixes every consumer.
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

# Canonical name -> column-pattern regex for every scale in the pipeline.
# Verified against the actual column names present in every real cleaned
# dataset (not just what earlier hand-written code assumed they'd be).
SCALE_PATTERNS <- c(
  FTOS_v1    = "^FTOS_v1_\\d+$",         # First-stage FTOS
  FTOS_v2    = "^FTOS_v2_\\d+$",         # Second-stage FTOS
  FTOS_pilot = "^FTOS_pilot_\\d+$",      # Pilot FTOS
  LPS_v1     = "^LPS_v1_\\d+$",          # First-stage LPS
  LPS_v2     = "^LPS_v2_\\d+$",          # Second-stage LPS
  LPS_pilot  = "^LPS_pilot_\\d+$",       # Pilot LPS
  DGI        = "^(Psy_)?DGI_?\\d+$",     # Dispositional Goal Instrumentality: "Psy_DGI#" everywhere
                                          # except IT_AUTO, which uses bare "DGI_#"
  LOT        = "^Psy_LOT\\d+$",          # Life Orientation Test (Brazil pilot, split from LoTeDGI)
  IPIP       = "^IPIP_\\d+$",            # Big Five personality inventory
  MLQ        = "^MLQ_\\d+$",             # Meaning in Life Questionnaire
  AS         = "^AS_\\d+$",              # Authenticity Scale
  GRIT       = "^GRIT_\\d+$",            # Grit Scale (perseverance)
  CAAS       = "^CAAS_\\d+$",            # Career Adapt-Abilities Scale
  CAAS_S     = "^CAAS_S_\\d+$",          # South African CAAS variant (ZA_AUTO only)
  ESS        = "^ES_\\d+$",              # Existential Scale
  ESW        = "^ESW_PS\\d+$",           # Existential Scale - Work
  HS         = "^HS_\\d+$",              # HS scale
  DASS       = "^DASS_\\d+$",            # Depression Anxiety Stress Scales
  IT         = "^IT_\\d+$",              # Italian Time Perspective scale
  DMF        = "^DMF_\\d+$",             # Decision Making Fluency
  BRS        = "^BRS_\\d+$",             # Life Satisfaction - Brief Resilience Scale (China only).
                                          # Was registered as "LS" with pattern "^LS_BRS\\d+$", which
                                          # never matched the actual post-normalization column name.
  FS         = "^FS_SES\\d+$",           # Flourishing Scale (China only). Was "^FS_\\d+$", which never
                                          # matched the actual "FS_SES#" columns.
  CIPIP      = "^Psy_CIPIP\\d+$"         # Circumplex of Interpersonal Problems (BRPT_277273 only).
                                          # Previously not referenced by any filter or by the merge.
)

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# `datasets` column format: a comma-separated string of tokens, each either
#   - a key in config/paths.R's DATASETS list (e.g. "brpt", "first_stage"),
#     resolved to that group's full vector of dataset names, or
#   - a literal dataset name not in DATASETS (e.g. "IT_AUTO", "SI_277273")
# NA means the check applies with no dataset restriction (mk_step(datasets = NULL)).
# See resolve_datasets() below for the resolution logic.
#
# WHY TWO SEPARATE TABLES (not one shared datasets column across filters):
#          The constant-answer and zigzag filters do NOT apply to the same
#          datasets for every scale that appears in both - e.g. IPIP's
#          constant-answer check applies to `brpt` (includes BR_PILOT) but
#          its zigzag check applies to `first_stage_brpt` (excludes
#          BR_PILOT); MLQ/AS similarly differ (zigzag also covers IT_AUTO,
#          which the constant-answer check does not). Forcing both filters
#          through one shared per-scale `datasets` column would silently
#          change which participants get checked. Keep this asymmetry
#          explicit rather than hidden behind a "convenient" merge.
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

# Scales checked by the "constant answer" (straightlining) filter.
# `scale_key` looks up the column pattern in SCALE_PATTERNS; `name` is the
# step's display name in the audit trail (sometimes the same, sometimes not,
# e.g. zigzag's "FTOS1"/"FTOS2" below).
CONSTANT_ANSWER_SCALES <- tibble::tribble(
  ~name,        ~scale_key,    ~datasets,
  "FTOS_v1",    "FTOS_v1",     NA_character_,   # Applies broadly
  "FTOS_v2",    "FTOS_v2",     "IT_AUTO",
  "FTOS_pilot", "FTOS_pilot",  NA_character_,   # Applies broadly
  "DGI",        "DGI",         NA_character_,   # Applies broadly (matches both Psy_DGI# and IT_AUTO's DGI_#)
  "LOT",        "LOT",         NA_character_,   # Applies broadly
  "IPIP",       "IPIP",        "brpt",
  "BRS",        "BRS",         "cn",
  "MLQ",        "MLQ",         "brpt,cn,us",
  "AS",         "AS",          "brpt,cn,us",
  "GRIT",       "GRIT",        "us"
)

# Scales checked by the zigzag (alternating-pattern) filter.
ZIGZAG_SCALES <- tibble::tribble(
  ~name,   ~scale_key,  ~datasets,
  "FTOS1", "FTOS_v1",   "first_stage",
  "FTOS2", "FTOS_v2",   "IT_AUTO",
  "LPS",   "LPS_v1",    "first_stage",
  "LPS2",  "LPS_v2",    "IT_AUTO",
  "MLQ",   "MLQ",       "first_stage_brpt,us,cn,IT_AUTO",
  "AS",    "AS",        "first_stage_brpt,us,cn,IT_AUTO",
  "IPIP",  "IPIP",      "first_stage_brpt",
  "HS",    "HS",        "first_stage_brpt",
  "CAAS",  "CAAS",      "cn",
  "ESS",   "ESS",       "cn",
  "ESW",   "ESW",       "cn",
  "GRIT",  "GRIT",      "us",
  "DASS",  "DASS",      "SI_277273",
  "IT",    "IT",        "IT_277273,IT_AUTO",
  "DMF",   "DMF",       "IT_277273,IT_AUTO"
)

# Resolve a comma-separated `datasets` spec (see format note above) into the
# character vector mk_step()/mk_group() expect, using DATASETS from
# config/paths.R. NA/NULL -> NULL (no restriction, check applies to everyone).
resolve_datasets <- function(spec) {
  if (is.null(spec) || is.na(spec)) {
    return(NULL)
  }
  tokens <- trimws(strsplit(spec, ",")[[1]])
  unlist(lapply(tokens, function(tok) {
    if (tok %in% names(DATASETS)) DATASETS[[tok]] else tok
  }), use.names = FALSE)
}

# Build a list of mk_step()s from a scale table (CONSTANT_ANSWER_SCALES or
# ZIGZAG_SCALES) and the step-factory function to apply (step_constant_answers
# or step_detect_zigzag) - both take a column-pattern regex as their first
# argument. `scale_key` is looked up in SCALE_PATTERNS to get that regex.
build_scale_steps <- function(scale_table, step_fn) {
  purrr::pmap(scale_table, function(name, scale_key, datasets) {
    pattern <- SCALE_PATTERNS[[scale_key]]
    if (is.null(pattern)) {
      stop(sprintf(
        "build_scale_steps: scale_key '%s' (step '%s') not found in SCALE_PATTERNS.",
        scale_key, name
      ), call. = FALSE)
    }
    mk_step(name, step_fn(pattern), datasets = resolve_datasets(datasets))
  })
}
