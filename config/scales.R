# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# scales.R - Registry of scale/dataset combinations for repetitive QC filters
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# PURPOSE: 02_clean.R's constant-answer and zigzag filters used to be ~25
#          hand-written mk_step() calls that only differ by scale name,
#          column-pattern regex, and which datasets the check applies to.
#          These two tables are the single source of truth for that
#          combination; 02_clean.R generates the actual mk_step() list from
#          them via build_scale_steps().
#
# WHY TWO SEPARATE TABLES (not one shared "SCALES" table):
#          The constant-answer and zigzag filters do NOT apply to the same
#          datasets for every scale that appears in both - e.g. IPIP's
#          constant-answer check applies to `brpt` (includes BR_PILOT) but
#          its zigzag check applies to `first_stage_brpt` (excludes
#          BR_PILOT); MLQ/AS similarly differ (zigzag also covers IT_AUTO,
#          which the constant-answer check does not). Forcing both filters
#          through one shared per-scale `datasets` column would silently
#          change which participants get checked. Keep this asymmetry
#          explicit rather than hidden behind a "convenient" merge.
#
# `datasets` column format: a comma-separated string of tokens, each either
#   - a key in config/paths.R's DATASETS list (e.g. "brpt", "first_stage"),
#     resolved to that group's full vector of dataset names, or
#   - a literal dataset name not in DATASETS (e.g. "IT_AUTO", "SI_277273")
# NA means the check applies with no dataset restriction (mk_step(datasets = NULL)).
# See resolve_datasets() below for the resolution logic.
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

# Scales checked by the "constant answer" (straightlining) filter.
CONSTANT_ANSWER_SCALES <- tibble::tribble(
  ~name,        ~col_pattern,             ~datasets,
  "FTOS_v1",    "^FTOS_v1_\\d+$",         NA_character_,   # First-stage FTOS, applies broadly
  "FTOS_v2",    "^FTOS_v2_\\d+$",         "IT_AUTO",       # Second-stage FTOS, IT_AUTO only
  "FTOS_pilot", "^FTOS_pilot_\\d+$",      NA_character_,   # Pilot FTOS, applies broadly
  "DGI",        "^Psy_DGI\\d+$",          NA_character_,   # Brazil pilot: DGI items (split from LoTeDGI)
  "LOT",        "^Psy_LOT\\d+$",          NA_character_,   # Brazil pilot: LOT items (split from LoTeDGI)
  "IPIP",       "^IPIP_\\d+$",            "brpt",          # Big Five personality inventory
  "LS",         "^LS_BRS\\d+$",           "cn",             # Life Satisfaction - Brief Resilience Scale
  "MLQ",        "^MLQ_\\d+$",             "brpt,cn,us",    # Meaning in Life Questionnaire
  "AS",         "^AS_\\d+$",              "brpt,cn,us",    # Authenticity Scale
  "GRIT",       "^GRIT_\\d+$",            "us"              # Grit Scale (perseverance)
)

# Scales checked by the zigzag (alternating-pattern) filter.
ZIGZAG_SCALES <- tibble::tribble(
  ~name,   ~col_pattern,           ~datasets,
  "FTOS1", "^FTOS_v1_\\d+$",       "first_stage",                     # First-stage FTOS
  "FTOS2", "^FTOS_v2_\\d+$",       "IT_AUTO",                         # Second-stage FTOS, IT_AUTO only
  "LPS",   "^LPS_v1_\\d+$",        "first_stage",                     # First-stage LPS
  "LPS2",  "^LPS_v2_\\d+$",        "IT_AUTO",                         # Second-stage LPS, IT_AUTO only
  "MLQ",   "^MLQ_\\d+$",           "first_stage_brpt,us,cn,IT_AUTO",  # Meaning in Life Questionnaire
  "AS",    "^AS_\\d+$",            "first_stage_brpt,us,cn,IT_AUTO",  # Authenticity Scale
  "IPIP",  "^IPIP_\\d+$",          "first_stage_brpt",                # Big Five personality (BRPT_277273 only)
  "HS",    "^HS_\\d+$",            "first_stage_brpt",                # HS scale (BRPT_277273 only)
  "CAAS",  "^CAAS_\\d+$",          "cn",                               # Career Adapt-Abilities Scale
  "ESS",   "^ES_\\d+$",            "cn",                               # Existential Scale
  "ESW",   "^ESW_PS\\d+$",         "cn",                               # Existential Scale - Work
  "GRIT",  "^GRIT_\\d+$",          "us",                               # Grit Scale (perseverance)
  "DASS",  "^DASS_\\d+$",          "SI_277273",                        # Depression Anxiety Stress Scales
  "IT",    "^IT_\\d+$",            "IT_277273,IT_AUTO",                # Italian Time Perspective scale
  "DMF",   "^DMF_\\d+$",           "IT_277273,IT_AUTO"                 # Decision Making Fluency
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
# or step_detect_zigzag) - both take `col_pattern` as their first argument.
build_scale_steps <- function(scale_table, step_fn) {
  purrr::pmap(scale_table, function(name, col_pattern, datasets) {
    mk_step(name, step_fn(col_pattern), datasets = resolve_datasets(datasets))
  })
}
