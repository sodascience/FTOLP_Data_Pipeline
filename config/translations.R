# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# translations.R - English translations for categorical demographic values
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# PURPOSE: Sex/Gender_v1/Gender_v2 and Nationality/Origin/ImmigrationCountry
# come out of different datasets in whatever language that dataset's raw
# export used (mostly Portuguese, the base LimeSurvey template's language,
# but also Arabic, Slovak, Dutch, Italian, Turkish, Russian, Spanish, and
# already-English). 03_merge.R uses these lookup tables to translate every
# recognized value to English for cross-dataset consistency, while keeping
# the original-language value in a parallel `<column>_original` column
# (e.g. `Sex_original`) so nothing is lost.
#
# COVERAGE: built from every distinct value actually observed in the real
# merged dataset (not guessed). Missing-value codes ("990"/"991"/"999") are
# deliberately not in these tables - translate_categorical() only replaces
# values it has an explicit translation for and leaves everything else
# (including those codes) untouched.
#
# Origin in particular is a free-text field, not a closed dropdown, so a few
# values are full sentences ("Romania, non ho ancora la cittadinanza
# italiana...") or ambiguous ("no") rather than a single country name -
# those are intentionally left out of COUNTRY_TRANSLATIONS and pass through
# unmodified, rather than guessing a translation for a compound answer.
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

GENDER_TRANSLATIONS <- c(
  # Portuguese (Sex, Gender_v1, Gender_v2 - the base LimeSurvey template)
  "Mulher"                      = "Female",
  "Homem"                       = "Male",
  "Prefiro não responder"       = "Prefer not to answer",
  "Queer/Não-Binário"           = "Queer/Non-binary",
  "Homem transexual"            = "Transgender man",
  "Mulher transexual"           = "Transgender woman",
  # Already-English
  "other"                       = "Other",
  "female"                      = "Female",
  "male"                        = "Male",
  "Male"                        = "Male",
  "Female"                      = "Female",
  "I prefer not to answer"      = "Prefer not to answer",
  # Slovak (SK_AUTO)
  "muž"                         = "Male",
  "žena"                        = "Female",
  # Dutch (NL_AUTO)
  "Vrouw"                       = "Female",
  "Man"                         = "Male",
  # Italian (IT_855796 / IT_AUTO)
  "Femmina"                     = "Female",
  "Maschio"                     = "Male",
  "Preferisco non rispondere"   = "Prefer not to answer",
  # Arabic (IL_AR_AUTO)
  "انثى"                        = "Female",
  "ذكر"                         = "Male"
)

COUNTRY_TRANSLATIONS <- c(
  # Portuguese (Nationality, Origin, ImmigrationCountry - the base template)
  "Brasil"                       = "Brazil",
  "Eslovênia"                    = "Slovenia",
  "Estados Unidos da América"    = "United States",
  "Itália"                       = "Italy",
  "Malásia"                      = "Malaysia",
  "França"                       = "France",
  "Suíça"                        = "Switzerland",
  "Moçambique"                   = "Mozambique",
  "Croácia"                      = "Croatia",
  "Luxemburgo"                   = "Luxembourg",
  "África do Sul"                = "South Africa",
  "Colômbia"                     = "Colombia",
  "Albânia"                      = "Albania",
  "Alemanha"                     = "Germany",
  "Birmânia"                     = "Myanmar",
  "Bósnia e Herzegovina"         = "Bosnia and Herzegovina",
  "Espanha"                      = "Spain",
  "Holanda"                      = "Netherlands",
  "Japão"                        = "Japan",
  "Polônia"                      = "Poland",
  "República Centro-Africana"    = "Central African Republic",
  "Romênia"                      = "Romania",
  "Rússia"                       = "Russia",
  "Sérvia"                       = "Serbia",
  "Turquia"                      = "Turkey",
  "Ucrânia"                      = "Ukraine",
  "Uruguai"                      = "Uruguay",
  "Bélgica"                      = "Belgium",
  "Irlanda"                      = "Ireland",
  "Austrália"                    = "Australia",
  "Canadá"                       = "Canada",
  "Noruega"                      = "Norway",
  "Nova Zelândia"                = "New Zealand",
  "República Tcheca"             = "Czech Republic",
  "Hungria"                      = "Hungary",
  "Paraguai"                     = "Paraguay",
  "Singapura"                    = "Singapore",
  "Suécia"                       = "Sweden",
  "Reino Unido (Escócia, Inglaterra, Irlando do Norte e País de Gales)" = "United Kingdom",
  # Already correctly-spelled English (kept as identity mappings so casing
  # stays consistent with the typo variants below)
  "Portugal"                     = "Portugal",
  "China"                        = "China",
  "Macau"                        = "Macau",
  "Hong Kong"                    = "Hong Kong",
  "Venezuela"                    = "Venezuela",
  "Angola"                       = "Angola",
  "Taiwan"                       = "Taiwan",
  "Argentina"                    = "Argentina",
  "Moldova"                      = "Moldova",
  "Peru"                         = "Peru",
  "Malta"                        = "Malta",
  "Zimbabwe"                     = "Zimbabwe",
  "South Africa"                 = "South Africa",
  "south africa"                 = "South Africa",
  "South africa"                 = "South Africa",
  "Lesotho"                      = "Lesotho",
  "Tanzania"                     = "Tanzania",
  "Chile"                        = "Chile",
  "Colombia"                     = "Colombia",
  "Cuba"                         = "Cuba",
  "India"                        = "India",
  "Romania"                      = "Romania",
  "Congo"                        = "Congo",
  "Israel"                       = "Israel",
  "israel"                       = "Israel",
  "Mozambique"                   = "Mozambique",
  "Nicaragua"                    = "Nicaragua",
  "Suriname"                     = "Suriname",
  "Uganda"                       = "Uganda",
  "UK"                           = "United Kingdom",
  "Uzbekistan"                   = "Uzbekistan",
  "Zambia"                       = "Zambia",
  # Dutch (Origin - NL_AUTO)
  "België"                       = "Belgium",
  "Duitsland"                    = "Germany",
  "Bulgarije"                    = "Bulgaria",
  "Frankrijk"                    = "France",
  "Kroatie"                      = "Croatia",
  "Países Bajos"                 = "Netherlands",
  "Tsjechië"                     = "Czech Republic",
  # Italian (Origin)
  "Italia"                       = "Italy",
  "Messico"                      = "Mexico",
  "Svizzera"                     = "Switzerland",
  "Ucraina"                      = "Ukraine",
  # Turkish (Origin)
  "Almanya"                      = "Germany",
  "Almaya"                       = "Germany",
  "Özbekistan"                   = "Uzbekistan",
  "türkiye"                      = "Turkey",
  # Spanish (Origin)
  "España"                       = "Spain",
  "Francia"                      = "France",
  "México"                       = "Mexico",
  "Mexico"                       = "Mexico",
  # Russian (Origin)
  "Россия"                       = "Russia",
  "РФ"                           = "Russia",
  "Таджикистан"                  = "Tajikistan",
  "Армения"                      = "Armenia",
  "Узбекистане"                  = "Uzbekistan",
  "России"                       = "Russia",
  "В росии"                      = "Russia",
  "Москва-Кыргызстан"            = "Moscow-Kyrgyzstan",
  # Arabic (Origin)
  "الاردن"                       = "Jordan"
)

# Replaces every value in `x` that has an entry in `lookup` with its
# translation; anything not in `lookup` (missing-value codes, free-text
# answers not covered above, unrecognized values) passes through unchanged.
translate_categorical <- function(x, lookup) {
  translated <- unname(lookup[x])
  unname(ifelse(is.na(translated), x, translated))
}
