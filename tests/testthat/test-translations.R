# These test the config/translations.R registry used by 03_merge.R to
# translate Sex/Gender_v1/Gender_v2/Nationality/Origin/ImmigrationCountry
# into English while preserving the original-language value in a parallel
# "<column>_original" column.

test_that("translate_categorical translates values present in the lookup table", {
  out <- translate_categorical(c("Mulher", "Homem"), GENDER_TRANSLATIONS)
  expect_equal(out, c("Female", "Male"))
})

test_that("translate_categorical leaves values absent from the lookup table unchanged", {
  # Covers missing-value codes ("990"/"991"/"999") and free-text answers that
  # aren't a single recognizable label/country name.
  out <- translate_categorical(c("990", "999", "no", "some free text"), GENDER_TRANSLATIONS)
  expect_equal(out, c("990", "999", "no", "some free text"))
})

test_that("GENDER_TRANSLATIONS covers every real gender label observed across datasets, in every source language", {
  # Portuguese (base template), English, Slovak, Dutch, Italian, Arabic.
  labels <- c(
    "Mulher", "Homem", "Prefiro não responder", "Queer/Não-Binário",
    "Homem transexual", "Mulher transexual", "other",
    "female", "male", "Male", "Female", "I prefer not to answer",
    "muž", "žena", "Vrouw", "Man",
    "Femmina", "Maschio", "Preferisco non rispondere",
    "انثى", "ذكر"
  )
  expect_true(all(labels %in% names(GENDER_TRANSLATIONS)))
})

test_that("GENDER_TRANSLATIONS maps Arabic Gender_v2 labels using the actual bare-alef spelling IL_AR_AUTO uses, not the hamza-alef spelling", {
  # Regression test: an earlier version of this table used "أنثى" (alef
  # with hamza, U+0623) - a plausible but wrong transcription. The real
  # decoded label (verified against IL_AR_AUTO's raw data) is "انثى" (bare
  # alef, U+0627), which silently failed to match and left 94 rows
  # untranslated.
  bare_alef_female <- intToUtf8(c(0x627, 0x646, 0x62B, 0x649))
  expect_true(bare_alef_female %in% names(GENDER_TRANSLATIONS))
  expect_equal(unname(GENDER_TRANSLATIONS[bare_alef_female]), "Female")
})

test_that("COUNTRY_TRANSLATIONS covers every real country name observed in Nationality/Origin/ImmigrationCountry, in every source language", {
  # Portuguese, Dutch, Italian, Turkish, Spanish, Russian, Arabic, plus
  # already-English spellings and known typo variants.
  countries <- c(
    "Brasil", "Eslovênia", "Estados Unidos da América", "Itália", "Malásia",
    "França", "Suíça", "Moçambique", "Alemanha", "Rússia", "Turquia",
    "Reino Unido (Escócia, Inglaterra, Irlando do Norte e País de Gales)",
    "South Africa", "south africa", "South africa", "israel", "Israel",
    "België", "Duitsland", "Países Bajos", "Italia", "Messico",
    "Almanya", "Almaya", "türkiye", "España", "México", "Mexico",
    "Россия", "РФ", "Таджикистан", "الاردن"
  )
  expect_true(all(countries %in% names(COUNTRY_TRANSLATIONS)))
})

test_that("COUNTRY_TRANSLATIONS does not attempt to translate compound free-text Origin answers", {
  # Origin is open-ended, not a closed dropdown - a few real answers are full
  # sentences or ambiguous single words rather than one country name. These
  # are deliberately absent from the table so translate_categorical() passes
  # them through unmodified instead of guessing.
  free_text <- c(
    "no",
    "Romania, non ho ancora la cittadinanza italiana. Ma vivo qui da tanti anni.",
    "Украина до 10 лет, потом Россия"
  )
  expect_false(any(free_text %in% names(COUNTRY_TRANSLATIONS)))
})

test_that("COUNTRY_TRANSLATIONS normalizes typo/case variants of the same country to one spelling", {
  expect_equal(unname(COUNTRY_TRANSLATIONS[c("South Africa", "south africa", "South africa")]),
               rep("South Africa", 3))
  expect_equal(unname(COUNTRY_TRANSLATIONS[c("israel", "Israel")]), rep("Israel", 2))
})
