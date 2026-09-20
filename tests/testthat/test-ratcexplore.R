dictionary_fixture <- tibble::tribble(
  ~atc_code, ~atc_name,
  " a ",     "ALIMENTARY TRACT AND METABOLISM",
  "A",       "Duplicate ignored",
  "A01",     "STOMATOLOGICAL PREPARATIONS",
  "A01A",    "STOMATOLOGICAL PREPARATIONS",
  "A01AA",   "Caries prophylactic agents",
  "A01AB",   "Antiinfectives and antiseptics for local oral treatment",
  "A01AA01", "sodium fluoride",
  "A01AA02", "sodium monofluorophosphate",
  "B",       "BLOOD AND BLOOD FORMING ORGANS",
  "B01",     "ANTITHROMBOTIC AGENTS",
  "B01A",    "ANTITHROMBOTIC AGENTS",
  "B01AA",   "Vitamin K antagonists",
  NA,         "Missing code",
  "",         "Empty code"
)

test_that("ratcexplore normalizes dictionaries and builds hierarchy maps", {
  hierarchy <- atc_build_hierarchy(dictionary_fixture)
  expect_identical(hierarchy$dictionary$atc_name[hierarchy$dictionary$atc_code == "A"], "ALIMENTARY TRACT AND METABOLISM")
  expect_identical(hierarchy$direct_children$A, "A01")
  expect_identical(hierarchy$direct_children$A01, "A01A")
  expect_setequal(hierarchy$descendants$A, c("A01", "A01A", "A01AA", "A01AB"))
  expect_false(any(nchar(hierarchy$all_codes) == 7L))
  expect_true(all(paste0("atc_", hierarchy$all_codes) == hierarchy$all_ids))
})

test_that("ratcexplore expands visible selections deterministically", {
  hierarchy <- atc_build_hierarchy(dictionary_fixture)
  expanded <- atc_expand_selection(c("A", "A"), hierarchy)
  expect_identical(expanded, c("A", "A01", "A01A", "A01AA", "A01AB"))
  expect_identical(atc_expand_selection(character(0), hierarchy), character(0))
})

test_that("ratcexplore expands selected prefixes to ATC5", {
  dictionary <- atc_normalize_dictionary(dictionary_fixture)
  atc5 <- atc_get_atc5_codes("A01AA", dictionary)
  expect_identical(atc5, c("A01AA01", "A01AA02"))

  table <- atc_build_final_table("A01AA", dictionary, include_atc5 = TRUE)
  expect_identical(table$code, c("A01AA", "A01AA01", "A01AA02"))
  expect_identical(table$level, c("ATC4", "ATC5", "ATC5"))
  expect_identical(table$description[[1]], "Caries prophylactic agents")
})

test_that("ratcexplore handles empty and invalid inputs", {
  dictionary <- atc_normalize_dictionary(dictionary_fixture)
  empty <- atc_build_table(c(NA, "", " "), dictionary)
  expect_identical(nrow(empty), 0L)
  expect_identical(atc_resolve_name("UNKNOWN", dictionary), "")
  expect_identical(atc_code_to_level("A01AA01"), "ATC5")
  expect_error(atc_normalize_dictionary(tibble::tibble(code = "A")), "missing required columns")
})

test_that("shared ATC hierarchy remains compatible with the checklist cache", {
  cache <- readRDS(file.path(project_root, "data", "atc_checklist_cached.rds"))
  hierarchy <- atc_build_hierarchy(cache$dict_who_atc)
  expect_identical(hierarchy$dictionary, cache$dict_who_atc)
  expect_identical(hierarchy$codes$atc1, cache$atc1_codes)
  expect_identical(hierarchy$codes$atc2, cache$atc2_codes)
  expect_identical(hierarchy$codes$atc3, cache$atc3_codes)
  expect_identical(hierarchy$codes$atc4, cache$atc4_codes)
  expect_identical(hierarchy$direct_children, cache$direct_children_map)
  expect_identical(hierarchy$descendants, cache$descendants_map)
})
