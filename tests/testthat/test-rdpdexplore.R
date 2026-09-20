test_that("rdpdexplore removes French columns only when requested", {
  dpd <- list(
    drug = tibble::tibble(DRUG_CODE = 1L, BRAND_NAME = "A", BRAND_NAME_F = "A FR"),
    last_updated = "2024-01-01"
  )
  kept <- dpd_keep_f_cols(dpd, keep_f_cols = TRUE)
  removed <- dpd_keep_f_cols(dpd, keep_f_cols = FALSE)
  expect_true("BRAND_NAME_F" %in% names(kept$drug))
  expect_false("BRAND_NAME_F" %in% names(removed$drug))
  expect_identical(removed$last_updated, "2024-01-01")
})

test_that("rdpdexplore chooses the latest status with deterministic ties", {
  status <- tibble::tribble(
    ~DRUG_CODE, ~CURRENT_STATUS_FLAG, ~STATUS,                 ~STATUS_F, ~HISTORY_DATE,
    1L,         "N",                  "MARKETED",              "M",       "01-Jan-2024",
    1L,         "N",                  "CANCELLED POST MARKET", "C",       "01-Jan-2024",
    2L,         "Y",                  "MARKETED",              "M",       "01-Jan-2023",
    2L,         "N",                  "DORMANT",               "D",       "01-Jan-2024"
  )
  result <- dpd_filter_status_current(status, keep_f_cols = TRUE)
  expect_identical(result$CURRENT_STATUS[result$DRUG_CODE == 1L], "CANCELLED POST MARKET")
  expect_identical(result$CURRENT_STATUS[result$DRUG_CODE == 2L], "DORMANT")
  expect_true(inherits(result$CURRENT_STATUS_DATE, "Date"))
  expect_true("CURRENT_STATUS_F" %in% names(result))
})

test_that("rdpdexplore constructs ingredient strengths including dosage units", {
  ingredients <- tibble::tibble(
    STRENGTH = c("10", "NIL"),
    STRENGTH_UNIT = c("mg", "NIL"),
    STRENGTH_UNIT_F = c("mg", "NIL"),
    DOSAGE_VALUE = c("1", "NIL"),
    DOSAGE_UNIT = c("mL", "NIL"),
    DOSAGE_UNIT_F = c("mL", "NIL")
  )
  result <- .dpd_ingred_strength(ingredients, keep_f_cols = TRUE)
  expect_identical(result$STRENGTH[[1]], "10 mg / 1 mL")
  expect_identical(result$STRENGTH_F[[1]], "10 mg / 1 mL")
  expect_true(is.na(result$STRENGTH[[2]]))
  expect_true(is.na(result$STRENGTH_F[[2]]))
})

test_that("rdpdexplore rolls up ingredients without breaking strength pairs", {
  flat <- tibble::tribble(
    ~DRUG_CODE, ~INGREDIENT, ~STRENGTH, ~CURRENT_STATUS_DATE, ~CURRENT_STATUS, ~LOT_NUMBER, ~EXPIRATION_DATE, ~BRAND_NAME,
    1L,         "BETA",      "2 MG",    as.Date("2024-01-01"), "MARKETED",    "L1",       "2025-01-01",     "Z BRAND",
    1L,         "ALPHA",     "1 MG",    as.Date("2024-01-01"), "MARKETED",    "L1",       "2025-01-01",     "A BRAND"
  )
  result <- dpd_rollup_code(flat, keep_f_cols = FALSE)
  expect_identical(result$INGREDIENT, "ALPHA ! BETA")
  expect_identical(result$STRENGTH, "1 MG ! 2 MG")
  expect_identical(result$BRAND_NAME, "A BRAND ! Z BRAND")
  expect_identical(nrow(result), 1L)
})

test_that("rdpdexplore identifies unusable columns", {
  result <- dpd_problem_cols(tibble::tibble(
    all_missing = c(NA, NA),
    constant = c("x", "x"),
    varying = c(1, 2),
    partly_missing = c(1, NA)
  ))
  expect_identical(result$all_na_cols, "all_missing")
  expect_identical(result$no_variability_cols, "constant")
  expect_error(dpd_problem_cols(1), "expects a data.frame")
})
