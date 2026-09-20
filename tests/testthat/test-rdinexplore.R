test_that("rdinexplore builds safe ATC prefix filters", {
  expect_true(is.na(.build_prefix_regex(character(0))))
  pattern <- .build_prefix_regex(c(" A01 ", "A01", "B(2)"))
  expect_true(stringr::str_detect("A01AB", pattern))
  expect_true(stringr::str_detect("B(2)ZZ", pattern))
  expect_false(stringr::str_detect("B22", pattern))

  filtered <- dpd_filter_atc(tibble::tibble(atc4 = c("A01AB", "B01AA")), character(0))
  expect_identical(nrow(filtered), 0L)
})

test_that("rdinexplore extracts and filters CIHI rows", {
  cihi <- list(
    last_updated = "2024-06",
    data = tibble::tibble(
      Jurisdiction = c("Ontario", "Ontario", "Ontario", "Quebec", "Ontario"),
      `Drug program` = c("Plan", "Plan", "Plan", "Plan", "Plan"),
      `Benefit status` = c("Benefit", "Benefit", "Benefit", "Benefit", "Limited"),
      `ATC4 code` = c("A01AB", "Y99YY", "Z01AA", "A01AB", "B01AA"),
      `Active ingredients` = c("Drug a,Drug b", NA, "Drug z", "Drug q", "Drug b"),
      `ATC5 description` = c("Unused", "Fallback name", "Unused", "Unused", "Unused"),
      `PDIN flag` = c("N", "Y", "N", "N", "N"),
      `Brand name` = c("Brand A", "Brand Y", "Brand Z", "Brand Q", "Brand B"),
      `Coverage start date` = as.Date(c("2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01")),
      `Coverage end date` = c(NA, NA, NA, NA, NA),
      DIN = c("00000001", "P0000001", "00000003", "00000004", "00000005"),
      `Drug type` = c("Prescription", "Prescription", "Prescription", "Prescription", "Over the counter")
    )
  )

  result <- cihi_extract(
    cihi,
    selected_benefit = c(" Benefit ", "Benefit"),
    selected_formulary = c(" Ontario:::Plan ", "Ontario:::Plan"),
    atc = "A"
  )

  expect_setequal(result$din, c("00000001", "P0000001"))
  expect_identical(result$api[result$din == "00000001"], "DRUG A ! DRUG B")
  expect_identical(result$api[result$din == "P0000001"], "FALLBACK NAME")
  expect_true(all(result$end_coverage == as.Date("2024-06-01")))
  expect_identical(result$flag_pdin[result$din == "P0000001"], 1)

  periods <- tibble::tibble(
    din = c("end-on-start", "start-on-end", "outside"),
    start_coverage = as.Date(c("2019-01-01", "2020-12-31", "2021-01-01")),
    end_coverage = as.Date(c("2020-01-01", "2021-01-01", "2021-12-31"))
  )
  covered <- cihi_filter_covered(periods, as.Date("2020-01-01"), as.Date("2020-12-31"))
  expect_setequal(covered$din, c("end-on-start", "start-on-end"))
})

test_that("rdinexplore handles marketed-period boundaries and status ties", {
  status <- tibble::tribble(
    ~DRUG_CODE, ~CURRENT_STATUS_FLAG, ~STATUS,                 ~HISTORY_DATE,
    1L,         "N",                  "MARKETED",              "01-Jan-2020",
    1L,         "Y",                  "DORMANT",               "01-Jan-2021",
    2L,         "N",                  "DORMANT",               "01-Jan-2019",
    2L,         "Y",                  "MARKETED",              "31-Dec-2020",
    3L,         "N",                  "MARKETED",              "01-Jan-2019",
    3L,         "Y",                  "DORMANT",               "01-Jan-2020",
    4L,         "Y",                  "APPROVED",              "01-Jan-2020",
    5L,         "N",                  "MARKETED",              "01-Jun-2020",
    5L,         "N",                  "CANCELLED POST MARKET", "01-Jun-2020",
    6L,         "Y",                  "MARKETED",              "01-Jun-2020",
    6L,         "N",                  "CANCELLED POST MARKET", "01-Jun-2020"
  )

  result <- dpd_filter_marketed(status, as.Date("2020-01-01"), as.Date("2020-12-31"))
  expect_setequal(result$code, c(1L, 2L, 6L))
})

test_that("rdinexplore merges sources and preserves output invariants", {
  dpd <- tibble::tribble(
    ~din,        ~atc4,   ~api,          ~strength,     ~formulation, ~route, ~brand_name, ~schedule,      ~flag_biosimilar,
    "00000001", "A01AA", "ALPHA ! BETA", "1 MG ! 2 MG", "TABLET",     "ORAL", "DPD BRAND", "PRESCRIPTION", 0L
  )
  cihi <- tibble::tribble(
    ~din,       ~atc4,   ~api,     ~brand_name, ~flag_pdin,
    "00000001", "A01AA", "CIHI A", "CIHI BRAND", 0L,
    "P0000002", "A01AA", "PDIN",   "PDIN BRAND", 1L,
    "00000003", "B01AA", "GAMMA",  "UNMAPPED",   0L
  )
  dictionary <- tibble::tribble(
    ~atc_code, ~atc_name,
    "A01AA",   "Alpha class",
    "B01AA",   "Beta class"
  )

  result <- merged_dpd_cihi(dpd, cihi) %>% merged_format(dictionary)
  expect_setequal(result$din, c("00000001", "P0000002", "00000003"))
  expect_identical(result$brand_name[result$din == "00000001"], "DPD BRAND")
  expect_identical(result$flag_pdin[result$din == "P0000002"], 1L)
  expect_identical(result$flag_unmapped[result$din == "00000003"], 1L)
  expect_identical(result$n_api[result$din == "00000001"], 2L)
  expect_identical(result$atc4_descriptor[result$din == "00000001"], "Alpha class")
  expect_true(all(result$flag_pdin %in% 0:1))
  expect_true(all(result$flag_unmapped %in% 0:1))
  expect_false(any(result$flag_pdin == 1L & result$flag_unmapped == 1L))
})
