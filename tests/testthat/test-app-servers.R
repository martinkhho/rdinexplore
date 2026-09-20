# Evaluate only the server definition: no browser, downloads, or repository writes

app_server_fixture <- function(filename) {
  root <- tempfile("app-server-")
  dir.create(root)
  env <- new.env(parent = environment())
  for (package in c("shiny", "shinyjs", "readr")) {
    exports <- getNamespaceExports(package)
    list2env(setNames(lapply(exports, function(name) getExportedValue(package, name)), exports), env)
  }
  env$here <- function(...) file.path(root, ...)
  
  # Fail on global exports instead of silently redirecting them
  env$assign <- function(x, value, envir) {
    if (identical(envir, .GlobalEnv)) stop("Server attempted a global export: ", x)
    base::assign(x, value, envir = envir)
  }
  expressions <- parse(file.path(project_root, filename))
  for (expression in expressions) {
    if (is.call(expression) && identical(expression[[1]], as.name("<-")) &&
        identical(expression[[2]], as.name("server"))) {
      eval(expression, env)
      return(list(server = env$server, env = env, root = root))
    }
  }
  stop("Server definition not found")
}

test_that("ATC checklist reloads remove old observers", {
  dictionary <- tibble::tibble(
    atc_code = c("A", "A01", "A01A", "A01AA"),
    atc_name = c("One", "Two", "Three", "Four")
  )
  for (app in c("rdinexplore.R", "ratcexplore.R")) {
    fixture <- app_server_fixture(app)
    on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
    shiny::testServer(fixture$server, {
      updates <- character(0)
      session$sendInputMessage <- function(inputId, message) {
        updates <<- c(updates, inputId)
      }
      expect_length(atc_selected_flags(), 0L)
      rebuild_atc_structures(dictionary, save_cache = FALSE)
      expect_length(atc_selected_flags(), 4L)
      bind_atc_observers()
      session$setInputs(atc_A = FALSE)

      # Check that rebuilding or loading the checklist does not duplicate observers
      rebuild_atc_structures(dictionary, save_cache = FALSE)
      bind_atc_observers()
      expect_true(save_atc_checklist_cache())
      expect_true(load_cached_atc_checklist())
      bind_atc_observers()
      session$flushReact()
      updates <- character(0)
      session$setInputs(atc_A = TRUE)
      expect_identical(sum(updates == "atc_A01"), 1L, info = app)
      expect_equal(selected_count(), 1L)

      # Check that inputs from the old checklist no longer trigger updates
      replacement <- tibble::tibble(atc_code = "B", atc_name = "Replacement")
      rebuild_atc_structures(replacement, save_cache = FALSE)
      expect_length(atc_selected_flags(), 1L)
      bind_atc_observers()
      session$flushReact()
      updates <- character(0)
      session$setInputs(atc_A = FALSE)
      expect_length(updates, 0L)
    })
  }
})

test_that("CIHI checklist reloads remove old observers", {
  fixture <- app_server_fixture("rdinexplore.R")
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  shiny::testServer(fixture$server, {
    updates <- character(0)
    session$sendInputMessage <- function(inputId, message) {
      updates <<- c(updates, inputId)
    }
    formularies <- tibble::tibble(Jurisdiction = "Ontario", `Drug program` = "Plan")
    rebuild_cihi_formulary_structures(formularies)
    bind_cihi_observers()
    session$setInputs(cihi_jur_001 = FALSE, cihi_jur_001_prog_001 = FALSE)

    # Check that rebuilding the checklist does not duplicate observers
    rebuild_cihi_formulary_structures(formularies)
    bind_cihi_observers()
    session$flushReact()
    updates <- character(0)
    session$setInputs(cihi_jur_001 = TRUE)
    expect_identical(updates, "cihi_jur_001_prog_001")

    # Check that inputs from the old checklist no longer trigger updates
    rebuild_cihi_formulary_structures(NULL)
    session$flushReact()
    updates <- character(0)
    session$setInputs(cihi_jur_001_prog_001 = TRUE)
    expect_length(updates, 0L)
  })
})

test_that("DIN results react to reruns and ignore another session's exports", {
  fixture <- app_server_fixture("rdinexplore.R")
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

  # Simulate leftover data from another session
  fixture$env$merged <- tibble::tibble(din = "other-session")
  fixture$env$initial_dins <- tibble::tibble(din = "other-session")
  fixture$env$dict_cihi_benefits <- "other-session-benefit"

  # Start a session and check that it is empty
  shiny::testServer(fixture$server, {
    expect_identical(cihi_benefit_values, character(0))

    # Page 6 reviews missing ATC matches and can append selected CIHI rows; its
    # final merged results must reflect this session's current run only
    expect_equal(nrow(page6_final_merged_df()), 0L)
    expect_equal(nrow(uploaded_din_data()), 0L)

    # Check that results change when a run is replaced
    first <- tibble::tibble(din = "00000001")
    second <- tibble::tibble(din = "00000002")
    run_merged(first)
    expect_identical(page6_final_merged_df(), first)
    run_merged(second)
    expect_identical(page6_final_merged_df(), second)

    # Check that resetting clears both datasets
    uploaded_din_data(first)
    reset_for_new_datasets()
    expect_equal(nrow(page6_final_merged_df()), 0L)
    expect_equal(nrow(uploaded_din_data()), 0L)
  })
})

test_that("DPD summary uses refreshed source dates", {
  fixture <- app_server_fixture("rdpdexplore.R")
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  env <- fixture$env

  # Simulate initially old source and retrieval dates
  env$source_updated <- "2020-01-01"
  env$downloaded_at <- "2020-01-02"
  env$read_provenance_field <- function(path, section, field) {
    if (identical(section, "source")) env$source_updated else env$downloaded_at
  }

  # Simulate refreshed DPD source data without downloading real data
  env$dpd_load_cache <- function(...) {
    env$source_updated <- "2024-06-01"
    env$downloaded_at <- "2024-06-15"
    list(status = tibble::tibble())
  }

  # Return minimal data from the remaining processing steps
  env$dpd_filter_status_current <- function(status, ...) status
  env$dpd_combine <- function(...) tibble::tibble(DRUG_CODE = 1L)
  env$dpd_rollup_code <- function(x, ...) x

  shiny::testServer(fixture$server, {
    # Select the latest DPD, keep French columns, and continue
    session$setInputs(dpd_download_btn = 1L, keep_f_cols_choice = "yes")
    session$setInputs(continue_btn_dpd = 1L)
    expect_identical(current_page(), "2_end")

    # The output summary should use both refreshed provenance dates
    summary <- readr::read_csv(file.path(run_output_dir, "summary.csv"), show_col_types = FALSE)
    expect_identical(summary$Selection[summary$Item == "DPD last updated"], "2024-06-01")
    expect_identical(summary$Selection[summary$Item == "DPD last downloaded"], "2024-06-15")
  })
})
