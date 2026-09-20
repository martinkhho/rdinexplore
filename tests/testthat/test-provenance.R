test_that("provenance sidecars round-trip through YAML", {
  path <- tempfile(fileext = ".yml")
  on.exit(unlink(path), add = TRUE)

  provenance <- list(
    dataset = "example",
    source = list(
      name = "Example source",
      url = "https://example.com/data",
      last_updated = "2026-09",
      last_updated_precision = "month"
    ),
    retrieval = list(
      retrieved_at = "2026-09-19T14:30:00Z",
      timestamp_precision = "second",
      artifacts = list(
        list(
          name = "data.csv",
          url = "https://example.com/data.csv",
          sha256 = paste(rep("a", 64), collapse = "")
        )
      )
    )
  )

  write_provenance(provenance, path)
  result <- read_provenance(path)

  expect_identical(result, provenance)
  expect_identical(
    read_provenance_field(path, "source", "last_updated"),
    "2026-09"
  )
  expect_identical(
    read_provenance_field(path, "retrieval", "retrieved_at"),
    "2026-09-19T14:30:00Z"
  )
})

test_that("committed provenance sidecars contain artifact metadata", {
  for (dataset in c("dpd", "cihi")) {
    path <- file.path(project_root, "data", dataset, "provenance.yml")
    provenance <- read_provenance(path)

    expect_identical(provenance$dataset, dataset)
    expect_match(provenance$source$last_updated, "^\\d{4}-\\d{2}(-\\d{2})?$")
    expect_true(length(provenance$retrieval$artifacts) >= 1L)
    expect_true(all(vapply(
      provenance$retrieval$artifacts,
      function(artifact) grepl("^[[:xdigit:]]{64}$", artifact$sha256),
      logical(1)
    )))
  }
})

test_that("incomplete provenance sidecars fail clearly", {
  path <- tempfile(fileext = ".yml")
  on.exit(unlink(path), add = TRUE)

  # Write an intentionally incomplete provenance file that has `dataset`, but is
  # missing `source` and `retrieval`
  yaml::write_yaml(list(dataset = "example"), path)

  expect_error(read_provenance(path), "Incomplete provenance")
})
