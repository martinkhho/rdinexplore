# ------------------------------------------------------------------------------
# Pure helpers for exploring WHO ATC codes
# ------------------------------------------------------------------------------

# Normalizes an ATC dictionary to one row per non-empty, upper-case code
atc_normalize_dictionary <- function(df_atc) {
  required <- c("atc_code", "atc_name")
  missing <- setdiff(required, names(df_atc))
  if (length(missing) > 0) {
    stop("ATC dictionary is missing required columns: ", paste(missing, collapse = ", "))
  }

  df_atc[, required, drop = FALSE] %>%
    dplyr::mutate(
      atc_code = stringr::str_to_upper(trimws(as.character(atc_code))),
      atc_name = trimws(as.character(atc_name))
    ) %>%
    dplyr::filter(!is.na(atc_code), nzchar(atc_code)) %>%
    dplyr::group_by(atc_code) %>%
    dplyr::summarise(
      atc_name = {
        values <- atc_name[!is.na(atc_name) & nzchar(atc_name)]
        if (length(values) == 0) NA_character_ else values[[1]]
      },
      .groups = "drop"
    )
}

# Builds the level-specific vectors and lookup maps used by both ATC viewers
atc_build_hierarchy <- function(df_atc) {
  dictionary <- atc_normalize_dictionary(df_atc)
  by_level <- list(
    atc1 = dplyr::filter(dictionary, nchar(atc_code) == 1),
    atc2 = dplyr::filter(dictionary, nchar(atc_code) == 3),
    atc3 = dplyr::filter(dictionary, nchar(atc_code) == 4),
    atc4 = dplyr::filter(dictionary, nchar(atc_code) == 5)
  )

  codes <- lapply(by_level, function(x) x$atc_code)
  non_leaf_codes <- c(codes$atc1, codes$atc2, codes$atc3)
  all_codes <- c(non_leaf_codes, codes$atc4)

  direct_children <- stats::setNames(vector("list", length(non_leaf_codes)), non_leaf_codes)
  for (code in codes$atc1) {
    direct_children[[code]] <- codes$atc2[startsWith(codes$atc2, code)]
  }
  for (code in codes$atc2) {
    direct_children[[code]] <- codes$atc3[startsWith(codes$atc3, code)]
  }
  for (code in codes$atc3) {
    direct_children[[code]] <- codes$atc4[startsWith(codes$atc4, code)]
  }

  descendants <- stats::setNames(vector("list", length(all_codes)), all_codes)
  for (code in all_codes) {
    descendants[[code]] <- c(
      codes$atc2[startsWith(codes$atc2, code)],
      codes$atc3[startsWith(codes$atc3, code)],
      codes$atc4[startsWith(codes$atc4, code)]
    )
  }

  list(
    dictionary = dictionary,
    by_level = by_level,
    codes = codes,
    non_leaf_codes = non_leaf_codes,
    all_codes = all_codes,
    all_ids = paste0("atc_", all_codes),
    direct_children = direct_children,
    descendants = descendants
  )
}

# Identifies the ATC level represented by a code's length
atc_code_to_level <- function(code) {
  code_length <- nchar(code)
  known <- c(`1` = "ATC1", `3` = "ATC2", `4` = "ATC3", `5` = "ATC4", `7` = "ATC5")
  if (as.character(code_length) %in% names(known)) {
    return(unname(known[[as.character(code_length)]]))
  }
  paste0("ATC", code_length)
}

# Looks up the description for an ATC code
atc_resolve_name <- function(code, dictionary) {
  if (is.null(code) || length(code) == 0 || is.na(code[[1]]) || !nzchar(code[[1]])) {
    return("")
  }
  index <- which(dictionary$atc_code == code[[1]])
  if (length(index) == 0 || is.na(dictionary$atc_name[index[[1]]])) {
    return("")
  }
  as.character(dictionary$atc_name[index[[1]]])
}

# Builds a sorted table of ATC codes, levels, and descriptions
atc_build_table <- function(codes, dictionary) {
  values <- unique(trimws(as.character(codes)))
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) == 0) {
    return(tibble::tibble(
      level = character(0),
      code = character(0),
      description = character(0)
    ))
  }
  result <- tibble::tibble(
    level = vapply(values, atc_code_to_level, character(1), USE.NAMES = FALSE),
    code = values,
    description = vapply(
      values,
      atc_resolve_name,
      character(1),
      dictionary = dictionary,
      USE.NAMES = FALSE
    )
  )
  result[order(result$code, method = "radix"), , drop = FALSE]
}

# Adds all ATC1-4 descendants to a set of selected codes
atc_expand_selection <- function(codes, hierarchy) {
  values <- unique(trimws(as.character(codes)))
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) == 0) {
    return(character(0))
  }
  descendants <- unlist(hierarchy$descendants[values], use.names = FALSE)
  sort(unique(c(values, descendants)), method = "radix")
}

# Finds all ATC5 codes beneath the selected ATC prefixes
atc_get_atc5_codes <- function(codes, dictionary) {
  values <- unique(trimws(as.character(codes)))
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) == 0) {
    return(character(0))
  }
  atc5 <- dictionary$atc_code[nchar(dictionary$atc_code) == 7]
  matches <- unlist(lapply(values, function(code) atc5[startsWith(atc5, code)]), use.names = FALSE)
  sort(unique(matches), method = "radix")
}

# Builds the final ATC output table with optional ATC5 rows
atc_build_final_table <- function(selected_codes, dictionary, include_atc5 = FALSE, atc5_codes = character(0)) {
  base_table <- atc_build_table(selected_codes, dictionary)
  if (!isTRUE(include_atc5)) {
    return(base_table)
  }
  if (length(atc5_codes) == 0) {
    atc5_codes <- atc_get_atc5_codes(selected_codes, dictionary)
  }
  if (length(atc5_codes) == 0) {
    return(base_table)
  }
  dplyr::bind_rows(base_table, atc_build_table(atc5_codes, dictionary)) %>%
    dplyr::distinct(level, code, description, .keep_all = TRUE) %>%
    dplyr::arrange(code)
}
