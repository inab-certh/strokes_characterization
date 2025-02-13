run_in_subgroups <- function(data, subgroup_settings, target, fun, ...) {
  
  result <- list()
  
  for (i in seq_along(subgroup_settings)) {
    tmp <- data |> 
      dplyr::filter(rowId %in% subgroup_settings[[i]]) |> 
      dplyr::filter(conceptId == target) |> 
      dplyr::pull(covariateValue)
    
    result[i] <-fun(tmp, ...)
  }
  
  names(result) <- names(subgroup_settings)
  
  return(result)
}


run_in_analysis <- function(
    data, analysis_name, analysis_id, fun, file = NULL, analysisRef,
    n_total = NULL, ...
) {
  
  .f1 <- function(data) {
    n_total <- nrow(data)
    data |> 
      dplyr::group_by(covariateName) |> 
      dplyr::summarise(result = dplyr::n() / n_total * 100, .groups = "drop")
  }
  
  multiple_concept <- analysisRef |> 
    dplyr::filter(analysisId == analysis_id) |> 
    dplyr::pull(multipleConceptCheck)
  
  if (!multiple_concept & !is.null(n_total)) {
    
    result_to_return <- data |> 
      dplyr::filter(analysisId == analysis_id) |> 
      dplyr::group_by(conceptId, covariateName) |> 
      tidyr::nest() |> 
      dplyr::mutate(
        result = unlist(purrr::map(.x = data, .f = fun)),
        n = purrr::map_dbl(.x = data, .f = nrow)
      ) |> 
      dplyr::select(-data) |> 
      dplyr::arrange(dplyr::desc(result)) |> 
      dplyr::relocate(conceptId) |> 
      dplyr::relocate(n, .after = conceptId) |> 
      dplyr::ungroup()
    
  } else if (multiple_concept) {
    
    result_to_return <- data |> 
      dplyr::filter(analysisId == analysis_id) |> 
      dplyr::group_by(conceptId) |> 
      tidyr::nest() |> 
      dplyr::mutate(
        result = purrr::map(.x = data, .f = .f1),
        n = purrr::map_dbl(.x = data, .f = nrow)
      ) |> 
      dplyr::select(-data) |> 
      tidyr::unnest(result) |> 
      dplyr::arrange(dplyr::desc(n), conceptId, result) |> 
      dplyr::ungroup() |> 
      dplyr::mutate(
        names = stringr::str_extract(covariateName, pattern = "^[^:]+(?= :)"),
        variable  = stringr::str_extract(covariateName, pattern ="(?<=: ).*")
      ) |>
      dplyr::select(-covariateName) |>
      dplyr::mutate(names = stringr::str_replace_all(names, " ", "_")) |>
      tidyr::pivot_wider(names_from = names, values_from = result)
    
  }
  
  if (!is.null(file)) {
    save_location <- file.path(
      file,
      paste0(analysis_name, "_", paste("overall_analysis_id", analysis_id, sep = "_"), ".csv")
    )
    readr::write_csv(
      x = result_to_return,
      file = save_location
    )
    message(paste("Wrote result in", save_location))
  }
  
  return(result_to_return)
}

run_subgroup_in_analysis <- function(
    data, subgroup_settings, analysis_name,
    subgroup_label, analysis_id, result_label = "result",
    file = NULL, fun, ...) {
  
  .f1 <- function(data) {
    n_total <- nrow(data)
    data |> 
      dplyr::group_by(covariateName) |> 
      dplyr::summarise(result = dplyr::n() / n_total * 100, .groups = "drop")
  }
  
  result_to_return <- list()
  subgroup_names <- names(subgroup_settings)
  multiple_concept <- analysisRef |> 
    dplyr::filter(analysisId == analysis_id) |> 
    dplyr::pull(multipleConceptCheck)
  for (i in seq_along(subgroup_settings)) {
    
    if (!multiple_concept) {
      tmp <- data |> 
        dplyr::filter(rowId %in% subgroup_settings[[i]]) |> 
        dplyr::filter(analysisId == analysis_id) |> 
        dplyr::group_by(covariateName) |> 
        tidyr::nest() |> 
        dplyr::mutate(
          result = sapply(data, fun, n = length(subgroup_settings[[i]]))
        ) |> 
        dplyr::select(-data) |> 
        dplyr::ungroup() |> 
        dplyr::arrange(dplyr::desc(result)) |> 
        dplyr::rename("{result_label}" := "result") |> 
        dplyr::mutate(subgroup_value = subgroup_names[i])
    } else {
      tmp <- data |> 
        dplyr::filter(rowId %in% subgroup_settings[[i]]) |> 
        dplyr::filter(analysisId == analysis_id) |> 
        dplyr::group_by(conceptId) |> 
        tidyr::nest() |> 
        dplyr::mutate(result = purrr::map(.x = data, .f = .f1)) |> 
        dplyr::select(-data) |> 
        tidyr::unnest(result) |> 
        dplyr::arrange(conceptId, result) |> 
        dplyr::ungroup() |> 
        dplyr::mutate(
          names = stringr::str_extract(covariateName, pattern = "^[^:]+(?= :)"),
          variable  = stringr::str_extract(covariateName, pattern ="(?<=: ).*")
        ) |>
        dplyr::select(-covariateName) |>
        dplyr::mutate(names = stringr::str_replace_all(names, " ", "_")) |>
        tidyr::pivot_wider(names_from = names, values_from = result) |> 
        dplyr::mutate(subgroup_value = subgroup_names[i]) |> 
        dplyr::relocate(subgroup_value, .after = conceptId)
    }
    
    result_to_return[[i]] <- tmp
    
  }
  
  names(result_to_return) <- subgroup_names
  
  
  if (!is.null(file)) {
    save_location <- file.path(
      file,
      paste0(
        analysis_name, "_",
        paste("subgroup", subgroup_label,
              "analysis_id", analysis_id, sep = "_"),
        ".csv"
      )
    )
    readr::write_csv(
      x = dplyr::bind_rows(result_to_return),
      file = save_location
    )
    message(paste("Wrote result in", save_location))
  }
  
  return(result_to_return)
}

is_multiple_concept <- function(data, analysis_id) {
  data |> 
    dplyr::filter(analysisId == analysis_id) |> 
    dplyr::group_by(rowId, conceptId) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop_last") |> 
    dplyr::arrange(dplyr::desc(n)) |> 
    dplyr::ungroup(rowId) |> 
    dplyr::slice_head(n = 1) |> 
    dplyr::pull(n) > 1
}
