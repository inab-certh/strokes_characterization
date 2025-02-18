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
  
  if (is.na(multiple_concept)) {
    result_to_return <- data |> 
      dplyr::filter(analysisId == analysis_id) |> 
      dplyr::group_by(conceptId, covariateName) |> 
      tidyr::nest() |>
      dplyr::mutate(
        result = purrr::map(
          .x = data,
          .f = \(data) {
            res <- quantile(
              data$covariateValue,
              probs = c(.01, .05, .1, .25, .5, .75, .9, .95, .99),
              na.rm = TRUE
            )
            data.frame(
              q_01 = res[1],
              q_05 = res[2],
              q_10 = res[3],
              q_25 = res[4],
              q_50 = res[5],
              q_75 = res[6],
              q_90 = res[7],
              q_95 = res[8],
              q_99 = res[9]
            )
          }
        )
      ) |> 
      dplyr::select(-data) |> 
      tidyr::unnest(cols = c(result)) |> 
      dplyr::ungroup() |> 
      dplyr::mutate(analysis = "overall") |> 
      dplyr::relocate(analysis)
    
  } else {
    
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
    message(paste("\t - Wrote result in", save_location))
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
    if (is.na(multiple_concept)) {
      tmp <- data |> 
        dplyr::filter(rowId %in% subgroup_settings[[i]]) |> 
        dplyr::filter(analysisId == analysis_id) |> 
        dplyr::group_by(conceptId, covariateName) |> 
        tidyr::nest() |>
        dplyr::mutate(
          result = purrr::map(
            .x = data,
            .f = \(data) {
              res <- quantile(
                data$covariateValue,
                probs = c(.01, .05, .1, .25, .5, .75, .9, .95, .99),
                na.rm = TRUE
              )
              data.frame(
                q_01 = res[1],
                q_05 = res[2],
                q_10 = res[3],
                q_25 = res[4],
                q_50 = res[5],
                q_75 = res[6],
                q_90 = res[7],
                q_95 = res[8],
                q_99 = res[9]
              )
            }
          )
        ) |> 
        dplyr::select(-data) |> 
        tidyr::unnest(cols = c(result)) |> 
        dplyr::ungroup() |> 
        dplyr::mutate(subgroup_value = subgroup_names[i]) |> 
        dplyr::relocate(subgroup_value)
    } else {
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
    }
    
    result_to_return[[i]] <- tmp
    
  }
  
  names(result_to_return) <- subgroup_names
  
  
  if (!is.null(file)) {
    save_location <- file.path(
      file,
      paste0(
        analysis_name, "_",
        paste("subgroup_analysis", subgroup_label,
              "analysis_id", analysis_id, sep = "_"),
        ".csv"
      )
    )
    readr::write_csv(
      x = dplyr::bind_rows(result_to_return),
      file = save_location
    )
    message(paste("\t - Wrote result in", save_location))
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

combine_custom_covariates <- function(location) {
  covariateData_files <- list.files(location, pattern = "^custom", full.names = TRUE)
  
  covariateData_list <- lapply(
    covariateData_files,
    FeatureExtraction::loadCovariateData
  )
  
  df_names <- names(covariateData_list[[1]])
  
  merged_data <- lapply(df_names, function(df_name) {
    dplyr::bind_rows(
      lapply(
        covariateData_list, 
        \(covariateData, df_name) covariateData[[df_name]] |> dplyr::collect(),
        df_name
      )
    )
  })
  names(merged_data) <- df_names
  
  Andromeda::andromeda(
    covariates = merged_data$covariates,
    covariateRef = merged_data$covariateRef,
    analysisRef = merged_data$analysisRef |> dplyr::distinct()
  )
}

include_custom_covariates_to_analysis <- function(
    target_covariateData,
    custom_covariateData
) {
  overall_analysisRef <- target_covariateData$analysisRef |> 
    dplyr::collect() |> 
    dplyr::bind_rows(custom_covariateData$analysisRef |> dplyr::collect())
  
  overall_covariateRef <- target_covariateData$covariateRef |> 
    dplyr::collect() |> 
    dplyr::bind_rows(custom_covariateData$covariateRef |> dplyr::collect())
  
  overall_covariates <- target_covariateData$covariates |> 
    dplyr::collect() |> 
    dplyr::bind_rows(custom_covariateData$covariates |> dplyr::collect())
  
  Andromeda::andromeda(
    covariates = overall_covariates,
    analysisRef = overall_analysisRef,
    covariateRef = overall_covariateRef
  )
}

