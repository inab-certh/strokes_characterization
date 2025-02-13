analysis_ref <- readr::read_csv("data/analysis_ref.csv")

multiple_concepts <- analysis_ref |> 
  dplyr::filter(multipleConceptCheck) |> 
  dplyr::pull(analysisId)


form_table <- function(data, is_subgroup = FALSE, is_measurement_range = FALSE) {
  formattable_options <- list()
  if (!is_subgroup) {
    if (!is_measurement_range) {
      result <- formattable::formattable(
        data |> 
          dplyr::mutate(result = formattable::percent(result)),
        list(result = formattable::color_bar(fun = "identity", color = "lightblue"))
      )
    } else {
      result <- formattable::formattable(
        data |> 
          dplyr::mutate(
            measurement_below_normal_range = formattable::percent(measurement_below_normal_range),
            measurement_above_normal_range = formattable::percent(measurement_above_normal_range),
            measurement_within_normal_range = formattable::percent(measurement_within_normal_range)
          ),
        list(
          measurement_below_normal_range = formattable::color_bar(fun = "identity", color = "lightblue"),
          measurement_above_normal_range = formattable::color_bar(fun = "identity", color = "lightblue"),
          measurement_within_normal_range = formattable::color_bar(fun = "identity", color = "lightblue")
        )
      )
    }
  } else {
    for (i in seq_along(unique(data$subgroup_value))) {
      formattable_options[[i]] <- formattable::color_bar(fun = "identity", color = "lightblue")
    }
    names(formattable_options) <- unique(data$subgroup_value)
    if (!is_measurement_range) {
      result <- formattable::formattable(
        data |> 
          dplyr::mutate(result = formattable::percent(result)) |> 
          tidyr::pivot_wider(names_from = subgroup_value, values_from = result),
        formattable_options
      )
    } else {
      result <- formattable::formattable(
        data |> 
          dplyr::mutate(
            measurement_below_normal_range = formattable::percent(measurement_below_normal_range),
            measurement_above_normal_range = formattable::percent(measurement_above_normal_range),
            measurement_within_normal_range = formattable::percent(measurement_within_normal_range)
          ) |> 
          dplyr::arrange(conceptId, subgroup_value) |> 
          dplyr::relocate(
            measurement_below_normal_range,
            .before = measurement_within_normal_range
          ) |>
          dplyr::relocate(
            measurement_within_normal_range,
            .after = measurement_above_normal_range
          ),
        list(
          measurement_below_normal_range = formattable::color_bar(fun = "identity", color = "lightblue"),
          measurement_above_normal_range = formattable::color_bar(fun = "identity", color = "lightblue"),
          measurement_within_normal_range = formattable::color_bar(fun = "identity", color = "lightblue")
        )
      )
      
    }
  }
  return(result)
}

