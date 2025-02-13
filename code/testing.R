source("code/helper.R")
covariateData <- FeatureExtraction::loadCovariateData("covariateData")

summary(covariateData)

covariates <- covariateData$covariates |> dplyr::collect()
covariateRef <- covariateData$covariateRef |> dplyr::collect()
analysisRef <- covariateData$analysisRef |> dplyr::collect()

FeatureExtraction::summary(covariateData)

aggregate <- FeatureExtraction::aggregateCovariates(covariateData)

pp <- aggregate$covariates |> dplyr::collect()
pp |> dplyr::arrange(desc(sumValue))

pp |> 
  dplyr::left_join(covariateRef, by = "covariateId") |> 
  dplyr::mutate(
    covariateName = stringr::str_remove(covariateName,pattern = ".*: ")
  ) |> 
  dplyr::filter(analysisId %in% c(1, 2, 101, 301)) |> 
  dplyr::arrange(analysisId, desc(sumValue)) |> 
  dplyr::group_by(analysisId) |> 
  dplyr::arrange(dplyr::desc(averageValue), .by_group = TRUE)

extended_covariates <- covariates |> 
  dplyr::left_join(covariateRef, by = "covariateId") |> 
  dplyr::arrange(rowId, analysisId, covariateId) |> 
  dplyr::mutate(
    covariateName = stringr::str_remove(covariateName, pattern = ".*: ")
  ) |> 
  dplyr::left_join(
    analysisRef |> dplyr::select(analysisId, analysisName),
    by = "analysisId"
  )

results_any_time_prior <- extended_covariates |> 
  dplyr::filter(analysisId %in% c(1, 2, 101, 301))

# Subgroup analyses
male_patient_ids <- extended_covariates |> 
  dplyr::filter(conceptId == 8507) |> 
  dplyr::pull(rowId) |> 
  unique()
female_patient_ids <- extended_covariates |> 
  dplyr::filter(conceptId == 8532) |> 
  dplyr::pull(rowId) |> 
  unique()
unknown_gender_patient_ids <- extended_covariates |> 
  dplyr::filter(conceptId == 4214687) |> 
  dplyr::pull(rowId) |> 
  unique()

subgroup_settings <- list(
  female = female_patient_ids,
  male = male_patient_ids
)

run_in_subgroups(
  data = results_any_time_prior,
  subgroup_settings = subgroup_settings,
  target = 0,
  fun = mean
)

results_any_time_prior |> 
  dplyr::filter(analysisId == 101) |> 
  dplyr::group_by(covariateName) |> 
  dplyr::summarise(perc = dplyr::n() / 320 * 100) |> 
  dplyr::arrange(dplyr::desc(perc))

results_any_time_prior |> 
  dplyr::filter(analysisId == 301) |> 
  dplyr::group_by(covariateName) |> 
  dplyr::summarise(perc = dplyr::n() / 320 * 100) |> 
  dplyr::arrange(dplyr::desc(perc))

run_in_analysis(
  data = extended_covariates,
  analysis_id = 1,
  fun = function(x) sum(x) / 320 * 100
)


analysis_ids <- c(
  gender = 1,
  conditions = 101, 
  drugs = 301,
  procedures = 501
)

analysis_ids |> 
  purrr::map(
    .f = run_in_analysis,
    data = extended_covariates, 
    fun = function(x) sum(x) / 320 * 100
  )

mapply(
  run_in_analysis,
  analysis_ids,
  data = extended_covariates, 
  fun = function(x) sum(x) / 320 * 100
)

results_any_time_prior |> dplyr::filter(covariateName %in% c(
  "rosuvastatin",
  "simvastatin",
  "atorvastatin",
  "fluvastatin",
  "pravastatin",
  "lovastatin",
  "cerivastatin"
))

analysis_ids <- c(conditions = 101, drugs = 301, procedures = 501)

pp <- lapply(
  analysis_ids, 
  run_in_analysis, 
  data = extended_covariates, 
  fun = function(x) sum(x) / 320 * 100
)

lapply(pp, dplyr::rename, "percent" = "result")

run_subgroup_in_analysis(
  data = extended_covariates,
  subgroup_settings = subgroup_settings,
  target = 104,
  fun = function(data, n) sum(data$covariateValue) / n * 100,
  result_label = "percent"
)








results_any_time_prior |> 
  dplyr::filter(analysisId == 2) -> data_with_age

data_with_age |> 
  dplyr::filter(rowId %in% subgroup_settings$female) |> 
  ggplot2::ggplot(
    ggplot2::aes(x = covariateValue)
  ) +
  ggplot2::geom_density()


Cairo::CairoPNG(filename = "test.png", width = 800, height = 600)
results_any_time_prior |> 
  dplyr::filter(analysisId == 2) |> 
  dplyr::mutate(
    sex = ifelse(rowId %in% subgroup_settings$female, "Female", "Male")
  ) |> 
  ggplot2::ggplot(
    ggplot2::aes(x = covariateValue, fill = sex)
  ) +
  ggplot2::geom_density(alpha = .4) +
  ggplot2::xlim(0, 120) +
  ggplot2::theme_bw()
dev.off()


subgroup_row_ids <- female_patient_ids
extended_covariates |> 
  dplyr::filter(
    rowId %in% subgroup_row_ids,
    analysisId == 304
  ) |> 
  dplyr::group_by(covariateId) |> 
  dplyr::summarise(
    percentage_present = sum(covariateValue) / length(subgroup_row_ids)*100
  ) |> 
  dplyr::arrange(desc(percentage_present)) |> 
  dplyr::left_join(covariateRef, by = "covariateId") |> 
  dplyr::select(covariateId, percentage_present, covariateName) |> 
  dplyr::mutate(
    covariateName = stringr::str_remove(covariateName,pattern = ".*: ")
  )
  
  
  
statinIds <- c(
  1545958, 21601860, 1592180,
  21601861, 1549686, 21601859,
  1592085, 21601857, 40165636,
  21601863, 1551860, 21601858,
  1510813, 21601862,1539403,
  21601856
)

statins <- covariates |> 
  dplyr::left_join(covariateRef, by = "covariateId") |> 
  dplyr::filter(conceptId %in% statinIds, analysisId == 301)


# Statin users overall
statins |> 
  dplyr::group_by(covariateName) |> 
  dplyr::summarise(n = n()) |> 
  dplyr::mutate(
    covariateName = stringr::str_remove(covariateName,pattern = ".*: ")
  ) |> 
  dplyr::arrange(desc(n))


multipleStatinsPatients <- statins |>
  dplyr::group_by(rowId) |>
  dplyr::summarise(n = n()) |> 
  # dplyr::arrange(desc(n)) |> 
  dplyr::filter(n > 1) |> 
  dplyr::arrange(rowId) |> 
  dplyr::pull(rowId)

statins |> 
  dplyr::filter(rowId %in% multipleStatinsPatients) |> 
  dplyr::select(rowId, covariateName, covariateValue) |> 
  dplyr::mutate(
    covariateName = stringr::str_remove(covariateName,pattern = ".*: ")
  ) |> 
  dplyr::arrange(rowId) |> 
  tidyr::pivot_wider(
    names_from = covariateName,
    values_from = covariateValue
  ) |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    sum = sum(
      simvastatin,
      atorvastatin,
      rosuvastatin,
      lovastatin,
      pravastatin,
      na.rm = TRUE
    )
  ) |> 
  dplyr::arrange(desc(sum))
  



covariateDataStatins <- FeatureExtraction::loadCovariateData("covariateDataStatins")

summary(covariateDataStatins)

covariates <- covariateDataStatins$covariates |> dplyr::collect()
covariateRef <- covariateDataStatins$covariateRef |> dplyr::collect()
analysisRef <- covariateDataStatins$analysisRef |> dplyr::collect()

FeatureExtraction::summary(covariateData)

aggregate <- FeatureExtraction::aggregateCovariates(covariateData)

pp <- aggregate$covariates |> dplyr::collect()
pp |> dplyr::arrange(desc(sumValue))

covariates |> 
  dplyr::left_join(covariateRef, by = "covariateId") |> 
  dplyr::glimpse()



