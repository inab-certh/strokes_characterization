#!/usr/bin/env Rscript
source("code/helper.R")
args <- commandArgs(trailingOnly = TRUE)

message(
  crayon::bold("Running characterization for analysis: "),
  crayon::bold(crayon::italic(args[1]))
)
covariateData <- FeatureExtraction::loadCovariateData(
  file = file.path(
    "data",
    paste("covariateData", args[1], sep = "_")
  )
)

custom_covariate_files <- list.files(
  path = "data",
  pattern = "^custom",
  full.names = TRUE
)

if (length(custom_covariate_files > 0)) {
  message("Found custom covariates")
  message("Merging with standard covariate data...")
  custom_covariateData <- combine_custom_covariates("data")
  combined_covariateData <- include_custom_covariates_to_analysis(
    target_covariateData = covariateData,
    custom_covariateData = custom_covariateData
  )
  Andromeda::close(covariateData)
  covariateData <- NULL
  Andromeda::close(custom_covariateData)
  custom_covariateData <- NULL
  covariateData <- combined_covariateData
}

message("Loading covariate data...")

covariates <- covariateData$covariates |> dplyr::collect()
covariateRef <- covariateData$covariateRef |> dplyr::collect()
analysisRef <- covariateData$analysisRef |> dplyr::collect()

# FeatureExtraction::summary(covariateData)

cohort_definition <- readr::read_csv(
  "data/cohort_definition.csv",
  col_types = readr::cols()
)

message("Merging covariate data...")
extended_covariates <- covariates |> 
  dplyr::left_join(covariateRef, by = "covariateId") |> 
  dplyr::arrange(rowId, analysisId, covariateId) |> 
  # dplyr::mutate(
  #   covariateName = stringr::str_remove(covariateName, pattern = ".*: ")
  # ) |> 
  dplyr::mutate(
    covariateName = stringr::str_replace(
      covariateName, pattern = "(during).*?:", ":"
    )
  ) |> 
  dplyr::left_join(
    analysisRef |> dplyr::select(analysisId, analysisName),
    by = "analysisId"
  )



# ------------------------------------------------------------------------------
# Overall analyses
# ------------------------------------------------------------------------------

message("Running overall analyses...")
analysis_ids <- analysisRef |> 
  dplyr::filter(isBinary == "Y") |> 
  dplyr::pull(analysisId)

multiple_concept_check <- analysis_ids |> 
  purrr::map_lgl(
    .f = is_multiple_concept,
    data = extended_covariates
  )

analysisRef <- dplyr::tibble(
  analysisId = analysis_ids,
  multipleConceptCheck = multiple_concept_check
) |> 
  dplyr::left_join(analysisRef, by = "analysisId")

save_directory <- file.path(
  "results",
  paste("results", args[1], sep = "_")
)

if (!dir.exists(save_directory)) {
  dir.create(path = save_directory, recursive = TRUE)
  message("Created directory: ", crayon::italic(save_directory))
}

n_total <- length(unique(extended_covariates$rowId))

overall_analysis <- analysis_ids |> 
  purrr::map(
    .f = run_in_analysis,
    data = extended_covariates, 
    fun = function(df, n = n_total) length(unique(df$rowId)) / n * 100,
    file = save_directory,
    analysis_name = args[1],
    analysisRef = analysisRef,
    n_total = n_total
  )
overview <- data.frame(
  cohort_id = cohort_definition$cohortId,
  cohort_name = cohort_definition$cohortName,
  cohort_description = cohort_definition$logicDescription,
  n_records = n_total
)
readr::write_csv(
  overview,
  file.path(
    save_directory,
    paste0(paste(args[1], "overall", "overview", sep = "_"), ".csv")
  )
)
message("Finished running overall analyses")

# ------------------------------------------------------------------------------
# Subgroup analyses
# ------------------------------------------------------------------------------

message("Running subgroup analyses...")

ischemic_stroke_concept_ids <- readr::read_csv(
  "extras/subgroups/ischemic_stroke.csv",
  col_types = readr::cols()
) |> 
  dplyr::pull(Id)
transient_ischemic_attack_concept_ids <- readr::read_csv(
  "extras/subgroups/transient_ischemic_attack.csv",
  col_types = readr::cols()
) |> 
  dplyr::pull(Id)
hemorrhagic_stroke_concept_ids <- readr::read_csv(
  "extras/subgroups/hemorrhagic_stroke.csv",
  col_types = readr::cols()
) |> 
  dplyr::pull(Id)

ischemic_stroke_ids <- extended_covariates |>
  dplyr::filter(conceptId %in% ischemic_stroke_concept_ids) |>
  dplyr::pull(rowId) |>
  unique()
transient_ischemic_attack_ids <- extended_covariates |>
  dplyr::filter(conceptId %in% transient_ischemic_attack_concept_ids) |>
  dplyr::pull(rowId) |>
  unique()
hemorrhagic_stroke_ids <- extended_covariates |>
  dplyr::filter(conceptId %in% hemorrhagic_stroke_concept_ids) |>
  dplyr::pull(rowId) |>
  unique()
male_patient_ids <- extended_covariates |>
  dplyr::filter(conceptId == 8507) |>
  dplyr::pull(rowId) |>
  unique()
female_patient_ids <- extended_covariates |>
  dplyr::filter(conceptId == 8532) |>
  dplyr::pull(rowId) |>
  unique()

stroke_type_subgroup_settings <- list(
  ischemic_stroke = ischemic_stroke_ids,
  hemorrhagic_stroke = hemorrhagic_stroke_ids,
  transient_ischemic_attack = transient_ischemic_attack_ids
)

stroke_type_overview <- data.frame(
  subgroup = names(stroke_type_subgroup_settings),
  n = lapply(stroke_type_subgroup_settings, length) |> unlist()
)
rownames(stroke_type_overview) <- NULL

readr::write_csv(
  stroke_type_overview,
  file.path(
    save_directory,
    paste0(paste(args[1], "subgroup_analysis", "stroke_type", "overview", sep = "_"), ".csv")
  )
)

stroke_type_subgroup_analysis <- analysis_ids |>
  purrr::map(
    .f = run_subgroup_in_analysis,
    data = extended_covariates,
    subgroup_settings = stroke_type_subgroup_settings,
    result_label = "result",
    fun = function(df, n) length(unique(df$rowId)) / n * 100,
    file = save_directory,
    analysis_name = args[1],
    subgroup_label = "stroke_type"
  )

stroke_type_by_gender_subgroup_settings <- list(
  ischemic_stroke_male = base::intersect(
    ischemic_stroke_ids, male_patient_ids
  ),
  ischemic_stroke_female = base::intersect(
    ischemic_stroke_ids, female_patient_ids
  ),
  hemorrhagic_stroke_male = base::intersect(
    hemorrhagic_stroke_ids, male_patient_ids
  ),
  hemorragic_stroke_female = base::intersect(
    hemorrhagic_stroke_ids, female_patient_ids
  ),
  transient_ischemic_attack_male = base::intersect(
    transient_ischemic_attack_ids, male_patient_ids
  ),
  transient_ischemic_attack_female = base::intersect(
    transient_ischemic_attack_ids, female_patient_ids
  )
)
stroke_type_by_gender_subgroup_analysis <- analysis_ids |>
  purrr::map(
    .f = run_subgroup_in_analysis,
    data = extended_covariates,
    subgroup_settings = stroke_type_by_gender_subgroup_settings,
    result_label = "result",
    fun = function(df, n) length(unique(df$rowId)) / n * 100,
    file = save_directory,
    analysis_name = args[1],
    subgroup_label = "stroke_type_by_gender"
  )


stroke_type_by_gender_overview <- data.frame(
  subgroup = names(stroke_type_by_gender_subgroup_settings),
  n = lapply(stroke_type_by_gender_subgroup_settings, length) |> unlist()
)
rownames(stroke_type_by_gender_overview) <- NULL
readr::write_csv(
  stroke_type_by_gender_overview,
  file.path(
    save_directory,
    paste0(paste(args[1], "subgroup_analysis", "stroke_type_by_gender", "overview", sep = "_"), ".csv")
  )
)

message("Finished subgroup analyses")


message("Saving results...")
analysisRef |> 
  dplyr::mutate(
    analysisNameShiny = dplyr::case_when(
      stringr::str_detect(analysisName, "ConditionGroup") ~ "Condition groups",
      stringr::str_detect(analysisName, "DrugExposure") ~ "Drugs",
      stringr::str_detect(analysisName, "Gender") ~ "Gender",
      stringr::str_detect(analysisName, "AgeGroup") ~ "Age groups",
      stringr::str_detect(analysisName, "Age") ~ "Age",
      stringr::str_detect(analysisName, "DrugGroup") ~ "Drug groups",
      stringr::str_detect(analysisName, "ProcedureOccurrence") ~ "Procedures",
      stringr::str_detect(analysisName, "ConditionOccurrence") ~ "Conditions",
      stringr::str_detect(analysisName, "MeasurementRange") ~ "Measurement range",
      stringr::str_detect(analysisName, "MeasurementValue") ~ "Measurement value",
      stringr::str_detect(analysisName, "Measurement") ~ "Measurement"
    ),
    analysis = args[1]
  ) |> 
  dplyr::arrange(analysisId) |> 
  readr::write_csv(
    file = file.path(
      "results",
      paste("results", args[1], sep = "_"),
      paste0(paste(args[1], "analysis_ref", sep = "_"), ".csv")
    )
  )

message("Finished characterization")
