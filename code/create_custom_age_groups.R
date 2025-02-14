#!/usr/bin/env Rscript
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = Sys.getenv("omop_db_dbms"),
  server = Sys.getenv("omop_db_server"),
  port = Sys.getenv("omop_db_port"),
  user = Sys.getenv("omop_db_user"),
  password = Sys.getenv("omop_db_password"),
  pathToDriver = Sys.getenv("omop_db_driver")
)
sql <- "
WITH table_with_ages AS (
    SELECT subject_id AS row_id,
        @covariate_id AS covariate_id,
        CASE 
            WHEN (EXTRACT(YEAR FROM c.cohort_start_date) - p.year_of_birth) >= @age_lower_limit AND
              (EXTRACT(YEAR FROM c.cohort_start_date) - p.year_of_birth) < @age_upper_limit
            THEN 1 
            ELSE 0 
        END AS covariate_value
    FROM results.cohort c
    JOIN omop.person p ON p.person_id = c.subject_id
    WHERE c.cohort_definition_id = @cohort_id
)
SELECT row_id, covariate_id, covariate_value
FROM table_with_ages
WHERE covariate_value = 1
ORDER BY row_id;
"

getDbCustomAgeGroups <- function(
    connection,
    sql,
    age_lower_limit,
    age_upper_limit,
    covariate_id,
    cohort_id,
    covariate_name,
    analysis_id,
    analysis_name
) {
  
  sql_rendered <- SqlRender::render(
    sql,
    age_lower_limit = age_lower_limit,
    age_upper_limit = age_upper_limit,
    covariate_id = covariate_id,
    cohort_id = cohort_id
  )
  
  sql_rendered <- SqlRender::translate(
    sql_rendered,
    targetDialect = attr(connection, "dbms")
  )
  
  covariates <- DatabaseConnector::querySql(
    connection = connection,
    sql = sql_rendered,
    snakeCaseToCamelCase = TRUE
  )
  
  covariateRef <- data.frame(
    covariateId = covariate_id,
    covariateName = covariate_name,
    analysisId = analysis_id,
    conceptId = 0
  )
  
  analysisRef <- data.frame(
    analysisId = analysis_id,
    analysisName = analysis_name,
    domainId = "Demographics",
    startDay = NA_integer_,
    endDay = NA_integer_,
    isBinary = "Y",
    missingMeansZero = NA_character_,
    missingIsZero = NA_character_
  )
  
  metaData <- list(sql = sql_rendered, call = match.call())
  result <- Andromeda::andromeda(
    covariates = covariates,
    analysisRef = analysisRef,
    covariateRef = covariateRef
    
  )
  attr(result, "metaData") <- metaData
  # class(result) <- "CovariateData"
  return(result)
}

age_limits <- c(55, 80, 1000)
analysis_labels = list(
  age_below_55 = "Age < 55",
  age_between_55_80 = "55 <= Age < 80",
  age_above_80 = "Age >= 80"
)

covariate_names <- c(
  "Age group 0 - 55",
  "Age group 55 - 80",
  "Age group 80 - ..."
)

age_limit_prev <- 0
covariate_id_prev <- 0
analysis_id <- 1000
connection <- DatabaseConnector::connect(connectionDetails)

for (i in seq_along(age_limits)) {
  
  res <- getDbCustomAgeGroups(
    connection = connection,
    sql = sql,
    age_lower_limit = age_limit_prev,
    age_upper_limit = age_limits[i],
    covariate_id = as.numeric(paste0(covariate_id_prev + 1, analysis_id)),
    covariate_name = covariate_names[covariate_id_prev + 1],
    analysis_id = analysis_id,
    cohort_id = 11,
    analysis_name = "AgeGroups"
  )
  covariate_id_prev <- covariate_id_prev + 1
  age_limit_prev <- age_limits[i]
  message(
    paste(
      "Generated custom age covariates for group:",
      analysis_labels[i]
    )
  )
  Andromeda::saveAndromeda(
    andromeda = res,
    fileName = file.path(
      "data",
      paste("custom_covariateData", "ageGroups", i, sep = "_")
    )
  )
  Andromeda::close(res)
  res  <- NULL
  message(
    paste(
      "Saved custom age covariates for group",
      analysis_labels[i]
    )
  )
}

DatabaseConnector::disconnect(connection)
message("Disconnected from database")
