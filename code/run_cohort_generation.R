#!/usr/bin/env Rscript

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = Sys.getenv("omop_db_dbms"),
  server = Sys.getenv("omop_db_server"),
  port = Sys.getenv("omop_db_port"),
  user = Sys.getenv("omop_db_user"),
  password = Sys.getenv("omop_db_password"),
  pathToDriver = Sys.getenv("omop_db_driver")
)

conn <- DatabaseConnector::connect(connectionDetails)

cdmDatabaseSchema <- "omop"
resultsDatabaseSchema <- "results"
databaseId <- "papag"
databaseName <- "Papageorgiou General Hospital"
databaseDescription <- "Papageorgiou General Hospital"

cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = Sys.getenv("atlas_web_api"),
  cohortIds = 11
)

readr::write_csv(cohortDefinitionSet, "data/cohort_definition.csv")


CohortGenerator::createCohortTables(
  connection = conn,
  cohortDatabaseSchema = "results"
)

# Generate the cohort set
cohortsGenerated <- CohortGenerator::generateCohortSet(
  # connectionDetails = connectionDetails,
  connection = conn,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = resultsDatabaseSchema,
  cohortDefinitionSet = cohortDefinitionSet
)

CohortGenerator::dropCohortStatsTables(
  # connectionDetails = connectionDetails,
  connection = conn,
  cohortDatabaseSchema = "results"
)

pp <- DatabaseConnector::querySql(
  connection = conn,
  sql = "SELECT COUNT(DISTINCT SUBJECT_ID) FROM results.cohort WHERE COHORT_DEFINITION_ID = 11"
)

message(
  paste(
    "A cohort with",
    pp$COUNT,
    "patients was created"
  )
)

DatabaseConnector::disconnect(conn)
