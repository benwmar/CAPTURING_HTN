# =============================================================================
# CAPTUR-HTN -- Hypertension Guidelines to Practice
#
# One script. Generates the cohorts with CohortGenerator, optionally runs
# CohortDiagnostics, computes every feature both tables need with
# FeatureExtraction, and writes the tables -- for the whole cohort and, if
# asked, split by prior heart failure.
#
#   Table 1  Characteristics of the hypertension cohorts and the base
#            ambulatory population
#   Table 2  Antihypertensive treatment patterns, by cohort
#
# With runSubgroupAnalysis = TRUE each table is written three times: all
# patients, those with a heart failure diagnosis on or before cohort start,
# and those without. Each file keeps the same three cohort columns, so the
# strata line up row for row.
#
# Everything you need to change is in SECTION 0. Nothing below it is
# site-specific.
#
# Run:  Rscript ExecuteAnalyses.R      (or source() it in RStudio)
#
# Layout this script expects, all relative to itself:
#   Cohorts.csv                        cohort_id, cohort_name
#   cohorts/<id>.json                  ATLAS definition
#   sql/sql_server/<id>.sql            ATLAS SQL
#   AntihypertensiveIngredients.csv    classId, ingredientConceptId, ingredientName
# =============================================================================


# =============================================================================
# SECTION 0 -- CONFIGURATION. Everything site-specific lives here.
# =============================================================================

# --- Database ----------------------------------------------------------------
# Credentials are read from the environment so they are never committed. Set
# them before running, e.g. in ~/.Renviron:

dbms                 <- "sql server"
server               <- Sys.getenv("CAPTUR_DB_SERVER")
user                 <- Sys.getenv("CAPTUR_DB_USER")
password             <- Sys.getenv("CAPTUR_DB_PASSWORD")
port                 <- Sys.getenv("CAPTUR_DB_PORT")
pathToDriver         <- Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")

# --- Schemas -----------------------------------------------------------------
cdmDatabaseSchema        <- ""                  # read-only: the CDM
vocabularyDatabaseSchema <- cdmDatabaseSchema   # usually the same as the CDM
cohortDatabaseSchema     <- ""                  # written to: the cohort table
cohortTable              <- "captur_htn_cohort"
tempEmulationSchema      <- "captur_scratch"    # "" if the platform has real temp tables

# --- Output ------------------------------------------------------------------
outputFolder <- file.path(getwd(), "CapturHtnOutput")
databaseId   <- "Site_OMOP"                     # short site identifier
minCellCount <- 5                              # counts below this are masked

# --- What to run this time ---------------------------------------------------
# Which cohorts to (re)generate. The rest are read from the cohort table as
# they already stand. integer(0) = generate nothing.
#
# Generating one cohort does not disturb the others: each ATLAS SQL file ends
# with a DELETE scoped to its own cohort_definition_id.
generateCohortIds <- integer(0)

# Cohort diagnostics are slow. Off by default so routine reruns of the tables
# do not pay for them; set TRUE, or set CAPTUR_RUN_DIAGNOSTICS=TRUE in the
# environment, when you actually want them.
runCohortDiagnostics <- FALSE
cohortDiagnosticsIds <- c(1348L, 1332L)   # the two hypertension cohorts

# Subgroup analysis: split every cohort by heart failure diagnosed on or
# before the cohort start date.
runSubgroupAnalysis <- TRUE
# Heart failure, with descendants. 316139 is "Heart failure"; add 319835
# ("Congestive heart failure") only if your mapping does not already place it
# under 316139, or it will be counted twice -- harmless here, since membership
# is a DISTINCT flag, but it makes the concept set harder to explain.
heartFailureConceptIds <- c(316139L)
# Permanent table holding the stratified cohorts, written beside the cohort
# table. FeatureExtraction needs a real cohort table to characterize, so the
# strata cannot live in a temp table.
strataTable <- paste0(cohortTable, "_strata")

# --- Study settings ----------------------------------------------------------
# Cohort ids, and the drug window each one uses. The window is the single most
# consequential setting in the study: it decides who counts as treated.
#   newly identified : index visit only          -> (0, 1)
#   overall          : index and prior 12 months -> (-365, 0)
#   base population  : same as overall, so the two treated shares compare
cohortSpecs <- data.frame(
  cohortId    = c(1322L, 1348L, 1332L),
  label       = c("Base ambulatory population",
                  "Newly identified HTN\nAt index",
                  "Overall HTN\nIndex and prior 12 mo"),
  windowStart = c(-365L, 0L, -365L),
  windowEnd   = c(   0L, 1L,    0L),
  stringsAsFactors = FALSE)

# Blood pressure measurement concepts, as used in the cohort definitions.
sbpConceptIds <- c(3004249L)   # Systolic blood pressure
dbpConceptIds <- c(3012888L)   # Diastolic blood pressure
sbpThreshold  <- 140
dbpThreshold  <- 90
bpValueRange  <- c(40, 300)    # readings outside this are dropped as implausible

# Demographic grouping for Table 1. FeatureExtraction returns 5-year age bands
# and one race covariate per race concept; both are collapsed here.
#
# ageBands: a patient falls in the first band whose upper bound they are under.
ageBands <- list(
  list(label = "18-44",       max = 45),
  list(label = "45-64",       max = 65),
  list(label = "65 or older", max = Inf))

# raceGroups: covariate names matching the pattern are summed into one row.
# Anything that matches no pattern keeps its own row, so a race concept that
# is new to the vocabulary is never silently dropped.
raceGroups <- list(
  "Asian" = paste0("asian|chinese|filipino|japanese|korean|vietnamese|",
                   "cambodian|hmong|laotian|thai|pakistani|bangladeshi|",
                   "indonesian|malaysian|sri lankan|burmese|nepalese|",
                   "taiwanese|okinawan|bhutanese|singaporean"),
  "Native Hawaiian or Other Pacific Islander" = paste0(
    "hawaiian|pacific islander|samoan|guamanian|chamorro|tongan|fijian|",
    "marshallese|chuukese|palauan|tahitian|melanesian|micronesian|",
    "polynesian|carolinian|kosraean|pohnpeian|saipanese|yapese|papua"))

# Drug-class accounting for Table 2.
#   FALSE: a record from a single-pill combination is counted only in the
#          single-pill rows. Class percentages then do NOT sum to 100. This
#          reproduces the published table.
#   TRUE:  each component is counted in its class.
# It no longer affects the regimen rows, which always count every agent.
countSpcComponentsInClasses <- FALSE

# Deduplication of drug records, and how wide it reaches.
#
#   "window"  a drug class counts at most once per patient for the WHOLE drug
#             window. For the newly identified cohort that window is the index
#             date and the day after, so metoprolol on both days is one beta
#             blocker record. For the two 12-month cohorts it means a class
#             counts once per patient per year -- twelve monthly refills of
#             metoprolol become one record, not twelve.
#   "day"     once per patient per class per calendar date.
#   "none"    every drug_exposure row counts, which is what produced the
#             earliest output.
#
# Different classes and different OTHER subclasses always count separately, as
# do standalone and combination-product records.
#
# "window" makes classes comparable ACROSS cohorts, because a 12-month window
# no longer mechanically produces more records than a 2-day one. It also makes
# "records" mean "patients on this class", so the Table 2 class percentages
# become a share of class-exposures rather than of prescriptions. Set "day" if
# you want repeat prescribing over the year to keep its weight.
dedupScope <- "window"
if (!dedupScope %in% c("window", "day", "none")) {
  stop("dedupScope must be one of: window, day, none")
}

# Drug records whose visit_occurrence has one of these visit_type_concept_ids
# are dropped before anything is counted. 32853 is the inpatient claim type:
# a claim generated during an admission is not outpatient prescribing, and
# counting it inflates the treated share and the class mix.
#
# A drug_exposure with no visit_occurrence_id is KEPT -- it did not come from
# an excluded visit, it came from no visit at all. Set to integer(0) to
# disable the exclusion.
excludeVisitTypeConceptIds <- c(32853L)


# =============================================================================
# SECTION 1 -- Setup
# =============================================================================

required <- c("DatabaseConnector", "SqlRender", "CohortGenerator",
              "FeatureExtraction", "dplyr", "readr")
missingPkgs <- required[!vapply(required, requireNamespace, logical(1),
                                quietly = TRUE)]
if (length(missingPkgs) > 0) {
  stop("Install these first: ", paste(missingPkgs, collapse = ", "),
       "\n  remotes::install_github(c('OHDSI/CohortGenerator', ",
       "'OHDSI/FeatureExtraction'))")
}

unsetVars <- c("CAPTUR_DB_SERVER", "CAPTUR_DB_USER", "CAPTUR_DB_PASSWORD")
unsetVars <- unsetVars[Sys.getenv(unsetVars) == ""]
if (length(unsetVars) > 0) {
  stop("Set these environment variables before running: ",
       paste(unsetVars, collapse = ", "))
}

# --- SQL dialect -------------------------------------------------------------
# Every statement in this script is written in OHDSI SQL and translated to
# `dbms` by SqlRender before it is sent. Nothing is executed as written.
targetDialect <- dbms
supportedDialects <- tryCatch({
  d <- SqlRender::listSupportedDialects()
  if (is.data.frame(d)) d[[1]] else as.character(d)
}, error = function(e) character(0))
if (length(supportedDialects) > 0 && !targetDialect %in% supportedDialects) {
  stop("SqlRender cannot translate to dbms = '", targetDialect, "'. ",
       "Supported: ", paste(supportedDialects, collapse = ", "))
}

if (nzchar(tempEmulationSchema)) {
  options(sqlRenderTempEmulationSchema = tempEmulationSchema)
}
tempSchema <- getOption("sqlRenderTempEmulationSchema")

if (targetDialect %in% c("oracle", "bigquery", "spark", "snowflake") &&
    (is.null(tempSchema) || !nzchar(tempSchema))) {
  stop("dbms = '", targetDialect, "' has no usable temp tables, so ",
       "tempEmulationSchema must be set in SECTION 0.")
}

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = dbms, server = server, user = user, password = password,
  port = if (nzchar(port)) port else NULL,
  pathToDriver = if (nzchar(pathToDriver)) pathToDriver else NULL)

dir.create(outputFolder, showWarnings = FALSE, recursive = TRUE)
logFile <- file.path(outputFolder, "log.txt")
if (requireNamespace("ParallelLogger", quietly = TRUE)) {
  ParallelLogger::addDefaultFileLogger(logFile)
}
message("Output folder: ", outputFolder)


# =============================================================================
# SECTION 2 -- Generate the cohorts
# =============================================================================

cohortsCsv <- readr::read_csv("Cohorts.csv", show_col_types = FALSE)
names(cohortsCsv) <- tolower(names(cohortsCsv))

cohortDefinitionSet <- do.call(rbind, lapply(cohortSpecs$cohortId, function(id) {
  jsonPath <- file.path("cohorts", paste0(id, ".json"))
  sqlPath  <- file.path("sql", "sql_server", paste0(id, ".sql"))
  if (!file.exists(jsonPath)) stop("Missing cohort definition: ", jsonPath)
  if (!file.exists(sqlPath))  stop("Missing cohort SQL: ", sqlPath)
  nameRow <- cohortsCsv$cohort_name[cohortsCsv$cohort_id == id]
  data.frame(
    cohortId   = as.integer(id),
    cohortName = if (length(nameRow) > 0) nameRow[1] else paste("Cohort", id),
    json       = paste(readLines(jsonPath, warn = FALSE), collapse = "\n"),
    sql        = paste(readLines(sqlPath,  warn = FALSE), collapse = "\n"),
    stringsAsFactors = FALSE)
}))

cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTable)

if (length(generateCohortIds) > 0) {
  unknown <- setdiff(generateCohortIds, cohortDefinitionSet$cohortId)
  if (length(unknown) > 0) {
    stop("generateCohortIds names cohort(s) not in cohortSpecs: ",
         paste(unknown, collapse = ", "))
  }
  toGenerate <- cohortDefinitionSet[
    cohortDefinitionSet$cohortId %in% generateCohortIds, ]
  message("Regenerating ", nrow(toGenerate), " of ",
          nrow(cohortDefinitionSet), " cohorts: ",
          paste(toGenerate$cohortId, collapse = ", "))
  
  # incremental = TRUE leaves the existing tables in place. With FALSE this
  # call DROPS and recreates them, wiping every cohort in the table.
  CohortGenerator::createCohortTables(
    connectionDetails = connectionDetails,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTableNames = cohortTableNames,
    incremental = TRUE)
  
  CohortGenerator::generateCohortSet(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    tempEmulationSchema = tempSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTableNames = cohortTableNames,
    cohortDefinitionSet = toGenerate,
    incremental = FALSE)
} else {
  message("Skipping cohort generation; using ", cohortDatabaseSchema, ".",
          cohortTable, " as it stands")
}


# =============================================================================
# SECTION 2b -- Cohort diagnostics (optional)
#
# Uses connectionDetails, not the script's connection, so it opens and closes
# its own. executeDiagnostics() disconnects on exit when handed a connection,
# which would close the one the rest of this script depends on.
# =============================================================================

if (runCohortDiagnostics) {
  if (!requireNamespace("CohortDiagnostics", quietly = TRUE)) {
    stop("runCohortDiagnostics is TRUE but CohortDiagnostics is not ",
         "installed:\n  remotes::install_github('OHDSI/CohortDiagnostics')")
  }
  cohortDiagnosticsFolder <- file.path(outputFolder, "cohortDiagnostics")
  dir.create(cohortDiagnosticsFolder, showWarnings = FALSE, recursive = TRUE)
  message("Running cohort diagnostics for cohort(s): ",
          paste(cohortDiagnosticsIds, collapse = ", "))
  
  # Inclusion statistics need the stats tables, which are only populated when
  # the cohorts were generated with generateStats = TRUE. Cohorts.csv has
  # generate_stats = FALSE, so that one is off.
  CohortDiagnostics::executeDiagnostics(
    cohortDefinitionSet = cohortDefinitionSet[
      cohortDefinitionSet$cohortId %in% cohortDiagnosticsIds, ],
    exportFolder = cohortDiagnosticsFolder,
    databaseId = databaseId,
    cohortDatabaseSchema = cohortDatabaseSchema,
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    tempEmulationSchema = tempSchema,
    cohortTable = cohortTable,
    cohortTableNames = cohortTableNames,
    cohortIds = cohortDiagnosticsIds,
    runInclusionStatistics = FALSE,
    runIncludedSourceConcepts = TRUE,
    runOrphanConcepts = TRUE,
    runTimeSeries = FALSE,
    runVisitContext = TRUE,
    runBreakdownIndexEvents = TRUE,
    runIncidenceRate = TRUE,
    runCohortRelationship = TRUE,
    runTemporalCohortCharacterization = TRUE,
    minCellCount = minCellCount,
    incremental = TRUE,
    incrementalFolder = file.path(cohortDiagnosticsFolder, "incremental"))
  
  message("Cohort diagnostics written to ", cohortDiagnosticsFolder)
} else {
  message("Skipping cohort diagnostics (runCohortDiagnostics = FALSE)")
}


connection <- DatabaseConnector::connect(connectionDetails)

# Cleanup is an explicit function called at the end, NOT an on.exit() handler.
# A top-level on.exit() fires as soon as the current top-level expression
# returns under source(), which closes the connection immediately after it is
# opened. If the run stops early, call cleanup() before re-running.
cleanup <- function() {
  try(DatabaseConnector::disconnect(connection), silent = TRUE)
  try(ParallelLogger::unregisterLogger("DEFAULT_FILE_LOGGER"), silent = TRUE)
  invisible(NULL)
}

if (!isTRUE(try(DatabaseConnector::dbIsValid(connection), silent = TRUE))) {
  stop("The database connection is not open immediately after connect().")
}

connectedDbms <- attr(connection, "dbms")
if (!is.null(connectedDbms) && !identical(connectedDbms, targetDialect)) {
  stop("dbms is set to '", targetDialect, "' but the connection reports '",
       connectedDbms, "'. Fix dbms in SECTION 0 before running.")
}

# --- SQL rendering and translation -------------------------------------------
sqlAuditFolder <- file.path(outputFolder, "sql")
dir.create(sqlAuditFolder, showWarnings = FALSE, recursive = TRUE)
.sqlCounter <- 0L

renderTranslate <- function(sql, label, ...) {
  rendered <- SqlRender::render(sql = sql, warnOnMissingParameters = TRUE, ...)
  translated <- SqlRender::translate(sql = rendered,
                                     targetDialect = targetDialect,
                                     tempEmulationSchema = tempSchema)
  .sqlCounter <<- .sqlCounter + 1L
  writeLines(translated,
             file.path(sqlAuditFolder,
                       sprintf("%03d_%s.sql", .sqlCounter,
                               gsub("[^A-Za-z0-9]+", "_", label))))
  translated
}

querySql <- function(sql, label = "query", ...) {
  df <- DatabaseConnector::querySql(connection,
                                    renderTranslate(sql, label, ...))
  names(df) <- tolower(names(df))
  df
}
execSql <- function(sql, label = "statement", ...) {
  DatabaseConnector::executeSql(connection,
                                renderTranslate(sql, label, ...),
                                progressBar = FALSE,
                                reportOverallTime = FALSE)
}

# One row per subject per cohort. A cohort that returns several eras per
# person would otherwise multiply every count that follows.
cohortCounts <- querySql(
  "SELECT cohort_definition_id,
          COUNT(*) AS n_rows,
          COUNT(DISTINCT subject_id) AS n_subjects
   FROM @cohort_database_schema.@cohort_table
   WHERE cohort_definition_id IN (@cohort_ids)
   GROUP BY cohort_definition_id;",
  label = "cohort_counts",
  cohort_database_schema = cohortDatabaseSchema,
  cohort_table = cohortTable,
  cohort_ids = cohortSpecs$cohortId)

missingCohorts <- setdiff(cohortSpecs$cohortId, cohortCounts$cohort_definition_id)
if (length(missingCohorts) > 0) {
  stop("No rows in ", cohortDatabaseSchema, ".", cohortTable,
       " for cohort id(s): ", paste(missingCohorts, collapse = ", "))
}
print(cohortCounts)

execSql(
  "DROP TABLE IF EXISTS #study_cohort;
   SELECT cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
   INTO #study_cohort
   FROM (
     SELECT cohort_definition_id, subject_id, cohort_start_date, cohort_end_date,
            ROW_NUMBER() OVER (PARTITION BY cohort_definition_id, subject_id
                               ORDER BY cohort_start_date, cohort_end_date) AS rn
     FROM @cohort_database_schema.@cohort_table
     WHERE cohort_definition_id IN (@cohort_ids)
   ) x
   WHERE rn = 1;",
  label = "dedup_study_cohort",
  cohort_database_schema = cohortDatabaseSchema,
  cohort_table = cohortTable,
  cohort_ids = cohortSpecs$cohortId)


# =============================================================================
# SECTION 2c -- Strata: prior heart failure
#
# Every cohort is split into three analysis groups, each of which gets its own
# cohort_definition_id in a separate strata table:
#
#   cohortId * 10 + 0   all patients
#   cohortId * 10 + 1   heart failure on or before cohort start
#   cohortId * 10 + 2   no heart failure on or before cohort start
#
# A separate permanent table is used rather than extra rows in the study's own
# cohort table: FeatureExtraction has to be pointed at a real cohort table to
# characterize a group, and writing synthetic ids into the cohort table would
# confuse anything else reading it, CohortDiagnostics included.
# =============================================================================

STRATUM_ALL <- "All patients"
STRATUM_HF  <- "Prior heart failure"
STRATUM_NO  <- "No prior heart failure"

if (runSubgroupAnalysis) {
  message("Flagging heart failure on or before cohort start")
  execSql(
    "DROP TABLE IF EXISTS #hf_concept;
     SELECT DISTINCT ca.descendant_concept_id AS concept_id
     INTO #hf_concept
     FROM @cdm_database_schema.concept_ancestor ca
     WHERE ca.ancestor_concept_id IN (@hf_concept_ids);

     DROP TABLE IF EXISTS #hf_subject;
     SELECT DISTINCT c.cohort_definition_id, c.subject_id
     INTO #hf_subject
     FROM #study_cohort c
     INNER JOIN @cdm_database_schema.condition_occurrence co
             ON co.person_id = c.subject_id
            AND co.condition_start_date <= c.cohort_start_date
     INNER JOIN #hf_concept h ON h.concept_id = co.condition_concept_id;",
    label = "heart_failure_flag",
    cdm_database_schema = cdmDatabaseSchema,
    hf_concept_ids = heartFailureConceptIds)
} else {
  execSql(
    "DROP TABLE IF EXISTS #hf_subject;
     SELECT cohort_definition_id, subject_id
     INTO #hf_subject
     FROM #study_cohort
     WHERE 1 = 0;",
    label = "heart_failure_flag_empty")
}

execSql(
  "DROP TABLE IF EXISTS @cohort_database_schema.@strata_table;
   SELECT cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
   INTO @cohort_database_schema.@strata_table
   FROM (
     SELECT CAST(c.cohort_definition_id * 10 AS INT) AS cohort_definition_id,
            c.subject_id, c.cohort_start_date, c.cohort_end_date
     FROM #study_cohort c

     UNION ALL

     SELECT CAST(c.cohort_definition_id * 10 +
                 CASE WHEN h.subject_id IS NULL THEN 2 ELSE 1 END AS INT),
            c.subject_id, c.cohort_start_date, c.cohort_end_date
     FROM #study_cohort c
     LEFT JOIN #hf_subject h
            ON h.cohort_definition_id = c.cohort_definition_id
           AND h.subject_id = c.subject_id
   ) s;",
  label = "build_strata_table",
  cohort_database_schema = cohortDatabaseSchema,
  strata_table = strataTable)

execSql(
  "DROP TABLE IF EXISTS #study_group;
   SELECT cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
   INTO #study_group
   FROM @cohort_database_schema.@strata_table;",
  label = "study_group",
  cohort_database_schema = cohortDatabaseSchema,
  strata_table = strataTable)

# The analysis grid: one row per cohort per stratum.
strataDefs <- data.frame(
  offset  = c(0L, 1L, 2L),
  stratum = c(STRATUM_ALL, STRATUM_HF, STRATUM_NO),
  stringsAsFactors = FALSE)
if (!runSubgroupAnalysis) strataDefs <- strataDefs[1, , drop = FALSE]

analysisGroups <- do.call(rbind, lapply(seq_len(nrow(strataDefs)), function(s) {
  data.frame(
    groupId     = cohortSpecs$cohortId * 10L + strataDefs$offset[s],
    cohortId    = cohortSpecs$cohortId,
    label       = cohortSpecs$label,
    stratum     = strataDefs$stratum[s],
    windowStart = cohortSpecs$windowStart,
    windowEnd   = cohortSpecs$windowEnd,
    stringsAsFactors = FALSE)
}))

groupCounts <- querySql(
  "SELECT cohort_definition_id, COUNT(*) AS n_subjects
   FROM #study_group GROUP BY cohort_definition_id;",
  label = "group_counts")
groupSizes <- stats::setNames(groupCounts$n_subjects,
                              groupCounts$cohort_definition_id)

missingGroups <- setdiff(analysisGroups$groupId, groupCounts$cohort_definition_id)
if (length(missingGroups) > 0) {
  # An empty stratum is possible (nobody with heart failure), so this is a
  # message rather than a stop; the column will read 0.
  message("No patients in analysis group(s): ",
          paste(missingGroups, collapse = ", "))
  for (g in missingGroups) groupSizes[[as.character(g)]] <- 0
}

# The strata must partition the cohort. If they do not, the heart failure join
# duplicated rows and every subgroup number below would be wrong.
if (runSubgroupAnalysis) {
  for (i in seq_len(nrow(cohortSpecs))) {
    cid <- cohortSpecs$cohortId[i]
    nAll <- groupSizes[[as.character(cid * 10L)]]
    nHf  <- groupSizes[[as.character(cid * 10L + 1L)]]
    nNo  <- groupSizes[[as.character(cid * 10L + 2L)]]
    if (nHf + nNo != nAll) {
      stop("Heart failure strata do not partition cohort ", cid, ": ",
           nHf, " + ", nNo, " != ", nAll)
    }
  }
  message("Heart failure strata partition every cohort.")
}


# =============================================================================
# SECTION 3 -- Antihypertensive exposure, per analysis group
# =============================================================================

ingredients <- readr::read_csv("AntihypertensiveIngredients.csv",
                               show_col_types = FALSE)
stopifnot(all(c("classId", "ingredientConceptId") %in% names(ingredients)))
if (anyDuplicated(ingredients$ingredientConceptId) > 0) {
  stop("An ingredient appears in more than one class, so class counts would ",
       "double-count it.")
}
if (!"subclassId" %in% names(ingredients)) ingredients$subclassId <- NA_character_
ingredients$subclassId[is.na(ingredients$subclassId)] <- ""
unlabelled <- ingredients$ingredientName[ingredients$classId == "OTHER" &
                                           !nzchar(ingredients$subclassId)]
if (length(unlabelled) > 0) {
  message("Note: OTHER ingredient(s) with no subclass, grouped as ",
          "'Other, unspecified': ", paste(unlabelled, collapse = ", "))
  ingredients$subclassId[ingredients$classId == "OTHER" &
                           !nzchar(ingredients$subclassId)] <- "Other, unspecified"
}

DatabaseConnector::insertTable(
  connection = connection, tableName = "#ah_ing",
  data = data.frame(class_id = ingredients$classId,
                    subclass_id = ingredients$subclassId,
                    ingredient_concept_id = as.integer(ingredients$ingredientConceptId)),
  dropTableIfExists = TRUE, createTable = TRUE, tempTable = TRUE,
  camelCaseToSnakeCase = FALSE, progressBar = FALSE,
  tempEmulationSchema = tempSchema)

execSql(
  "DROP TABLE IF EXISTS #ah_drug;
   SELECT DISTINCT ca.descendant_concept_id AS drug_concept_id,
          i.ingredient_concept_id, i.class_id, i.subclass_id
   INTO #ah_drug
   FROM @cdm_database_schema.concept_ancestor ca
   INNER JOIN #ah_ing i ON i.ingredient_concept_id = ca.ancestor_concept_id;

   DROP TABLE IF EXISTS #ah_product;
   SELECT drug_concept_id, COUNT(DISTINCT ingredient_concept_id) AS n_ah_ingredients
   INTO #ah_product
   FROM #ah_drug GROUP BY drug_concept_id;",
  label = "ah_drug_products",
  cdm_database_schema = cdmDatabaseSchema)

classFilter <- if (countSpcComponentsInClasses) "1 = 1" else "is_spc = 0"

# Built here rather than as an @parameter inside the statement because
# SqlRender::render() substitutes once: a parameter whose VALUE contains
# @cdm_database_schema would be left unsubstituted.
visitExclusionExpr <- if (length(excludeVisitTypeConceptIds) > 0) {
  sprintf(paste("CASE WHEN EXISTS (SELECT 1 FROM %s.visit_occurrence v",
                "WHERE v.visit_occurrence_id = de.visit_occurrence_id",
                "AND v.visit_type_concept_id IN (%s)) THEN 1 ELSE 0 END"),
          cdmDatabaseSchema,
          paste(excludeVisitTypeConceptIds, collapse = ", "))
} else {
  "0"
}
if (length(excludeVisitTypeConceptIds) > 0) {
  message("Excluding drug records from visit_type_concept_id(s): ",
          paste(excludeVisitTypeConceptIds, collapse = ", "))
}

ahResults <- lapply(seq_len(nrow(analysisGroups)), function(i) {
  grp <- analysisGroups[i, ]
  message("Antihypertensive exposure: group ", grp$groupId, " (",
          grp$stratum, "), days ", grp$windowStart, " to ", grp$windowEnd)
  
  execSql(
    "DROP TABLE IF EXISTS #ah_exp;
     SELECT c.subject_id, de.drug_exposure_id,
            de.drug_exposure_start_date AS exposure_date,
            d.class_id, d.subclass_id, p.n_ah_ingredients,
            @visit_exclusion_expr AS is_excluded_visit
     INTO #ah_exp
     FROM #study_group c
     INNER JOIN @cdm_database_schema.drug_exposure de ON de.person_id = c.subject_id
     INNER JOIN #ah_drug d ON d.drug_concept_id = de.drug_concept_id
     INNER JOIN #ah_product p ON p.drug_concept_id = de.drug_concept_id
     WHERE c.cohort_definition_id = @group_id
       AND de.drug_exposure_start_date >= DATEADD(DAY, @window_start, c.cohort_start_date)
       AND de.drug_exposure_start_date <= DATEADD(DAY, @window_end, c.cohort_start_date);",
    label = "ah_exposures",
    cdm_database_schema = cdmDatabaseSchema,
    visit_exclusion_expr = visitExclusionExpr,
    group_id = grp$groupId,
    window_start = grp$windowStart,
    window_end = grp$windowEnd)
  
  # The deduplication key. subclass_id is in it because the OTHER breakout is
  # reported at subclass level -- without it a furosemide and a doxazosin on
  # the same day would collapse into one OTHER row and the subclass rows would
  # stop summing to it. is_spc is in it because combination records are
  # counted separately from standalone ones.
  #
  # exposure_date is in the key only for "day" scope. Dropping it is what
  # makes "window" scope collapse a class across every day of the window.
  #
  # DISTINCT rather than GROUP BY: there is nothing to aggregate.
  execSql(
    "DROP TABLE IF EXISTS #ah_day;
     SELECT @dedup_distinct subject_id, @dedup_date_col class_id, subclass_id,
            CASE WHEN n_ah_ingredients > 1 THEN 1 ELSE 0 END AS is_spc
     INTO #ah_day
     FROM #ah_exp
     WHERE is_excluded_visit = 0;",
    label = "ah_day_level",
    dedup_distinct = if (dedupScope == "none") "" else "DISTINCT",
    dedup_date_col = if (dedupScope == "day") "exposure_date," else "")
  
  # Two class counts per patient, deliberately:
  #   n_classes     -- honours countSpcComponentsInClasses
  #   n_classes_all -- every agent counted, combination components included
  # The regimen rows are built from n_classes_all, because a patient on a
  # single-pill combination plus one other agent is not on monotherapy.
  perPatient <- querySql(
    "SELECT n_classes, n_classes_all, any_spc, COUNT(*) AS n_patients
     FROM (
       SELECT subject_id,
              COUNT(DISTINCT CASE WHEN @class_filter THEN class_id END) AS n_classes,
              COUNT(DISTINCT class_id) AS n_classes_all,
              MAX(is_spc) AS any_spc
       FROM #ah_day
       GROUP BY subject_id
     ) p
     GROUP BY n_classes, n_classes_all, any_spc;",
    label = "ah_per_patient",
    class_filter = classFilter)
  
  byClass <- querySql(
    "SELECT class_id, subclass_id,
            COUNT(DISTINCT subject_id) AS n_patients,
            COUNT(*) AS n_records
     FROM #ah_day
     WHERE @class_filter
     GROUP BY class_id, subclass_id;",
    label = "ah_by_class",
    class_filter = classFilter)
  
  # n_records is the Table 2 class denominator and so comes from the day-level
  # table. n_orders stays on the raw table: it counts actual drug_exposure
  # rows, which is a different quantity and should not be deduplicated.
  totals <- querySql(
    "SELECT d.n_treated, d.n_records, d.n_spc_patients,
            e.n_orders, e.n_spc_orders, e.n_excluded_rows, e.n_excluded_orders
     FROM (
       SELECT COUNT(DISTINCT subject_id) AS n_treated,
              COUNT(*) AS n_records,
              COUNT(DISTINCT CASE WHEN is_spc = 1 THEN subject_id END)
                AS n_spc_patients
       FROM #ah_day
     ) d
     CROSS JOIN (
       SELECT COUNT(DISTINCT CASE WHEN is_excluded_visit = 0
                                  THEN drug_exposure_id END) AS n_orders,
              COUNT(DISTINCT CASE WHEN is_excluded_visit = 0
                                   AND n_ah_ingredients > 1
                                  THEN drug_exposure_id END) AS n_spc_orders,
              SUM(is_excluded_visit) AS n_excluded_rows,
              COUNT(DISTINCT CASE WHEN is_excluded_visit = 1
                                  THEN drug_exposure_id END) AS n_excluded_orders
       FROM #ah_exp
     ) e;",
    label = "ah_totals")
  
  list(groupId = grp$groupId, perPatient = perPatient,
       byClass = byClass, totals = totals)
})
names(ahResults) <- as.character(analysisGroups$groupId)


# =============================================================================
# SECTION 4 -- Blood pressure at index
# =============================================================================

execSql(
  "DROP TABLE IF EXISTS #bp_index;
   SELECT cohort_definition_id, subject_id, modality, value_as_number
   INTO #bp_index
   FROM (
     SELECT c.cohort_definition_id, c.subject_id,
            CASE WHEN m.measurement_concept_id IN (@sbp_ids) THEN 'sbp' ELSE 'dbp' END AS modality,
            m.value_as_number,
            ROW_NUMBER() OVER (
              PARTITION BY c.cohort_definition_id, c.subject_id,
                           CASE WHEN m.measurement_concept_id IN (@sbp_ids) THEN 'sbp' ELSE 'dbp' END
              ORDER BY m.measurement_date, m.measurement_id) AS rn
     FROM #study_group c
     INNER JOIN @cdm_database_schema.measurement m ON m.person_id = c.subject_id
     WHERE m.measurement_concept_id IN (@sbp_ids, @dbp_ids)
       AND m.value_as_number IS NOT NULL
       AND m.value_as_number >= @value_min
       AND m.value_as_number <= @value_max
       AND m.measurement_date = c.cohort_start_date
   ) ranked
   WHERE rn = 1;",
  label = "bp_at_index",
  cdm_database_schema = cdmDatabaseSchema,
  sbp_ids = sbpConceptIds, dbp_ids = dbpConceptIds,
  value_min = bpValueRange[1], value_max = bpValueRange[2])

bpSummary <- querySql(
  "SELECT cohort_definition_id, modality,
          COUNT(*) AS n_measured,
          AVG(value_as_number) AS mean_value,
          STDEV(value_as_number) AS sd_value,
          SUM(CASE WHEN (modality = 'sbp' AND value_as_number >= @sbp_threshold)
                     OR (modality = 'dbp' AND value_as_number >= @dbp_threshold)
                   THEN 1 ELSE 0 END) AS n_above
   FROM #bp_index
   GROUP BY cohort_definition_id, modality;",
  label = "bp_summary",
  sbp_threshold = sbpThreshold, dbp_threshold = dbpThreshold)


# =============================================================================
# SECTION 5 -- Demographics, from the standard covariate set
#
# Pointed at the strata table, so each analysis group is characterized as its
# own cohort.
# =============================================================================

covariateSettings <- FeatureExtraction::createCovariateSettings(
  useDemographicsGender = TRUE,
  useDemographicsAge = TRUE,
  useDemographicsAgeGroup = TRUE,
  useDemographicsRace = TRUE,
  useDemographicsEthnicity = TRUE,
  useDemographicsIndexYear = TRUE)

# FeatureExtraction renamed cohortId -> cohortIds, and oracleTempSchema ->
# tempEmulationSchema. Dropping either argument would silently characterize
# every cohort in the table, so the names are resolved rather than guessed.
fxArgs <- names(formals(FeatureExtraction::getDbCovariateData))
cohortArg <- if ("cohortIds" %in% fxArgs) "cohortIds" else
  if ("cohortId" %in% fxArgs) "cohortId" else
    stop("FeatureExtraction::getDbCovariateData accepts neither cohortIds ",
         "nor cohortId; check the installed version.")

demographics <- lapply(analysisGroups$groupId, function(gid) {
  message("Demographics for analysis group ", gid)
  covArgs <- list(connection = connection,
                  cdmDatabaseSchema = cdmDatabaseSchema,
                  cohortDatabaseSchema = cohortDatabaseSchema,
                  cohortTable = strataTable,
                  covariateSettings = covariateSettings,
                  aggregated = TRUE)
  covArgs[[cohortArg]] <- gid
  if ("tempEmulationSchema" %in% fxArgs) {
    covArgs$tempEmulationSchema <- tempSchema
  } else if ("oracleTempSchema" %in% fxArgs) {
    covArgs$oracleTempSchema <- tempSchema
  }
  covData <- do.call(FeatureExtraction::getDbCovariateData, covArgs)
  list(
    id = gid,
    binary = as.data.frame(dplyr::collect(covData$covariates)),
    continuous = if ("covariatesContinuous" %in% names(covData))
      as.data.frame(dplyr::collect(covData$covariatesContinuous)) else
        data.frame(),
    ref = as.data.frame(dplyr::collect(covData$covariateRef)))
})
names(demographics) <- as.character(analysisGroups$groupId)


# =============================================================================
# SECTION 6 -- Assemble the tables
#
# Denominators, stated once because they differ by block:
#   Patients, N                            the analysis group
#   Any antihypertensive                   the analysis group
#   Monotherapy / >=2 / >=3 / single-pill  TREATED patients
#   Drug class                             antihypertensive drug RECORDS
# =============================================================================

mask <- function(n) ifelse(!is.na(n) & n > 0 & n < minCellCount, NA_real_, n)

fmtN <- function(n) {
  n <- mask(n)
  ifelse(is.na(n), paste0("<", minCellCount), formatC(n, format = "d", big.mark = ","))
}
fmtPct <- function(n, d) {
  n <- mask(n)
  ifelse(is.na(n), paste0("<", minCellCount),
         ifelse(is.na(d) | d == 0, fmtN(n),
                sprintf("%s (%.1f%%)", formatC(n, format = "d", big.mark = ","),
                        100 * n / d)))
}
fmtMeanSd <- function(m, s) ifelse(is.na(m), "--", sprintf("%.1f (%.1f)", m, s))

sizeOf      <- function(gid) {
  v <- groupSizes[[as.character(gid)]]
  if (is.null(v)) 0 else v
}
treatedOf   <- function(gid) {
  r <- ahResults[[as.character(gid)]]
  if (is.null(r) || nrow(r$totals) == 0) 0 else as.numeric(r$totals$n_treated[1])
}
recordsOf   <- function(gid) {
  r <- ahResults[[as.character(gid)]]
  if (is.null(r) || nrow(r$totals) == 0) 0 else as.numeric(r$totals$n_records[1])
}
totalField <- function(gid, field) {
  r <- ahResults[[as.character(gid)]]
  if (is.null(r) || nrow(r$totals) == 0 || !field %in% names(r$totals)) return(0)
  v <- as.numeric(r$totals[[field]][1])
  if (is.na(v)) 0 else v
}
excludedOf       <- function(gid) totalField(gid, "n_excluded_rows")
excludedOrdersOf <- function(gid) totalField(gid, "n_excluded_orders")
regimenOf <- function(gid, what) {
  r <- ahResults[[as.character(gid)]]
  if (is.null(r)) return(0)
  pp <- r$perPatient
  if (nrow(pp) == 0) return(0)
  switch(what,
         mono     = sum(pp$n_patients[pp$n_classes_all == 1]),
         two      = sum(pp$n_patients[pp$n_classes_all >= 2]),
         three    = sum(pp$n_patients[pp$n_classes_all >= 3]),
         spc      = sum(pp$n_patients[pp$any_spc == 1]),
         monoPub  = sum(pp$n_patients[pp$n_classes == 1]),
         twoPub   = sum(pp$n_patients[pp$n_classes >= 2]),
         threePub = sum(pp$n_patients[pp$n_classes >= 3]),
         stop("unknown regimen measure: ", what))
}
classRecordsOf <- function(gid, keys) {
  r <- ahResults[[as.character(gid)]]
  if (is.null(r)) return(0)
  sum(r$byClass$n_records[r$byClass$class_id %in% keys], na.rm = TRUE)
}
subclassRecordsOf <- function(gid, sub) {
  r <- ahResults[[as.character(gid)]]
  if (is.null(r)) return(0)
  bc <- r$byClass
  sum(bc$n_records[bc$class_id == "OTHER" & bc$subclass_id == sub], na.rm = TRUE)
}
demoIdSum <- function(gid, covariateIds) {
  d <- demographics[[as.character(gid)]]
  if (is.null(d) || length(covariateIds) == 0) return(NA_real_)
  sum(d$binary$sumValue[d$binary$covariateId %in% covariateIds], na.rm = TRUE)
}
demoBinary <- function(gid, pattern) {
  d <- demographics[[as.character(gid)]]
  if (is.null(d)) return(NA_real_)
  ids <- d$ref$covariateId[grepl(pattern, d$ref$covariateName, ignore.case = TRUE)]
  if (length(ids) == 0) return(NA_real_)
  sum(d$binary$sumValue[d$binary$covariateId %in% ids], na.rm = TRUE)
}
demoContinuous <- function(gid, pattern, field) {
  d <- demographics[[as.character(gid)]]
  if (is.null(d) || nrow(d$continuous) == 0) return(NA_real_)
  ids <- d$ref$covariateId[grepl(pattern, d$ref$covariateName, ignore.case = TRUE)]
  row <- d$continuous[d$continuous$covariateId %in% ids, , drop = FALSE]
  if (nrow(row) == 0) return(NA_real_)
  as.numeric(row[[field]][1])
}
bpValue <- function(gid, modality, field) {
  row <- bpSummary[bpSummary$cohort_definition_id == gid &
                     bpSummary$modality == modality, , drop = FALSE]
  if (nrow(row) == 0) return(NA_real_)
  as.numeric(row[[field]][1])
}

# Row builders, over one stratum's groups.
makeRowFns <- function(groups) {
  byGroup <- function(fn) vapply(groups$groupId, fn, character(1))
  list(
    addRow = function(rows, label, fn) {
      c(rows, list(c(list(Measure = label),
                     stats::setNames(as.list(byGroup(fn)), groups$label))))
    },
    blankRow = function(rows, label) {
      c(rows, list(c(list(Measure = label),
                     stats::setNames(as.list(rep("", nrow(groups))),
                                     groups$label))))
    })
}

buildTable1 <- function(groups) {
  f <- makeRowFns(groups); addRow <- f$addRow; blankRow <- f$blankRow
  t1 <- list()
  t1 <- addRow(t1, "Patients, N", function(g) fmtN(sizeOf(g)))
  t1 <- addRow(t1, "Age at index, mean (SD), y", function(g) {
    fmtMeanSd(demoContinuous(g, "^age$|age in years", "averageValue"),
              demoContinuous(g, "^age$|age in years", "standardDeviation"))
  })
  t1 <- addRow(t1, "Female, n (%)", function(g) {
    fmtPct(demoBinary(g, "gender = FEMALE"), sizeOf(g))
  })
  t1 <- blankRow(t1, "Blood pressure at index")
  t1 <- addRow(t1, "Systolic, mean (SD), mm Hg", function(g) {
    fmtMeanSd(bpValue(g, "sbp", "mean_value"), bpValue(g, "sbp", "sd_value"))
  })
  t1 <- addRow(t1, "Diastolic, mean (SD), mm Hg", function(g) {
    fmtMeanSd(bpValue(g, "dbp", "mean_value"), bpValue(g, "dbp", "sd_value"))
  })
  t1 <- addRow(t1, sprintf("Systolic %g mm Hg or greater, n (%%)", sbpThreshold),
               function(g) fmtPct(bpValue(g, "sbp", "n_above"),
                                  bpValue(g, "sbp", "n_measured")))
  t1 <- addRow(t1, sprintf("Diastolic %g mm Hg or greater, n (%%)", dbpThreshold),
               function(g) fmtPct(bpValue(g, "dbp", "n_above"),
                                  bpValue(g, "dbp", "n_measured")))
  t1 <- addRow(t1, "Patients with a reading at index, n (%)", function(g) {
    fmtPct(bpValue(g, "sbp", "n_measured"), sizeOf(g))
  })
  
  refRows <- demographics[[as.character(groups$groupId[1])]]$ref
  
  ageRef <- refRows[grepl("^age group", refRows$covariateName,
                          ignore.case = TRUE), , drop = FALSE]
  if (nrow(ageRef) > 0) {
    lowerBound <- as.integer(sub(".*?([0-9]+).*", "\\1", ageRef$covariateName))
    if (any(is.na(lowerBound))) {
      stop("Could not read a lower bound from age covariate name(s): ",
           paste(ageRef$covariateName[is.na(lowerBound)], collapse = ", "))
    }
    belowAdult <- ageRef$covariateName[lowerBound < 18]
    if (length(belowAdult) > 0) {
      message("Note: age band(s) below 18 folded into the first band: ",
              paste(belowAdult, collapse = ", "))
    }
    bandOf <- vapply(lowerBound, function(lo) {
      for (b in ageBands) if (lo < b$max) return(b$label)
      ageBands[[length(ageBands)]]$label
    }, character(1))
    
    t1 <- blankRow(t1, "Age group, n (%)")
    for (b in ageBands) {
      local({
        ids <- ageRef$covariateId[bandOf == b$label]
        t1 <<- addRow(t1, paste0("    ", b$label), function(g) {
          fmtPct(demoIdSum(g, ids), sizeOf(g))
        })
      })
    }
  }
  
  raceRef <- refRows[grepl("^race", refRows$covariateName,
                           ignore.case = TRUE), , drop = FALSE]
  if (nrow(raceRef) > 0) {
    groupOf <- rep(NA_character_, nrow(raceRef))
    for (rg in names(raceGroups)) {
      hit <- is.na(groupOf) &
        grepl(raceGroups[[rg]], raceRef$covariateName, ignore.case = TRUE)
      groupOf[hit] <- rg
    }
    plain <- is.na(groupOf)
    groupOf[plain] <- sub("^race\\s*[:=]?\\s*", "", raceRef$covariateName[plain],
                          ignore.case = TRUE)
    
    t1 <- blankRow(t1, "Race, n (%)")
    order1 <- vapply(unique(groupOf), function(rg) {
      sum(demoIdSum(groups$groupId[1], raceRef$covariateId[groupOf == rg]),
          na.rm = TRUE)
    }, numeric(1))
    for (rg in names(sort(order1, decreasing = TRUE))) {
      local({
        ids <- raceRef$covariateId[groupOf == rg]
        t1 <<- addRow(t1, paste0("    ", rg), function(g) {
          fmtPct(demoIdSum(g, ids), sizeOf(g))
        })
      })
    }
  }
  
  do.call(rbind, lapply(t1, as.data.frame, stringsAsFactors = FALSE))
}

otherSubclasses <- sort(unique(unlist(lapply(ahResults, function(r) {
  bc <- r$byClass
  bc$subclass_id[bc$class_id == "OTHER" & nzchar(bc$subclass_id)]
}))))

buildTable2 <- function(groups) {
  f <- makeRowFns(groups); addRow <- f$addRow; blankRow <- f$blankRow
  t2 <- list()
  t2 <- addRow(t2, "Patients, N", function(g) fmtN(sizeOf(g)))
  t2 <- addRow(t2, "Any antihypertensive, n (%)",
               function(g) fmtPct(treatedOf(g), sizeOf(g)))
  t2 <- blankRow(t2, "Among treated patients, n (%)")
  t2 <- addRow(t2, "Monotherapy (1 class)",
               function(g) fmtPct(regimenOf(g, "mono"), treatedOf(g)))
  t2 <- addRow(t2, "≥ 2 classes (includes ≥ 3)",
               function(g) fmtPct(regimenOf(g, "two"), treatedOf(g)))
  t2 <- addRow(t2, "≥ 3 classes",
               function(g) fmtPct(regimenOf(g, "three"), treatedOf(g)))
  t2 <- addRow(t2, "Single-pill combination",
               function(g) fmtPct(regimenOf(g, "spc"), treatedOf(g)))
  if (!countSpcComponentsInClasses) {
    t2 <- blankRow(t2, "Excluding combination components (as previously published)")
    t2 <- addRow(t2, "Monotherapy (1 class)",
                 function(g) fmtPct(regimenOf(g, "monoPub"), treatedOf(g)))
    t2 <- addRow(t2, "≥ 2 classes (includes ≥ 3)",
                 function(g) fmtPct(regimenOf(g, "twoPub"), treatedOf(g)))
    t2 <- addRow(t2, "≥ 3 classes",
                 function(g) fmtPct(regimenOf(g, "threePub"), treatedOf(g)))
  }
  t2 <- blankRow(t2, "Drug class, n (%)")
  classRows <- list(list("ACE inhibitors", "ACEI"),
                    list("ARBs",           "ARB"),
                    list("Beta blockers",  "BB"),
                    list("CCB",            c("DCCB", "NDCCB")),
                    list("TZDs",           "TD"),
                    list("Others",         "OTHER"))
  for (cr in classRows) {
    local({
      keys <- cr[[2]]
      t2 <<- addRow(t2, cr[[1]],
                    function(g) fmtPct(classRecordsOf(g, keys), recordsOf(g)))
    })
  }
  # A partition of the "Others" row, not an addition to it.
  for (sub in otherSubclasses) {
    local({
      key <- sub
      t2 <<- addRow(t2, paste0("    ", key),
                    function(g) fmtPct(subclassRecordsOf(g, key), recordsOf(g)))
    })
  }
  # A drug record belongs to exactly one class, so these are sums of the rows
  # above, not unions of patient sets -- no overlap to remove.
  t2 <- blankRow(t2, "Combined classes, n (%)")
  t2 <- addRow(t2, "ACE inhibitor or ARB", function(g) {
    fmtPct(classRecordsOf(g, c("ACEI", "ARB")), recordsOf(g))
  })
  t2 <- addRow(t2, "ACE inhibitor, ARB, or CCB", function(g) {
    fmtPct(classRecordsOf(g, c("ACEI", "ARB", "DCCB", "NDCCB")), recordsOf(g))
  })
  
  do.call(rbind, lapply(t2, as.data.frame, stringsAsFactors = FALSE))
}


# =============================================================================
# SECTION 7 -- Write output, and check the arithmetic
# =============================================================================

slug <- function(x) gsub("[^A-Za-z0-9]+", "_", x)

tables <- list()
for (st in strataDefs$stratum) {
  groups <- analysisGroups[analysisGroups$stratum == st, , drop = FALSE]
  groups <- groups[match(cohortSpecs$cohortId, groups$cohortId), , drop = FALSE]
  t1 <- buildTable1(groups)
  t2 <- buildTable2(groups)
  tables[[st]] <- list(table1 = t1, table2 = t2)
  
  suffix <- if (identical(st, STRATUM_ALL)) "" else paste0("_", slug(st))
  readr::write_csv(t1, file.path(outputFolder, paste0(
    databaseId, "_Table1_characteristics", suffix, ".csv")))
  readr::write_csv(t2, file.path(outputFolder, paste0(
    databaseId, "_Table2_treatment", suffix, ".csv")))
}

readr::write_csv(cohortCounts, file.path(outputFolder,
                                         paste0(databaseId, "_cohort_counts.csv")))
readr::write_csv(
  merge(analysisGroups, data.frame(
    groupId = as.integer(names(groupSizes)),
    nSubjects = as.numeric(groupSizes)), by = "groupId", all.x = TRUE),
  file.path(outputFolder, paste0(databaseId, "_analysis_groups.csv")))

checks <- do.call(rbind, lapply(seq_len(nrow(analysisGroups)), function(i) {
  g <- analysisGroups$groupId[i]
  classSum <- sum(ahResults[[as.character(g)]]$byClass$n_records, na.rm = TRUE)
  data.frame(
    groupId = g,
    cohortId = analysisGroups$cohortId[i],
    stratum = analysisGroups$stratum[i],
    nSubjects = sizeOf(g),
    treated = treatedOf(g),
    monoPlusTwo = regimenOf(g, "mono") + regimenOf(g, "two"),
    unaccounted = treatedOf(g) - regimenOf(g, "mono") - regimenOf(g, "two"),
    monoPlusTwoPublished = regimenOf(g, "monoPub") + regimenOf(g, "twoPub"),
    unaccountedPublished = treatedOf(g) - regimenOf(g, "monoPub") -
      regimenOf(g, "twoPub"),
    classRecords = classSum,
    allRecords = recordsOf(g),
    excludedVisitRows = excludedOf(g),
    excludedVisitOrders = excludedOrdersOf(g),
    classShareOfRecords = round(100 * classSum / max(recordsOf(g), 1), 1),
    stringsAsFactors = FALSE)
}))
readr::write_csv(checks, file.path(outputFolder,
                                   paste0(databaseId, "_arithmetic_checks.csv")))

# Monotherapy and two-or-more classes are mutually exclusive and exhaustive
# once every agent is counted, so anything other than zero is a bug here.
if (any(checks$unaccounted != 0)) {
  stop("Regimen rows do not account for every treated patient in group(s): ",
       paste(checks$groupId[checks$unaccounted != 0], collapse = ", "),
       ". See ", databaseId, "_arithmetic_checks.csv")
}

# The strata must also add up on treated patients and drug records, which is a
# stronger check than the subject-count partition in SECTION 2c.
if (runSubgroupAnalysis) {
  for (cid in cohortSpecs$cohortId) {
    for (measure in c("treated", "allRecords")) {
      total <- checks[[measure]][checks$groupId == cid * 10L]
      parts <- sum(checks[[measure]][checks$groupId %in% c(cid * 10L + 1L,
                                                           cid * 10L + 2L)])
      if (!isTRUE(all.equal(total, parts))) {
        stop("Heart failure strata do not sum on '", measure, "' for cohort ",
             cid, ": ", parts, " != ", total)
      }
    }
  }
  message("Heart failure strata sum correctly on patients, treated and records.")
}

if (!countSpcComponentsInClasses && any(checks$unaccountedPublished > 0)) {
  message("Note: under the published counting, ",
          sum(checks$unaccountedPublished), " treated patients fall into ",
          "neither regimen row because their only antihypertensive is a ",
          "combination product.")
}

cleanup()

message("\nDone. Files written to ", outputFolder)
for (st in names(tables)) {
  message("\n=== ", st, " ===")
  print(tables[[st]]$table1, right = FALSE)
  print(tables[[st]]$table2, right = FALSE)
}
