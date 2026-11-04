# =============================================================================
# CAPTUR-HTN -- Hypertension Guidelines to Practice
#
# One script. Generates the three cohorts with CohortGenerator, computes every
# feature both tables need with FeatureExtraction, and writes the tables.
#
#   Table 1  Characteristics of the hypertension cohorts and the base
#            ambulatory population
#   Table 2  Antihypertensive treatment patterns, by cohort
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

# --- Study settings ----------------------------------------------------------
# Cohort ids, and the drug window each one uses. The window is the single most
# consequential setting in the study: it decides who counts as treated.
#   newly identified : index visit only         -> (0, 1)
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
#          single-pill rows. Class percentages then do NOT sum to 100, and
#          monotherapy + >=2 classes does not account for every treated
#          patient. This reproduces the published table.
#   TRUE:  each component is counted in its class. Both problems go away, and
#          every percentage in the drug-class block moves.
countSpcComponentsInClasses <- FALSE

# Set FALSE to reuse a cohort table that is already generated.
generateCohorts <- TRUE


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

# Platforms without real temp tables need somewhere to emulate them. Catching
# this here beats a translation that silently writes to the wrong place.
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
  # unregisterLogger()'s signature has changed across ParallelLogger versions,
  # and this runs in on.exit() -- where an error would replace whatever real
  # error caused the exit. Tearing down the log is never worth losing that.
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

if (generateCohorts) {
  message("Generating ", nrow(cohortDefinitionSet), " cohorts into ",
          cohortDatabaseSchema, ".", cohortTable)
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
    cohortDefinitionSet = cohortDefinitionSet,
    incremental = TRUE)
} else {
  message("Skipping cohort generation; using ", cohortDatabaseSchema, ".",
          cohortTable, " as it stands")
}

connection <- DatabaseConnector::connect(connectionDetails)


# The dialect the script translates to must be the dialect it is talking to.
# These can disagree when a connection is reused or the config is edited
# mid-session, and the result is SQL that is valid for the wrong platform.
connectedDbms <- attr(connection, "dbms")
if (!is.null(connectedDbms) && !identical(connectedDbms, targetDialect)) {
  stop("dbms is set to '", targetDialect, "' but the connection reports '",
       connectedDbms, "'. Fix dbms in SECTION 0 before running.")
}

# --- SQL rendering and translation -------------------------------------------
# render() substitutes the @parameters, then translate() rewrites the statement
# into `targetDialect`. Both steps happen here rather than inside
# DatabaseConnector's renderTranslate* convenience wrappers, so that the exact
# SQL sent to the server is available to inspect, and is written to
# <outputFolder>/sql/ for audit.
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

# One row per subject per cohort. A cohort that returns several eras per person
# would otherwise multiply every count that follows by the number of eras --
# the defect that produced sex percentages above 100 in the earlier analysis.
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

dedup <- querySql(
  "SELECT cohort_definition_id, COUNT(*) AS n_kept FROM #study_cohort
   GROUP BY cohort_definition_id;",
    label = "dedup_check")
cohortSizes <- stats::setNames(dedup$n_kept, dedup$cohort_definition_id)
dropped <- merge(cohortCounts, dedup, by = "cohort_definition_id")
if (any(dropped$n_rows > dropped$n_kept)) {
  message("De-duplicated extra cohort eras in: ",
          paste(dropped$cohort_definition_id[dropped$n_rows > dropped$n_kept],
                collapse = ", "))
}


# =============================================================================
# SECTION 3 -- Antihypertensive exposure, per cohort and window
#
# Built once per cohort because the drug window differs by cohort. Produces,
# per cohort: treated patients, distinct class count per patient, single-pill
# combination use, and drug-record counts by class -- the four things Table 2
# needs and the standard covariate set does not provide.
# =============================================================================

ingredients <- readr::read_csv("AntihypertensiveIngredients.csv",
                               show_col_types = FALSE)
stopifnot(all(c("classId", "ingredientConceptId") %in% names(ingredients)))
if (anyDuplicated(ingredients$ingredientConceptId) > 0) {
  stop("An ingredient appears in more than one class, so class counts would ",
       "double-count it.")
}
# subclassId breaks the catch-all OTHER class into pharmacologic groups for
# Table 2. Ingredients in the named classes carry no subclass.
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

# Expand ingredients to every drug product, and count how many distinct
# antihypertensive ingredients each product contains: two or more is a
# single-pill combination.
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

classFilter <- if (countSpcComponentsInClasses) "1 = 1" else "n_ah_ingredients = 1"

ahResults <- lapply(seq_len(nrow(cohortSpecs)), function(i) {
  spec <- cohortSpecs[i, ]
  message("Antihypertensive exposure for cohort ", spec$cohortId,
          ", days ", spec$windowStart, " to ", spec$windowEnd)

  # One row per (drug_exposure, antihypertensive ingredient). A combination
  # product therefore contributes one row per ingredient, which is why the
  # record count exceeds the order count.
  execSql(
    "DROP TABLE IF EXISTS #ah_exp;
     SELECT c.subject_id, de.drug_exposure_id, d.class_id, d.subclass_id,
            p.n_ah_ingredients
     INTO #ah_exp
     FROM #study_cohort c
     INNER JOIN @cdm_database_schema.drug_exposure de ON de.person_id = c.subject_id
     INNER JOIN #ah_drug d ON d.drug_concept_id = de.drug_concept_id
     INNER JOIN #ah_product p ON p.drug_concept_id = de.drug_concept_id
     WHERE c.cohort_definition_id = @cohort_id
       AND de.drug_exposure_start_date >= DATEADD(DAY, @window_start, c.cohort_start_date)
       AND de.drug_exposure_start_date <= DATEADD(DAY, @window_end, c.cohort_start_date);",
    label = "ah_exposures",
    cdm_database_schema = cdmDatabaseSchema,
    cohort_id = spec$cohortId,
    window_start = spec$windowStart,
    window_end = spec$windowEnd)

  # Collapse to one row per patient first, then tabulate.
  # Two class counts per patient, deliberately:
  #   n_classes     -- honours countSpcComponentsInClasses, so when that is
  #                    FALSE a patient whose only antihypertensives are
  #                    combination products counts 0 classes.
  #   n_classes_all -- every agent counted, combination components included.
  # The regimen rows are built from n_classes_all, because a patient on a
  # single-pill combination plus one other agent is on three drugs, not on
  # monotherapy. n_classes is kept only to reproduce the published figures.
  perPatient <- querySql(
    "SELECT n_classes, n_classes_all, any_spc, COUNT(*) AS n_patients
     FROM (
       SELECT subject_id,
              COUNT(DISTINCT CASE WHEN @class_filter THEN class_id END) AS n_classes,
              COUNT(DISTINCT class_id) AS n_classes_all,
              MAX(CASE WHEN n_ah_ingredients > 1 THEN 1 ELSE 0 END) AS any_spc
       FROM #ah_exp
       GROUP BY subject_id
     ) p
     GROUP BY n_classes, n_classes_all, any_spc;",
    label = "ah_per_patient",
    class_filter = classFilter)

  byClass <- querySql(
    "SELECT class_id, subclass_id,
            COUNT(DISTINCT subject_id) AS n_patients,
            COUNT(*) AS n_records
     FROM #ah_exp
     WHERE @class_filter
     GROUP BY class_id, subclass_id;",
    label = "ah_by_class",
    class_filter = classFilter)

  totals <- querySql(
    "SELECT COUNT(DISTINCT subject_id) AS n_treated,
            COUNT(*) AS n_records,
            COUNT(DISTINCT drug_exposure_id) AS n_orders,
            COUNT(DISTINCT CASE WHEN n_ah_ingredients > 1 THEN drug_exposure_id END)
              AS n_spc_orders,
            COUNT(DISTINCT CASE WHEN n_ah_ingredients > 1 THEN subject_id END)
              AS n_spc_patients
     FROM #ah_exp;",
    label = "ah_totals")

  list(cohortId = spec$cohortId, perPatient = perPatient,
       byClass = byClass, totals = totals)
})
names(ahResults) <- as.character(cohortSpecs$cohortId)


# =============================================================================
# SECTION 4 -- Blood pressure at index
#
# The first systolic and the first diastolic reading on the index date. The
# standard measurement-value covariates look back over a window rather than
# pinning to the index date, which is why this is computed here.
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
     FROM #study_cohort c
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
# =============================================================================

covariateSettings <- FeatureExtraction::createCovariateSettings(
  useDemographicsGender = TRUE,
  useDemographicsAge = TRUE,
  useDemographicsAgeGroup = TRUE,
  useDemographicsRace = TRUE,
  useDemographicsEthnicity = TRUE,
  useDemographicsIndexYear = TRUE)

demographics <- lapply(cohortSpecs$cohortId, function(id) {
  message("Demographics for cohort ", id)
  # FeatureExtraction renders and translates its own SQL; it needs the same
  # temp-emulation schema this script uses, or it will look for the temp
  # tables somewhere else on platforms without real ones.
  # FeatureExtraction renamed cohortId -> cohortIds. Getting this wrong by
  # dropping the argument would silently characterize every cohort in the
  # table, so the name is resolved rather than guessed.
  fxArgs <- names(formals(FeatureExtraction::getDbCovariateData))
  cohortArg <- if ("cohortIds" %in% fxArgs) "cohortIds" else
    if ("cohortId" %in% fxArgs) "cohortId" else
      stop("FeatureExtraction::getDbCovariateData accepts neither cohortIds ",
           "nor cohortId; check the installed version.")
  covArgs <- list(connection = connection,
                  cdmDatabaseSchema = cdmDatabaseSchema,
                  cohortDatabaseSchema = cohortDatabaseSchema,
                  cohortTable = cohortTable,
                  covariateSettings = covariateSettings,
                  aggregated = TRUE)
  covArgs[[cohortArg]] <- id
  if ("tempEmulationSchema" %in% fxArgs) {
    covArgs$tempEmulationSchema <- tempSchema
  } else if ("oracleTempSchema" %in% fxArgs) {
    covArgs$oracleTempSchema <- tempSchema
  }
  covData <- do.call(FeatureExtraction::getDbCovariateData, covArgs)
  list(
    id = id,
    binary = as.data.frame(dplyr::collect(covData$covariates)),
    continuous = if ("covariatesContinuous" %in% names(covData))
      as.data.frame(dplyr::collect(covData$covariatesContinuous)) else
        data.frame(),
    ref = as.data.frame(dplyr::collect(covData$covariateRef)))
})
names(demographics) <- as.character(cohortSpecs$cohortId)


# =============================================================================
# SECTION 6 -- Assemble the tables
#
# Denominators, stated once because they differ by block:
#   Patients, N                            the cohort
#   Any antihypertensive                   the cohort
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

byCohort <- function(fn) {
  vapply(seq_len(nrow(cohortSpecs)),
         function(i) fn(cohortSpecs$cohortId[i]), character(1))
}
addRow <- function(rows, label, fn) {
  c(rows, list(c(list(Measure = label),
                 stats::setNames(as.list(byCohort(fn)), cohortSpecs$label))))
}
blankRow <- function(rows, label) {
  c(rows, list(c(list(Measure = label),
                 stats::setNames(as.list(rep("", nrow(cohortSpecs))),
                                 cohortSpecs$label))))
}

demoBinary <- function(id, pattern) {
  d <- demographics[[as.character(id)]]
  ids <- d$ref$covariateId[grepl(pattern, d$ref$covariateName, ignore.case = TRUE)]
  if (length(ids) == 0) return(NA_real_)
  sum(d$binary$sumValue[d$binary$covariateId %in% ids], na.rm = TRUE)
}
demoContinuous <- function(id, pattern, field) {
  d <- demographics[[as.character(id)]]
  if (nrow(d$continuous) == 0) return(NA_real_)
  ids <- d$ref$covariateId[grepl(pattern, d$ref$covariateName, ignore.case = TRUE)]
  row <- d$continuous[d$continuous$covariateId %in% ids, , drop = FALSE]
  if (nrow(row) == 0) return(NA_real_)
  as.numeric(row[[field]][1])
}
bpValue <- function(id, modality, field) {
  row <- bpSummary[bpSummary$cohort_definition_id == id &
                     bpSummary$modality == modality, , drop = FALSE]
  if (nrow(row) == 0) return(NA_real_)
  as.numeric(row[[field]][1])
}

# --- Table 1 -----------------------------------------------------------------
t1 <- list()
t1 <- addRow(t1, "Patients, N", function(id) fmtN(cohortSizes[[as.character(id)]]))
t1 <- addRow(t1, "Age at index, mean (SD), y", function(id) {
  fmtMeanSd(demoContinuous(id, "^age$|age in years", "averageValue"),
            demoContinuous(id, "^age$|age in years", "standardDeviation"))
})
t1 <- addRow(t1, "Female, n (%)", function(id) {
  fmtPct(demoBinary(id, "gender = FEMALE"), cohortSizes[[as.character(id)]])
})
t1 <- blankRow(t1, "Blood pressure at index")
t1 <- addRow(t1, "Systolic, mean (SD), mm Hg", function(id) {
  fmtMeanSd(bpValue(id, "sbp", "mean_value"), bpValue(id, "sbp", "sd_value"))
})
t1 <- addRow(t1, "Diastolic, mean (SD), mm Hg", function(id) {
  fmtMeanSd(bpValue(id, "dbp", "mean_value"), bpValue(id, "dbp", "sd_value"))
})
t1 <- addRow(t1, sprintf("Systolic %g mm Hg or greater, n (%%)", sbpThreshold),
             function(id) fmtPct(bpValue(id, "sbp", "n_above"),
                                 bpValue(id, "sbp", "n_measured")))
t1 <- addRow(t1, sprintf("Diastolic %g mm Hg or greater, n (%%)", dbpThreshold),
             function(id) fmtPct(bpValue(id, "dbp", "n_above"),
                                 bpValue(id, "dbp", "n_measured")))
t1 <- addRow(t1, "Patients with a reading at index, n (%)", function(id) {
  fmtPct(bpValue(id, "sbp", "n_measured"), cohortSizes[[as.character(id)]])
})

# Age bands and race, taken from the covariate reference so the table does not
# depend on FeatureExtraction's internal covariate ids.
refRows <- demographics[[1]]$ref

# Sum a set of covariate ids for one cohort.
demoIdSum <- function(id, covariateIds) {
  d <- demographics[[as.character(id)]]
  if (length(covariateIds) == 0) return(NA_real_)
  sum(d$binary$sumValue[d$binary$covariateId %in% covariateIds], na.rm = TRUE)
}

# --- Age bands ---------------------------------------------------------------
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
    message("Note: age band(s) below 18 present and folded into the first ",
            "band -- check the cohort is adults only: ",
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
      t1 <<- addRow(t1, paste0("    ", b$label), function(id) {
        fmtPct(demoIdSum(id, ids), cohortSizes[[as.character(id)]])
      })
    })
  }
}

# --- Race --------------------------------------------------------------------
raceRef <- refRows[grepl("^race", refRows$covariateName,
                         ignore.case = TRUE), , drop = FALSE]
if (nrow(raceRef) > 0) {
  groupOf <- rep(NA_character_, nrow(raceRef))
  for (g in names(raceGroups)) {
    hit <- is.na(groupOf) &
      grepl(raceGroups[[g]], raceRef$covariateName, ignore.case = TRUE)
    groupOf[hit] <- g
  }
  # Ungrouped race concepts keep their own row, under their own name.
  plain <- is.na(groupOf)
  groupOf[plain] <- sub("^race\\s*[:=]?\\s*", "", raceRef$covariateName[plain],
                        ignore.case = TRUE)

  t1 <- blankRow(t1, "Race, n (%)")
  # Largest first in the base population, so the table reads sensibly.
  order1 <- vapply(unique(groupOf), function(g) {
    sum(demoIdSum(cohortSpecs$cohortId[1],
                  raceRef$covariateId[groupOf == g]), na.rm = TRUE)
  }, numeric(1))
  for (g in names(sort(order1, decreasing = TRUE))) {
    local({
      ids <- raceRef$covariateId[groupOf == g]
      t1 <<- addRow(t1, paste0("    ", g), function(id) {
        fmtPct(demoIdSum(id, ids), cohortSizes[[as.character(id)]])
      })
    })
  }
}

table1 <- do.call(rbind, lapply(t1, as.data.frame, stringsAsFactors = FALSE))

# --- Table 2 -----------------------------------------------------------------
treatedOf <- function(id) as.numeric(ahResults[[as.character(id)]]$totals$n_treated[1])
recordsOf <- function(id) as.numeric(ahResults[[as.character(id)]]$totals$n_records[1])
regimenOf <- function(id, what) {
  pp <- ahResults[[as.character(id)]]$perPatient
  if (nrow(pp) == 0) return(0)
  switch(what,
         # every agent counted: monotherapy and >=2 partition the treated
         mono     = sum(pp$n_patients[pp$n_classes_all == 1]),
         two      = sum(pp$n_patients[pp$n_classes_all >= 2]),
         three    = sum(pp$n_patients[pp$n_classes_all >= 3]),
         spc      = sum(pp$n_patients[pp$any_spc == 1]),
         # combination components excluded, as previously published
         monoPub  = sum(pp$n_patients[pp$n_classes == 1]),
         twoPub   = sum(pp$n_patients[pp$n_classes >= 2]),
         threePub = sum(pp$n_patients[pp$n_classes >= 3]),
         stop("unknown regimen measure: ", what))
}
classRecordsOf <- function(id, keys) {
  bc <- ahResults[[as.character(id)]]$byClass
  sum(bc$n_records[bc$class_id %in% keys], na.rm = TRUE)
}
subclassRecordsOf <- function(id, sub) {
  bc <- ahResults[[as.character(id)]]$byClass
  sum(bc$n_records[bc$class_id == "OTHER" & bc$subclass_id == sub], na.rm = TRUE)
}
# Subclasses present anywhere in the run, so every cohort shows the same rows.
otherSubclasses <- sort(unique(unlist(lapply(ahResults, function(r) {
  bc <- r$byClass
  bc$subclass_id[bc$class_id == "OTHER" & nzchar(bc$subclass_id)]
}))))

t2 <- list()
t2 <- addRow(t2, "Patients, N", function(id) fmtN(cohortSizes[[as.character(id)]]))
t2 <- addRow(t2, "Any antihypertensive, n (%)",
             function(id) fmtPct(treatedOf(id), cohortSizes[[as.character(id)]]))
t2 <- blankRow(t2, "Among treated patients, n (%)")
t2 <- addRow(t2, "Monotherapy (1 class)",
             function(id) fmtPct(regimenOf(id, "mono"), treatedOf(id)))
t2 <- addRow(t2, "≥ 2 classes (includes ≥ 3)",
             function(id) fmtPct(regimenOf(id, "two"), treatedOf(id)))
t2 <- addRow(t2, "≥ 3 classes",
             function(id) fmtPct(regimenOf(id, "three"), treatedOf(id)))
t2 <- addRow(t2, "Single-pill combination",
             function(id) fmtPct(regimenOf(id, "spc"), treatedOf(id)))
# Kept so the previously published figures can be reconciled against this run.
# These exclude combination-product components from the class count, so a
# patient on a single-pill combination plus one other agent appears here as
# monotherapy, and patients on combinations alone appear in no row at all.
if (!countSpcComponentsInClasses) {
  t2 <- blankRow(t2, "Excluding combination components (as previously published)")
  t2 <- addRow(t2, "Monotherapy (1 class)",
               function(id) fmtPct(regimenOf(id, "monoPub"), treatedOf(id)))
  t2 <- addRow(t2, "≥ 2 classes (includes ≥ 3)",
               function(id) fmtPct(regimenOf(id, "twoPub"), treatedOf(id)))
  t2 <- addRow(t2, "≥ 3 classes",
               function(id) fmtPct(regimenOf(id, "threePub"), treatedOf(id)))
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
                  function(id) fmtPct(classRecordsOf(id, keys), recordsOf(id)))
  })
}

# The OTHER class is a catch-all, so it is broken out. These rows sum to the
# "Others" row above; they are a partition of it, not an addition to it.
for (sub in otherSubclasses) {
  local({
    key <- sub
    t2 <<- addRow(t2, paste0("    ", key),
                  function(id) fmtPct(subclassRecordsOf(id, key), recordsOf(id)))
  })
}

# Combined rows. A drug record belongs to exactly one class, so these are
# sums of the rows above, not unions of patient sets -- no overlap to remove.
t2 <- blankRow(t2, "Combined classes, n (%)")
t2 <- addRow(t2, "ACE inhibitor or ARB", function(id) {
  fmtPct(classRecordsOf(id, c("ACEI", "ARB")), recordsOf(id))
})
t2 <- addRow(t2, "ACE inhibitor, ARB, or CCB", function(id) {
  fmtPct(classRecordsOf(id, c("ACEI", "ARB", "DCCB", "NDCCB")), recordsOf(id))
})

table2 <- do.call(rbind, lapply(t2, as.data.frame, stringsAsFactors = FALSE))


# =============================================================================
# SECTION 7 -- Write output, and check the arithmetic
# =============================================================================

readr::write_csv(table1, file.path(outputFolder,
                                   paste0(databaseId, "_Table1_characteristics.csv")))
readr::write_csv(table2, file.path(outputFolder,
                                   paste0(databaseId, "_Table2_treatment.csv")))
readr::write_csv(cohortCounts, file.path(outputFolder,
                                         paste0(databaseId, "_cohort_counts.csv")))

# Checks worth failing on, rather than discovering in a reviewer's email.
checks <- do.call(rbind, lapply(cohortSpecs$cohortId, function(id) {
  classSum <- sum(ahResults[[as.character(id)]]$byClass$n_records, na.rm = TRUE)
  data.frame(
    cohortId = id,
    nSubjects = cohortSizes[[as.character(id)]],
    treated = treatedOf(id),
    # every agent counted, so these two must account for every treated patient
    monoPlusTwo = regimenOf(id, "mono") + regimenOf(id, "two"),
    unaccounted = treatedOf(id) - regimenOf(id, "mono") - regimenOf(id, "two"),
    # the published counting, kept for reconciliation only
    monoPlusTwoPublished = regimenOf(id, "monoPub") + regimenOf(id, "twoPub"),
    unaccountedPublished = treatedOf(id) - regimenOf(id, "monoPub") -
      regimenOf(id, "twoPub"),
    classRecords = classSum,
    allRecords = recordsOf(id),
    classShareOfRecords = round(100 * classSum / recordsOf(id), 1),
    stringsAsFactors = FALSE)
}))

# Monotherapy and two-or-more classes are mutually exclusive and exhaustive
# once every agent is counted, so anything other than zero here is a bug in
# this script, not a property of the data.
if (any(checks$unaccounted != 0)) {
  stop("Regimen rows do not account for every treated patient in cohort(s): ",
       paste(checks$cohortId[checks$unaccounted != 0], collapse = ", "),
       ". See ", databaseId, "_arithmetic_checks.csv")
}
readr::write_csv(checks, file.path(outputFolder,
                                   paste0(databaseId, "_arithmetic_checks.csv")))

if (!countSpcComponentsInClasses && any(checks$unaccountedPublished > 0)) {
  message("Note: under the published counting, ",
          sum(checks$unaccountedPublished), " treated patients fall into ",
          "neither regimen row because their only antihypertensive is a ",
          "combination product. The rows above the reconciliation block count ",
          "every agent and do account for all of them.")
}
if (any(checks$classShareOfRecords < 99.9)) {
  message("Note: drug classes account for ",
          paste(checks$classShareOfRecords, collapse = "%, "),
          "% of antihypertensive records, so the class rows do not sum to ",
          "100%. The remainder is single-pill combination records.")
}

message("\nDone. Files written to ", outputFolder)
print(table1, right = FALSE)
print(table2, right = FALSE)
