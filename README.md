# CAPTUR-HTN — ExecuteAnalyses.R

Edit SECTION 0 to specify your OMOP DB connection details before running the script.

```
Rscript ExecuteAnalyses.R
```

## Files

```
ExecuteAnalyses.R                 the full analysis, cohort generation through characterization
Cohorts.csv                       cohort_id, cohort_name
cohorts/<id>.json                 ATLAS definitions (1322, 1332, 1348)
sql/sql_server/<id>.sql           ATLAS SQL
AntihypertensiveIngredients.csv   ingredient -> drug class, 56 ingredients
TestTableAssembly.R               offline check of the Table 2 arithmetic
```

## Output

Written to `outputFolder`:

```
<db>_Table1_characteristics.csv
<db>_Table2_treatment.csv
<db>_cohort_counts.csv
<db>_arithmetic_checks.csv
log.txt
```

## Cohort definitions

| Id | Cohort | Role |
|---|---|---|
| 1322 | [CAPTUR] 3YR Longitudinal Ambulatory | base population |
| 1348 | [CAPTUR] HTN New Cases | newly identified, index visit |
| 1332 | [CAPTUR] HTN All Cases | overall, index and prior 12 mo |

**Drug window**, per cohort in `cohortSpecs`. Newly identified uses the index
visit `(0, 1)`; overall and the base population use index and the prior 12
months `(-365, 0)`.
