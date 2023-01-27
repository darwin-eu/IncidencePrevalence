/***********************************
File cohortsTestDb.sql
***********************************/
IF OBJECT_ID('@resultsDatabaseSchema.cohort', 'U') IS NOT NULL
  DROP TABLE @resultsDatabaseSchema.cohort;

SELECT first_use.*
INTO @resultsDatabaseSchema.cohort
FROM (
  SELECT drug_concept_id AS cohort_definition_id,
  MIN(drug_era_start_date) AS cohort_start_date,
  MIN(drug_era_end_date) AS cohort_end_date,
  person_id AS subject_id
  FROM @cdmDatabaseSchema.drug_era
    WHERE drug_concept_id = 19054876
  GROUP BY drug_concept_id, person_id) first_use
INNER JOIN @cdmDatabaseSchema.observation_period
  ON first_use.subject_id = observation_period.person_id
  AND cohort_start_date >= observation_period_start_date
  AND cohort_end_date <= observation_period_end_date;

UPDATE @resultsDatabaseSchema.cohort set cohort_definition_id = 1;
ALTER TABLE @resultsDatabaseSchema.cohort ALTER COLUMN cohort_start_date date;
ALTER TABLE @resultsDatabaseSchema.cohort ALTER COLUMN cohort_end_date date;
