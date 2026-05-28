# 2. Data Sources

This document covers **where the data comes from**, the upstream pipeline,
and the relationship between the public source files and the `.RDS` files
shipped inside `data/`.

---

## 2.1 Upstream sources

### Primary source — USDA Food and Nutrition Service (FNS)

USDA-FNS publishes the SNAP Quality Control (QC) Database every fiscal year.
Each annual release contains the case-level records that state and federal
QC reviewers compiled from a nationally representative sample of active
SNAP cases.

- Landing page:
  [`https://www.fns.usda.gov/snap/qc/database`](https://www.fns.usda.gov/snap/qc/database)
- Format: SAS transport files (`.xpt`) plus codebooks (PDFs).
- Coverage in this app: federal fiscal years **FFY 2017 – FFY 2024**.

A **federal fiscal year** runs from October 1 of the previous calendar year
through September 30 of the year named — so "FFY 2024" covers
2023-10-01 through 2024-09-30. Year labels in every dataset (`year`,
`Year`) refer to the **federal fiscal year**, not the calendar year.

### Convenience mirror

A community mirror of the same files in friendlier formats is hosted at:

- [`https://snapqcdata.net/datafiles`](https://snapqcdata.net/datafiles)

If FNS reorganizes the source URLs, the mirror is usually a faster way to
re-download an old release.

### What is NOT in this repository

The raw SAS files are **not** committed to this repository. Only the
pre-aggregated `.RDS` files used by the app live in `data/`. If you need to
rebuild the `.RDS` files from scratch — for example to add FFY 2025 — see
[`09-maintenance.md`](09-maintenance.md), section "Adding a new data year."

---

## 2.2 The two-table model used by the app

The app distinguishes two kinds of rows:

1. **Case rows** — one row per *reviewed case*. This is the *denominator*
   for any rate or share calculation. Lives in
   `data/base_case_2017_2024.rds`.
2. **Case–error rows** — one row per *(case × error finding)* pair. A
   single reviewed case appears here zero or more times depending on how
   many distinct errors the auditor recorded. This is the *numerator* for
   most counts and the only source of the Element / Nature / Type / Dollar
   amount fields. Lives in `data/base_cat_2017_2024.rds` and the per-year
   files (`pivot_table_<year>.RDS`, `snap_error_<year>.RDS`).

Demographics (`snap_demographics_<year>.RDS`) are also keyed at the
case–error grain because the bar-chart tab needs the demographic of the
case repeated alongside each error finding it had.

---

## 2.3 The yearly QC error-rate threshold

Every fiscal year, USDA-FNS publishes a **dollar threshold** below which a
detected payment variance is **not** counted as an official "error" for
purposes of the federal payment error rate. The Viewer applies this
threshold via the **"Only include cases above yearly error-rate threshold"**
checkbox that appears on every tab (defaulted to ON).

The thresholds are hard-coded in `app.R` at line 11:

| Federal Fiscal Year | Threshold (USD) |
| ------------------- | --------------- |
| 2017                | 38              |
| 2018                | 37              |
| 2019                | 37              |
| 2020                | 37              |
| 2021                | 39              |
| 2022                | 48              |
| 2023                | 54              |
| 2024                | 56              |

**Source for the thresholds:** the USDA-FNS QC technical documentation /
annual error-rate publications. When you add a new year you must look up
the new threshold from the FNS error-rate release for that year and add
it to `threshold_by_year`. See [`09-maintenance.md`](09-maintenance.md).

Internally, every dataset that has a `Dollar Amount in Error` column is
augmented at load time with two derived columns:

```r
threshold      = threshold_by_year[as.character(Year)]
over_threshold = if_else(Dollar Amount in Error > threshold, 1L, 0L)
```

The threshold checkbox in each tab simply filters on `over_threshold == 1`.

---

## 2.4 Pipeline (raw → RDS)

The pipeline that turns USDA-FNS SAS files into the `.RDS` files in `data/`
is **maintained outside this repository**. The app itself never re-runs it.
At a high level:

```
Step 1.  Download annual SAS files (.xpt) from USDA-FNS or snapqcdata.net.

Step 2.  In a separate R/Python environment:
           a. Decode SAS variables using the codebook to map numeric
              codes to human-readable labels (e.g. "ELEM" -> "Wages and
              salaries").
           b. Reshape into the long "case-error" form, one row per
              (case_id, error finding).
           c. Roll up the Type column into Client / Agency / Technical
              error responsibility (same mapping as type_to_responsibility
              in app.R lines 134-152).
           d. Build the case-level denominator (base_case_*).
           e. Build the case-error file (base_cat_*) for the Base Rates
              tab.
           f. Build the per-year case-error files for the Sankey,
              Severity, and Pivot Table tabs.
           g. Build the per-year demographics file by joining household
              characteristics onto the case-error rows.

Step 3.  Save each frame with saveRDS() into data/ using the file names
         the app expects (see 03-data-files.md).
```

If/when the ETL scripts are recovered or rewritten, store them under
`scripts/etl/` and document them in [`09-maintenance.md`](09-maintenance.md).

---

## 2.5 Data freshness and update cadence

USDA-FNS typically publishes a new fiscal-year release roughly **9–15 months
after the close of the fiscal year** (e.g., FFY 2024 became available in
2025–2026). To keep the dashboard current, plan for an annual refresh:

1. Add the new year's threshold to `threshold_by_year`.
2. Generate the new year's `pivot_table_<year>.RDS`,
   `snap_error_<year>.RDS`, and `snap_demographics_<year>.RDS`.
3. Regenerate `base_case_2017_2024.rds` and `base_cat_2017_2024.rds` so
   they include the new year (the date range in their filenames should
   also be bumped, and the names updated everywhere they are read in
   `app.R`).
4. Add the new year to the `bind_rows()` blocks in `app.R` (search for the
   year `2024` to find every site that lists years explicitly).
5. Re-deploy.

Detailed steps are in [`09-maintenance.md`](09-maintenance.md).

---

## 2.6 Reference sheet for analysts

For illustrative examples of how Error Element, Nature, and Type combine to
form a real-world error scenario, see the public reference sheet maintained
by the authors:

- [Error scenarios reference sheet (Google Sheets)](https://docs.google.com/spreadsheets/d/1zLF0h30Ic2Y9cv4sRDV3UTuInR-QgeN9RWrvRA0bT3k/edit?usp=sharing)

This sheet is also linked from the footer of the live app. It is the
single best resource for understanding *what a row in the data actually
means* in plain language.
