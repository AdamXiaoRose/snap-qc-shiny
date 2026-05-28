# 1. Project Overview

## What this app is

The **SNAP QC Error Viewer** is an interactive R Shiny dashboard that lets
users explore **Quality Control (QC) errors** in the U.S. Supplemental
Nutrition Assistance Program (SNAP). The data comes from the
**SNAP QC sample** — a nationally representative sample of SNAP cases that
state and federal auditors review each fiscal year to verify whether the
participating household received the correct benefit amount.

The dashboard turns the raw QC database — which is technical, hierarchical,
and hard to navigate in spreadsheet form — into a small set of focused visual
and tabular views. It is meant to help researchers, policymakers, journalists,
and program staff answer questions such as:

- **What** is going wrong in SNAP cases (which program elements have errors)?
- **How** are those errors happening (the nature of the mistake)?
- **Why** are they happening (client, agency, or technical responsibility)?
- **Who** is most affected (which household and case characteristics)?
- **How costly** are different categories of errors (under- vs. overissuance,
  average dollar amount, frequency)?
- **How prevalent** are errors across states, years, and case types?

## Coverage

- **Years:** Federal Fiscal Years (FFY) **2017 through 2024**.
- **Geography:** all 50 states + the District of Columbia + 2 territories
  (53 jurisdictions in total appear in the data).
- **Population:** *reviewed* SNAP cases (not the full SNAP participating
  population). The QC database is a *sample*; treat all numbers as
  representative of reviewed cases, not as direct counts of all SNAP
  households.

## The five tabs

The app is organized into five top-level tabs. They share the same global
filters (year, state, error status, threshold toggle) but each answers a
different question.

| Tab | Question it answers | Visual |
|---|---|---|
| **Error Pathways** | Which combinations of *what went wrong → how → why* are most common? | Sankey diagram |
| **Error Demographics** | Who is affected by a particular error category? | Grouped bar chart |
| **Error Severity** | Which errors are most costly per case (or most frequent)? | Dot plot (size = case count, y = avg $) |
| **Pivot Table** | Free-form ad-hoc cross-tab over any two dimensions. | `pivottabler` cross-tab |
| **Base Rates** | What share of *reviewed cases* have a particular error, by state / year / case type? | DT data tables (long + wide) |

Every tab has a default-on checkbox **"Only include cases above yearly
error-rate threshold."** When checked (the default), the view restricts to
case–error rows whose dollar amount exceeds that fiscal year's federal
QC error-rate threshold. This is the cutoff USDA-FNS uses to distinguish a
*reportable payment error* from a small variance.

## Architecture in one diagram

```
                ┌──────────────────────────────────────────────────┐
                │   Public source: USDA-FNS SNAP QC microdata      │
                │   (SAS files, redistributed at snapqcdata.net)   │
                └────────────────────────┬─────────────────────────┘
                                         │  (offline ETL, not in this repo)
                                         ▼
            ┌──────────────────────────────────────────────────────┐
            │ data/  — pre-aggregated .RDS files, 1 per year       │
            │   pivot_table_<year>.RDS      (case–error rows)      │
            │   snap_error_<year>.RDS       (case–error rows)      │
            │   snap_demographics_<year>.RDS (case–error + demog)  │
            │   base_case_2017_2024.rds     (one row per reviewed  │
            │                                 case — denominator)  │
            │   base_cat_2017_2024.rds      (case × error-category │
            │                                 — numerator)         │
            └────────────────────────┬─────────────────────────────┘
                                     ▼
                    ┌────────────────────────────────┐
                    │  app.R   (UI + server, single  │
                    │           file Shiny app)      │
                    └────────────────┬───────────────┘
                                     ▼
                    ┌────────────────────────────────┐
                    │ shinyapps.io                   │
                    │   bettergovernmentlab/         │
                    │   snap-qc-shiny                │
                    └────────────────────────────────┘
```

Key points:

- **Single-file app.** UI and server are both defined in `app.R`. There are
  no R modules, no `global.R`, no separate `ui.R`/`server.R`.
- **Pre-aggregated data ships with the app.** The Shiny runtime never reads
  the raw SAS microdata. It loads the `.RDS` files at startup, binds them
  in memory, then reacts to user input. This keeps cold-start fast and the
  shinyapps.io memory footprint manageable.
- **Two deployment targets exist** in `rsconnect/`:
  - `bettergovernmentlab/snap-qc-shiny` — the **production** target.
  - `xiao-dataviz/snap-qc-shiny` — a personal/staging target.

  See [`08-deployment.md`](08-deployment.md) for which one to use.

## Vocabulary you must know before reading the code

The QC database has its own three-way taxonomy of errors. The app refers to
these everywhere and the code uses them as column names. Memorize the
distinction:

| Term | Question it answers | Examples |
|---|---|---|
| **Error Element** | *What* went wrong (which program element). | "Wages and salaries", "Shelter deduction", "Citizenship and noncitizen status". |
| **Error Nature** | *How* it went wrong (the nature of the mistake). | "Unreported source of income", "Incorrect standard used", "Eligible person(s) excluded". |
| **Error Type** | *Why* it went wrong (the underlying cause / responsibility). | "Information not reported by client", "Policy incorrectly applied", "Computer programming error". |
| **Error Responsibility** | A coarse rollup of Type into three buckets: **Client**, **Agency**, or **Technical**. | The mapping is hard-coded as `type_to_responsibility` in `app.R` (lines 134–152). |
| **Status of Error Findings** | Whether the case was over-issued, under-issued, or correct. | "Overissuance", "Underissuance", "Amount correct". |
| **Error Discovery** | How the auditor discovered the error. | e.g., "From case record (verification from an automated match)". |
| **Error Timing** | When the underlying event occurred relative to the agency's most recent action. | e.g., "At time of most recent action by agency", "After most recent action by agency". |
| **Case Type / Action Type** | Whether this case was a new application, a recertification, etc. | "Certification", "Recertification". |
| **Threshold (yearly QC error-rate threshold)** | The per-FFY dollar cutoff used by USDA-FNS to flag a case as having a *reportable* payment error. | 2017: $38; 2018–2020: $37; 2021: $39; 2022: $48; 2023: $54; 2024: $56. Hard-coded in `threshold_by_year` at `app.R:11`. |

A single reviewed case can carry **multiple** error rows — one for each
distinct error finding. That is why the case-level data (`base_case_*`) is
separate from the case–error data (`pivot_table_*`, `snap_error_*`,
`base_cat_*`). The denominator is *cases*; the numerator depends on the
question and may be *cases with at least one error of type X* or
*case–error rows*.

## Caveats users should be aware of

These are reproduced verbatim in the app's banner text, but they are
load-bearing for honest interpretation:

> The QC database is a **sample** of reviewed cases, not the full population
> of SNAP participants. Differences across states may reflect variation in
> policy implementation and reporting practices and should not be read as
> direct comparisons of program performance. The data also exclude cases
> later found to be ineligible, which tend to involve larger errors.
> Interpret results with appropriate caution.

Any future tab you add should preserve this framing — for example, never
report error counts as percentages of "all SNAP households," only as
percentages of *reviewed cases*.

## Credits

Developed by **Zhaowen Guo** and **Xiao Xu** at the
[**Better Government Lab**](https://www.bettergovernmentlab.org/resources/snap-quality-control-error-viewer),
McCourt School of Public Policy, **Georgetown University**.

Data source: USDA Food and Nutrition Service —
[SNAP Quality Control Database](https://www.fns.usda.gov/snap/qc/database).

License: MIT (see [`../LICENSE`](../LICENSE)).
