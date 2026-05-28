# SNAP QC Error Viewer

🔗 **Live app:** [https://bettergovernmentlab.shinyapps.io/snap-qc-shiny/](https://bettergovernmentlab.shinyapps.io/snap-qc-shiny/)

## Overview

The **SNAP QC Error Viewer** is an interactive R Shiny dashboard for exploring
and analyzing **Quality Control (QC) errors** in the U.S. Supplemental Nutrition
Assistance Program (SNAP). It is built on a **nationally representative sample
of SNAP cases** that are reviewed each fiscal year by **state and federal
auditors** to verify whether participating households received the correct
benefit amount.

The dashboard turns the raw QC data — which is technical, hierarchical, and
hard to navigate in spreadsheet form — into a set of focused visual and
tabular views. It helps users answer questions such as:

- **What** is going wrong in SNAP cases (which program elements have errors)?
- **How** are those errors happening (the nature of the mistake)?
- **Why** are they happening (client, agency, or technical responsibility)?
- **Who** is most affected (which household and case characteristics)?
- **How costly** are different categories of errors (under- vs. overissuance,
  average dollar amount, frequency)?
- **How prevalent** are errors across states, years, and case types?

The tool covers fiscal years **2017 through 2024** and applies the **annual
QC error-rate threshold** (the per-year dollar cutoff used by USDA-FNS to
distinguish a reportable payment error from a small variance) so that users
can choose to focus on the cases that count toward official error rates.

This tool is intended for researchers, policymakers, program administrators,
journalists, and analysts interested in SNAP operations, administrative
burden, payment accuracy, and program integrity.

> **Caveat.** The QC database is a *sample* of reviewed cases, not the full
> population of SNAP participants. Differences across states may reflect
> variation in policy implementation and reporting practices and should not
> be read as direct comparisons of program performance. The data also exclude
> cases later found to be ineligible, which tend to involve larger errors.
> Interpret results with appropriate caution.

## Application Tabs

The app is organized into five tabs, each answering a different question
about SNAP QC errors. Every tab supports filtering by **year(s)**, **state**,
and (where applicable) **error status** (overissuance vs. underissuance), and
includes an **"Only include cases above yearly error-rate threshold"** toggle
so users can restrict the view to officially reportable errors.

### 1. Error Pathways

An interactive **Sankey diagram** that traces how errors flow from *what
went wrong* to *how it happened* to *why it happened*.

- Choose one of four flow combinations:
  - `Element → Nature → Type` (full three-stage view)
  - `Element → Nature`
  - `Element → Type`
  - `Nature → Type`
- Switch the metric the link width represents:
  - **Occurrences** — number of case-error records, or
  - **Dollar amount** — total dollars in error.
- Filter by specific **Error Element**, **Nature**, or **Type** values, by
  state, by error status, and by a minimum-flow threshold (to hide noise).

This view is best for understanding *patterns and sequences* — e.g., which
program elements most often lead to client-driven vs. agency-driven errors.

### 2. Error Demographics

A **comparative bar chart** showing how a chosen error breaks down across
household demographic and case characteristics.

- Pick an **Error Category** (Type, Nature, or Element) and a specific value
  within it.
- Pick a **Demographic** to break the cases down by:
  - Employment, Race, Gender, Age
  - Disability (Household Head / Household Member)
  - Unit Composition, Homelessness
  - Expedited Service, Application or Renewal
  - Status of Error Findings
- Optionally **include "Other Errors" as a comparison series** to see whether
  the demographic pattern is specific to the chosen error or reflects a more
  general distribution across all errors.

This view is best for understanding *who is affected* by a particular kind
of error.

### 3. Error Severity

A **dot plot** that quantifies the financial impact of each error category.

- Group errors by **Error Type**, **Error Nature**, or **Error Element**.
- Restrict to **Client**, **Agency**, or **Technical** error responsibility.
- Sort by **Average Dollar Amount** or **Number of Cases**.
- Show the top **5 / 10 / 20** categories.
- Each point's **size** reflects how many unique cases involved that error;
  its **position** reflects the average dollar amount in error.
- A summary line reports the total dollar impact for the current selection,
  and the underlying data can be exported via **Download Data**.

This view is best for understanding *which errors are most costly* per case
and in aggregate.

### 4. Pivot Table

A **flexible cross-tabulation** built on `pivottabler`, similar to a
spreadsheet pivot table.

- Choose any subset of dimensions to **filter on** (Error Responsibility,
  Type, Nature, Element, Discovery, Timing, Status of Error Findings, Case
  Type, Year, State). Selecting a dimension reveals a multi-select picker
  with all of its values.
- Choose any dimension as the **row** and any other as the **column**.
- The body of the table reports counts of case–error combinations.
- A note clarifies that each row in the underlying dataset is a unique
  case–error combination, so a single case may appear more than once if it
  involves multiple errors.

This view is best for *ad-hoc exploration* when the predefined plots don't
match the question being asked.

### 5. Base Rates

A **state-level base-rate analysis** that puts error counts in the context
of the number of cases reviewed.

- **Filter** the underlying QC sample on any combination of dimensions.
- **Break down base rates** by State, Year, Case Type, Status of Error
  Findings, Error Type, Nature, or Element.
- Two output views:
  - **Base error rates** — a downloadable table with reviewed-case counts,
    error counts, and computed error rates for the chosen breakdown.
  - **Simple view (State × Error Nature)** — a wide pivot with states as
    rows, error natures as columns, and the percentage of total errors as
    cell values; horizontally scrollable and downloadable as CSV.

This view is best for *benchmarking* — comparing error prevalence across
states, years, and case types using a consistent denominator.

## Data

The app ships with pre-processed `.RDS` files in `data/`, derived from the
publicly available SNAP Quality Control microdata.

- **Raw files (not included in this repo):** [https://snapqcdata.net/datafiles](https://snapqcdata.net/datafiles)
- **USDA-FNS QC Database:** [https://www.fns.usda.gov/snap/qc/database](https://www.fns.usda.gov/snap/qc/database)

The app uses three families of pre-aggregated tables, one per year for
2017–2024:

| File pattern                       | Purpose                                               |
| ---------------------------------- | ----------------------------------------------------- |
| `pivot_table_<year>.RDS`           | Case–error rows for the Pivot Table and Base Rates   |
| `snap_error_<year>.RDS`            | Case–error rows for the Sankey and Severity views    |
| `snap_demographics_<year>.RDS`     | Case-level demographics for the Demographics view    |
| `base_case_2017_2024.rds`          | Case-level denominator (reviewed cases) for base rates |
| `base_cat_2017_2024.rds`           | Categorical error-dimension table for base rates     |

Each year is tagged with the corresponding **federal QC error-rate threshold**
(the per-year dollar cutoff used by FNS to flag a case as having a reportable
payment error):

| Year | Threshold (USD) |
| ---- | --------------- |
| 2017 | 38              |
| 2018 | 37              |
| 2019 | 37              |
| 2020 | 37              |
| 2021 | 39              |
| 2022 | 48              |
| 2023 | 54              |
| 2024 | 56              |

When the **"Only include cases above yearly error-rate threshold"** checkbox
is enabled (default on every tab), the view restricts to case–errors whose
dollar amount exceeds that year's threshold.

## Running Locally

### Requirements

- R (>= 4.1 recommended)
- The following R packages:
  `shiny`, `shinyBS`, `bslib`, `shinyWidgets`, `jsonlite`, `scales`,
  `plotly`, `pivottabler`, `tidyverse`, `DT`

Install with:

```r
install.packages(c(
  "shiny", "shinyBS", "bslib", "shinyWidgets", "jsonlite", "scales",
  "plotly", "pivottabler", "tidyverse", "DT"
))
```

### Launch

From the project root:

```r
shiny::runApp()
```

or open `app.R` in RStudio and click **Run App**.

## Repository Structure

```
snap-qc-shiny/
├── app.R                              # Single-file Shiny app (UI + server)
├── data/                              # Pre-processed QC datasets, 2017–2024
├── www/                               # Static assets (logos)
├── rsconnect/                         # shinyapps.io deployment metadata
├── README.md
└── LICENSE
```

## Credits

Developed by **Zhaowen Guo** and **Xiao Xu** at the
[**Better Government Lab**](https://www.bettergovernmentlab.org/resources/snap-quality-control-error-viewer),
McCourt School of Public Policy, **Georgetown University**.

Data source: USDA Food and Nutrition Service —
[SNAP Quality Control Database](https://www.fns.usda.gov/snap/qc/database).

For illustrative examples of how Error Element, Nature, and Type combine to
form an error scenario, see the
[reference sheet](https://docs.google.com/spreadsheets/d/1zLF0h30Ic2Y9cv4sRDV3UTuInR-QgeN9RWrvRA0bT3k/edit?usp=sharing).

## License

See [LICENSE](LICENSE).
