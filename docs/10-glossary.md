# 10. Glossary

Definitions of SNAP and QC terminology used throughout the app and the
documentation.

---

| Term | Definition |
|---|---|
| **SNAP** | Supplemental Nutrition Assistance Program — the federal nutrition assistance program (formerly "food stamps") administered by USDA-FNS. |
| **USDA-FNS** | United States Department of Agriculture — Food and Nutrition Service. The federal agency that administers SNAP and publishes the QC database. |
| **QC (Quality Control)** | The federal program that monitors SNAP payment accuracy by reviewing a nationally representative sample of cases each fiscal year. State agencies sample and review cases; FNS reviews a subset of those reviews. |
| **QC database** | The annual public-use file released by USDA-FNS containing the case-level findings from QC reviews. The source data behind this app. |
| **Federal Fiscal Year (FFY)** | October 1 of the previous calendar year through September 30 of the year named. "FFY 2024" = 2023-10-01 through 2024-09-30. All `year`/`Year` columns in the data are FFY. |
| **Reviewed case** | A SNAP case selected for QC review during a given FFY. The denominator for any base-rate calculation. |
| **Case–error row** | A single error finding attached to a single reviewed case. A case may have zero, one, or many error rows. The grain of `pivot_table_*`, `snap_error_*`, and `base_cat_*`. |
| **Payment error** | A discrepancy between the benefit a household received and the benefit they were entitled to under program rules. Can be over-issuance (received too much) or under-issuance (received too little). |
| **Yearly QC error-rate threshold** | The per-fiscal-year dollar cutoff used by USDA-FNS to flag a case as having a *reportable* payment error. Variances at or below the threshold are not counted in the official payment error rate. Hard-coded in `threshold_by_year` (`app.R:11`). |
| **Error Element** | The program element in which the error occurred — *what* went wrong. Examples: "Wages and salaries", "Shelter deduction", "Citizenship and noncitizen status". |
| **Error Nature** | The nature of the mistake — *how* the error happened. Examples: "Unreported source of income", "Incorrect standard used", "Eligible person(s) excluded". |
| **Error Type** | The underlying cause / source of responsibility — *why* the error happened. Examples: "Information not reported by client" (Client), "Policy incorrectly applied" (Agency), "Computer programming error" (Technical). |
| **Error Responsibility** | A coarse rollup of Error Type into three buckets: **Client Errors**, **Agency Errors**, **Technical Errors**. The mapping is hard-coded in `type_to_responsibility` (`app.R:134–152`). |
| **Status of Error Findings** | The outcome of the case review: `"Underissuance"` (household received less than they should have), `"Overissuance"` (received more), `"Amount correct"` (no error). |
| **Error Discovery** | The mechanism through which the auditor discovered the error (e.g., from an automated match, from information provided by the recipient, etc.). |
| **Error Timing** | When the underlying event that caused the error occurred relative to the agency's most recent action on the case. |
| **Case Type / Action Type** | The type of casework being audited: `"Certification"` (initial application) or `"Recertification"` (periodic renewal). |
| **Certification** | The initial determination of eligibility and benefit amount for a SNAP case. |
| **Recertification** | The periodic review (typically every 6–12 months) that re-determines a case's eligibility and benefit amount. |
| **IPV (Intentional Program Violation)** | A finding that a participant intentionally provided false information or otherwise broke program rules. One of the Client Error categories ("Information withheld by client (case referred for IPV investigation)"). |
| **TANF, PA, GA** | Temporary Assistance for Needy Families; Public Assistance; General Assistance. Other programs whose benefits sometimes appear in a SNAP household's income calculation. |
| **RSDI** | Retirement, Survivors, and Disability Insurance — the Social Security retirement / survivor / disability benefits, treated as unearned income for SNAP. |
| **SSI** | Supplemental Security Income — federal income support for low-income aged, blind, or disabled individuals. |
| **Expedited Service** | The accelerated SNAP application process available to households in immediate need; benefits must be issued within 7 days. The "Expedited Service" demographic column tells you whether the case was entitled to expedited service. |
| **Unit Composition** | A categorical description of the household composition (e.g., "No children", "Child(ren) and one female adult"). |
| **Sankey diagram** | A flow diagram in which the width of arrows is proportional to the flow quantity. Used in the **Error Pathways** tab to show how error counts (or dollars) flow from Element → Nature → Type. |
| **Pivot Table** | A spreadsheet-style cross-tabulation. Rendered using the [`pivottabler`](https://cran.r-project.org/package=pivottabler) R package on the **Pivot Table** tab. |
| **Base rate** | The share of *reviewed cases* that meet a condition (e.g., "had an error of type X"). The numerator and denominator are *cases*, not case–error rows. Computed on the **Base Rates** tab. |
| **shinyapps.io** | The hosted Shiny service operated by Posit (formerly RStudio). The production deployment lives on this platform under the `bettergovernmentlab` account. |
| **`rsconnect`** | The R package used to deploy Shiny apps to shinyapps.io / Posit Connect. Configuration files for deployment live in `rsconnect/`. |
