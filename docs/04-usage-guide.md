# 4. Usage Guide

This document walks through every tab of the live app — every control,
every default, and how filters interact. It serves two purposes:

1. **As a help page** for new users (or for embedding into the
   bettergovernmentlab.org page).
2. **As a behavior contract** for future code changes — if a control
   here disagrees with the deployed app, one of them is wrong.

Refer to the live deployment as you read:
[https://bettergovernmentlab.shinyapps.io/snap-qc-shiny/](https://bettergovernmentlab.shinyapps.io/snap-qc-shiny/)

---

## 4.0 Global behavior shared by all tabs

### The yearly threshold checkbox

Every tab has a checkbox labeled **"Only include cases above yearly
error-rate threshold."** It is **on by default**. When checked, the view
restricts to case–error rows whose `Dollar Amount in Error` exceeds that
fiscal year's federal QC error-rate threshold (see
[`02-data-sources.md`](02-data-sources.md), section 2.3).

Use cases:

- **Checked (default):** matches what USDA-FNS would treat as a
  *reportable* payment error. Recommended for public-facing or
  policy-relevant analysis.
- **Unchecked:** includes all variances no matter how small; useful for
  understanding patterns of agency activity, even ones too small to count
  toward the official error rate.

### Year selectors

Every tab lets you pick one or more fiscal years. Defaults differ:

- **Error Pathways, Error Demographics, Error Severity:** all 8 years
  (2017–2024) selected.
- **Pivot Table, Base Rates:** the year filter is opt-in via the "Choose
  filters" picker.

Multi-select aggregates across the chosen years.

### State selectors

Every tab includes `"All States"` as the first option (the default). When
combined with multi-state selection (Demographics tab), `"All States"`
**adds** an aggregated panel alongside the per-state panels rather than
replacing them.

### Status of Error Findings

A three-way filter (`All` / `Underissuance` / `Overissuance`) appears on
the Error Pathways and Error Severity tabs. The Pivot Table and Base Rates
tabs expose Status of Error Findings via the generic "Choose filters"
mechanism instead.

---

## 4.1 Tab 1 — Error Pathways (Sankey diagram)

A Sankey diagram that traces how errors flow from *what went wrong* to
*how it happened* to *why it happened*.

### Controls (sidebar, left)

| Control | Choices | Default | Notes |
|---|---|---|---|
| **Select Year(s)** | 2017–2024, multi-select | all 8 years | Aggregates if more than one year is picked. |
| **Select Error Pathway to Visualize** | `Element → Nature → Type` (3-stage), `Element → Nature`, `Element → Type`, `Nature → Type` | `Element → Nature → Type` | Switching this changes which side filters appear below. |
| **Flow by** | `Occurrences` (count of case–error rows) / `Dollar amount` (sum of \|dollars\|) | `Occurrences` | Changes the link width metric AND the threshold input units. |
| **(Dynamic) Element / Nature / Type filters** | grouped multi-select with "Select All" | "Select All" | Generated based on the current pathway choice. |
| **Select State** | `"All States"` + 53 states | `"All States"` | Single-select. |
| **Filter flows ≥ (occurrences/dollars)** | numeric | 1000 (count) or 10000 (dollars) | Hides links smaller than this — i.e. removes visual noise. Re-renders when you change the metric. |
| **Filter by Error Status** | All / Underissuance / Overissuance | All | |
| **Only include cases above yearly error-rate threshold** | checkbox | ON | |

### Reading the diagram

- **Left column (red-pink):** Error Element — *what* went wrong.
- **Middle column (purple):** Error Nature — *how* it happened. Only
  appears in the 3-stage pathway.
- **Right column (gold):** Error Type — *why* it happened (Client / Agency
  / Technical when rolled up).
- **Link width:** number of case–errors OR total dollar value, depending
  on the "Flow by" radio.
- **Hover** over a node to see its total; over a link to see source →
  target → value.

### Common questions

- *Why are some categories missing?* They were filtered out by the
  minimum-flow threshold. Lower the value or switch from dollars to
  occurrences.
- *Why does the diagram look empty?* Either the year × state combination
  has no rows, OR the threshold is set too high. The plot will print a
  message such as "No flows pass the threshold."

---

## 4.2 Tab 2 — Error Demographics (bar chart)

A grouped bar chart showing how the cases tied to a chosen error category
break down across a household demographic.

### Controls

| Control | Choices | Default | Notes |
|---|---|---|---|
| **Select Year(s)** | 2017–2024, multi-select | all 8 years | |
| **Select Error Category** | `Type` / `Nature` / `Element` | `Type` | Radio. Drives the dropdown below. |
| **Select Error Type/Nature/Element** | grouped picker matching the category | varies (e.g., `"Policy incorrectly applied"`) | The choice list is *static* — see `app.R:1219–1378`. |
| **Select State(s)** | `"All States"` + 53 states, multi-select | `"All States"` | If you pick `"All States"` together with one or more individual states, you get a faceted plot with a separate panel per state plus an aggregated `"All States"` panel. |
| **Only include cases above yearly error-rate threshold** | checkbox | ON | Uses the case-level `over_threshold` flag built from `df_error`. |
| **Select Demographic** | Employment, Race, Gender, Age, Disability (Household Head), Disability (Household Member), Unit Composition, Homelessness, Expedited Service, Application or Renewal, Status of Error Findings | `Employment` | |
| **Include Other Errors for comparison** | checkbox | ON | When ON, adds a second bar group per demographic value showing the same demographic distribution computed from *all other* errors. Helps separate error-specific patterns from the general distribution. |

### Reading the chart

- **X axis:** two grouped bars per demographic value — `"Selected Error: …"`
  and `"Other Errors"` (the latter only if the comparison checkbox is on).
- **Y axis:** percentage of cases within the panel that fall in that
  demographic bucket.
- **Facets:** one panel per state when more than one state is selected.
- **0% bars are hidden** — values that round to 0% are dropped via
  `filter(round(Proportion, 0) > 0)` (`app.R:1460`).

### Caveats

- The bar chart uses `df` (the demographics frame), which **drops** rows
  with NA in any demographic column. Counts here will not exactly match
  the Sankey or Pivot Table tabs.
- The legend uses a fixed palette of 8 named colors plus a fallback of
  `"#59385c"` for any extra categories (see `app.R:1481–1488`). If a
  demographic has more than 8 buckets, the extras all share one color —
  this is by design but worth being aware of.

---

## 4.3 Tab 3 — Error Severity (dot plot)

A dot plot that quantifies the financial impact of each error category.
Useful for answering "which errors cost the most per case?"

### Controls

| Control | Choices | Default | Notes |
|---|---|---|---|
| **Select Year(s)** | 2017–2024, multi-select | all 8 years | |
| **Select State** | `"All States"` + 53 states | `"All States"` | Single-select (not multi). |
| **Select Error Status** | All / Underissuance / Overissuance | All | |
| **Only include cases above yearly error-rate threshold** | checkbox | ON | |
| **Error Responsibility** | All / Client / Agency / Technical | All | Filters using the hard-coded `type_to_responsibility` mapping. |
| **Group By** | Error Type / Error Nature / Error Element | Error Type | What's on the X axis. |
| **Sort By** | Average Dollar Amount / Number of Cases | Average Dollar Amount | What determines which categories are in the "top N". |
| **Display Top** | 5 / 10 / 20 | 5 | Limits the X axis. |

### Reading the plot

- **X position:** the chosen Group (Type / Nature / Element), wrapped to
  25-character lines.
- **Y position:** average dollar amount in error per unique case for that
  group.
- **Dot size:** `sqrt(n_cases) * 3` — proportional to the square root of
  the number of unique cases, so visually comparable but not linear.

### Download data

The **Download Data** button below the plot exports a CSV with these
columns for the current filter selection:

```
Error Category, ResponsibilityGroup, Average_Dollar_Error, Case_Count
```

The CSV ignores the "Top N" cap — it includes all categories that pass the
filters. Filename: `error_severity_all_<YYYY-MM-DD>.csv`.

---

## 4.4 Tab 4 — Pivot Table

An ad-hoc cross-tabulation built on the
[`pivottabler`](https://cran.r-project.org/package=pivottabler) package.
This is the "build your own view" tab.

### Controls — Filter data (top of sidebar)

- **Choose filters** (multi-select picker): pick which fields to filter on.
  Default: `State`, `Year`. Each chosen field reveals a multi-select
  picker beneath the main one with all values pre-selected. Leaving the
  filter unset for a field includes ALL data.
- **Only include cases above yearly error-rate threshold** (default ON).

The available fields are:

```
Error Responsibility, Error Type, Error Nature, Error Element,
Error Discovery, Error Timing, Status of Error Findings,
Case Type, Year, State
```

### Controls — Configure pivot

- **Select row** (single): which dimension goes on the rows.
  Default: `Error Type`.
- **Select column** (single): which dimension goes on the columns.
  Default: `Error Discovery`.

(There is also a commented-out `pivot_agg` selector in the source — the
table is currently always counts of unique `Case ID`. To re-enable
average-dollar / sum-dollar metrics, uncomment the relevant section near
`app.R:600` and the `pivot_agg` switch in `output$pvt_table` at
`app.R:2281–2306`.)

### Reading the table

- **Cell values:** unique `Case ID` count for the (row, column) pair —
  i.e. how many distinct cases had that combination.
- **Row/column totals:** automatic totals are appended by `pivottabler`
  and shaded a darker blue.
- **Title bar above the table** is generated dynamically and reads, for
  example: *"Error Type by Error Discovery — Showing State: 53 selected
  • Year: 2017–2024. Metric: Unique Cases."*

> **Important caveat displayed below the table:** "Each row in the dataset
> represents a unique case–error combination, so a single case may appear
> more than once if it involves multiple types of errors." Counts here
> are **distinct cases** within each (row, column) cell, not raw
> case–error rows.

---

## 4.5 Tab 5 — Base Rates

A state-level base-rate analysis that puts error counts in the context of
the *number of reviewed cases*. This is the only tab that uses the
`base_case_*` denominator.

### Controls — Filter data

- **Choose filters** (multi-select picker): the same 10 fields as the
  Pivot Table tab.
  Default selection: `State`, `Error Element`, `Error Nature`.
- For each chosen field, a multi-select picker appears with all values
  pre-selected. Special default: if `Error Element` is selected and the
  value `"Earned income deductions"` exists, only that one value is
  pre-selected (`app.R:1729–1733`). This is intentional — it makes the
  default view interpretable instead of overwhelming.
- **Only include cases above yearly error-rate threshold** (default ON).

### Controls — Configure output

- **Break down base rates by** (multi-select picker): which dimensions
  appear as columns in the output table.
  Choices: `State`, `Year`, `Case Type`, `Status of Error Findings`,
  `Error Type`, `Error Nature`, `Error Element`.
  Default: `State`, `Year`.

### The two output sub-tabs

#### 4.5.1 "Base error rates"

A long-format DT table with the columns:

```
<grouping vars> | Reviewed cases (N) | Error cases (N) | Error rate (%)
```

- `Reviewed cases (N)` is the count from `base_case_all` after applying
  the case-side filters.
- `Error cases (N)` is the distinct-case count from `base_cat_all` after
  applying the category-side filters and intersecting with reviewed cases.
- `Error rate (%)` = `error_n / reviewed_n × 100`, rounded to 1 decimal.
- Sortable / paginated (25 rows / page).

A **Download CSV** button is provided
(`base_error_rates_<YYYY-MM-DD>.csv`).

#### 4.5.2 "Simple view (State × Error Nature)"

A wide pivot for quick comparison across states:

- **Rows:** State.
- **Second column:** `n` — total error-case count for that state under
  the current filters.
- **Other columns:** one per Error Nature, with the **percentage of total
  errors** (within that state) attributable to that nature.
- Rounded to 1 decimal; missing combinations rendered as `0`.

The table is wide; it scrolls horizontally inside the main panel without
the page itself scrolling. (See the CSS block under `#base-rates-layout`
in `app.R:222–257` — that whole block exists to contain the horizontal
scroll inside the table wrapper rather than the page.)

A **Download CSV** button is provided
(`base_rates_simple_<YYYY-MM-DD>.csv`).

### How filters interact (important)

The Base Rates tab uses **two-sided filtering** — read this carefully:

- Filters that are **case-level** (State, Year, Case Type, Status,
  threshold) are applied to the `base_case_all` denominator.
- Filters that are **error-dimension** (Error Type / Nature / Element) are
  applied to `base_cat_all` only — they restrict which errors are counted
  in the numerator but do **not** shrink the denominator.

So changing `Error Element = "Earned income deductions"` answers the
question *"out of all reviewed cases, what share had an error in earned
income deductions?"* — not *"out of cases that had earned income
deductions, what share had an error?"* This is the correct prevalence
calculation; do not "fix" it without first reading
[`06-key-modules.md`](06-key-modules.md) section "Base Rates joins."

---

## 4.6 Footer

The footer (rendered below every tab) shows:

- The Better Government Lab logo.
- A link to the upstream USDA SNAP QC database.
- A reminder that Sankey nodes are sorted in descending frequency and
  bar-chart values that round to 0% are hidden.
- A link to the
  [error-scenarios reference sheet](https://docs.google.com/spreadsheets/d/1zLF0h30Ic2Y9cv4sRDV3UTuInR-QgeN9RWrvRA0bT3k/edit?usp=sharing).
- Author credits.
