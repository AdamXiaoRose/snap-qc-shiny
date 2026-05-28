# 7. Running the App Locally

This guide gets the app running on your laptop in ~10 minutes assuming a
working R installation.

---

## 7.1 Prerequisites

| Requirement | Tested with | Notes |
|---|---|---|
| R | **4.4.1** (also tested on 4.1+) | Any R ≥ 4.1 should work. |
| RStudio | recent release | Recommended but optional — `Rscript`/`R` from the command line works too. |
| OS | Windows 11, macOS, Linux | Path handling differs on Windows; the repo uses forward slashes throughout. |

The full original development environment is Windows 11, R 4.4.1,
RStudio. The shinyapps.io runtime is Linux + R 4.x.

### Required R packages

The app declares its dependencies at the top of `app.R` (lines 1–9):

```r
library(shiny)
library(shinyBS)
library(bslib)
library(shinyWidgets)
library(jsonlite)
library(scales)
library(plotly)
library(pivottabler)
library(tidyverse)
```

`tidyverse` brings in `dplyr`, `tidyr`, `ggplot2`, `stringr`, `readr`,
`forcats`, etc. `DT` is also needed (used in the Base Rates tab via
`DT::renderDT`), and `haven` may be loaded transitively for labelled
columns in `base_cat_all`.

A complete one-shot install:

```r
install.packages(c(
  "shiny", "shinyBS", "bslib", "shinyWidgets", "jsonlite", "scales",
  "plotly", "pivottabler", "tidyverse", "DT", "haven", "rlang",
  "htmltools", "rsconnect"
))
```

`rsconnect` is only needed when you plan to publish to shinyapps.io; it
is not required to run locally.

---

## 7.2 Clone & launch

```bash
git clone https://github.com/<your-account>/snap-qc-shiny.git
cd snap-qc-shiny
```

Then either:

**Option A — RStudio.** Open `app.R`, click **Run App** in the upper
right of the editor pane.

**Option B — R console / Rscript.**

```r
setwd("snap-qc-shiny")          # the repo root
shiny::runApp(launch.browser = TRUE)
```

The first launch will read all 20 `.RDS` files and `bind_rows` them in
memory; expect a 5–15 second cold start. Subsequent reactive updates are
fast because every dataset stays resident.

---

## 7.3 Where to look when something breaks locally

| Symptom | Likely cause | Fix |
|---|---|---|
| `cannot open file 'data/pivot_table_2017.RDS'` | `setwd()` is wrong | `setwd()` to the repo root, then re-run. |
| Package `pivottabler` not found | not installed | `install.packages("pivottabler")` |
| `htmltools::tags`, `rlang::quos` errors | very old `tidyverse` | `update.packages(ask = FALSE)`; ensure `tidyverse` is from CRAN, not an old MRAN snapshot. |
| Threshold checkbox excludes the new year you just added | new year missing from `threshold_by_year` | Add the new year to `app.R:11–19`. |
| Bar chart panel headers say "All States" instead of state names | `"All States"` was passed as a literal state in the multi-select | Verify you also picked individual state(s); `"All States"` alone is intentional aggregate-only. |
| Sankey shows "No flows pass the threshold" | the min-flow numeric is too high for the current filter combo | Lower the value or switch metric. |

---

## 7.4 Editing while the app is running

`shiny::runApp()` watches `app.R` for file changes and auto-restarts on
save. If you edit a function body and Shiny does not pick it up,
restart manually with the **Reload** button or by stopping and re-running.

`.RDS` files are loaded once at startup. **You must restart Shiny after
modifying any file in `data/`** — Shiny will not re-read `.RDS` on a
reactive update.

---

## 7.5 Quick smoke test

After launching, click through each tab and verify:

- **Error Pathways:** the Sankey renders for the default
  (`Element → Nature → Type`, `Occurrences`, threshold ON) and the link
  hover shows e.g. *"Wages and salaries → … → Information not reported by
  client: 12,345 case-errors"*.
- **Error Demographics:** the bar chart shows two grouped bars
  ("Selected Error" and "Other Errors") for `Type = "Policy incorrectly
  applied"`, demographic = `Employment`.
- **Error Severity:** the dot plot shows the top 5 Error Types across
  2017–2024 with `Average Dollar Amount` on the y-axis.
- **Pivot Table:** the table renders for `Error Type` × `Error
  Discovery` and the title bar reads
  *"Error Type by Error Discovery — Showing State: 53 selected
  • Year: 2017–2024. Metric: Unique Cases."*
- **Base Rates:** the long table renders, and the **Simple view** sub-tab
  shows a State × Error Nature percentage matrix.

If all five render, you have a working local environment.
