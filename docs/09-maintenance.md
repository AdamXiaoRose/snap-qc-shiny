# 9. Maintenance Playbook

This document covers recurring maintenance tasks, common gotchas, and a
troubleshooting playbook.

---

## 9.1 Annual data refresh — adding a new fiscal year

USDA-FNS releases a new fiscal-year QC dataset roughly **9–15 months**
after the close of the FFY. To add (for example) FFY 2024 to the app:

### Step 1 — Get the new threshold

Find USDA-FNS's published QC error-rate threshold for the new year. It
appears in the annual error-rate report on the
[FNS QC database page](https://www.fns.usda.gov/snap/qc/database).

Add the threshold to `app.R:11–19`:

```r
threshold_by_year <- c(
  `2017` = 38, `2018` = 37, `2019` = 37, `2020` = 37,
  `2021` = 39, `2022` = 48, `2023` = 54,
  `2024` = <NEW>           # <— add this line
)
```

### Step 2 — Build the per-year `.RDS` files

Re-run the upstream ETL (kept outside this repo — see
[`02-data-sources.md`](02-data-sources.md), section 2.4) to produce:

```
data/pivot_table_2024.RDS
data/snap_error_2024.RDS
data/snap_demographics_2024.RDS
```

Their schemas must match those in
[`03-data-files.md`](03-data-files.md). The single most important
contract:

- `pivot_table_<year>.RDS` and `snap_error_<year>.RDS` must have the
  same row count (one row per case × error finding).
- `snap_demographics_<year>.RDS` row count will be lower because of
  `drop_na()` chains in the app.

### Step 3 — Rebuild the cross-year base files

Regenerate the two combined files so they cover the new year too:

```
data/base_case_2017_2024.rds
data/base_cat_2017_2024.rds
```

(Notice the *file name itself* changes — the year range is part of the
name.) Then update **every reference** in `app.R`:

- `app.R:63–64` — `readRDS("data/base_case_2017_2024.rds")` and
  `readRDS("data/base_cat_2017_2024.rds")`.
- Anywhere these names appear in [`03-data-files.md`](03-data-files.md)
  and in this doc.

### Step 4 — Wire up the new year in `app.R`

Search `app.R` for the literal `2023` to find every site that lists
years explicitly. The five places you must edit:

```r
# 1. Pivot Table data load (lines 22–28)
verification_2024 <- readRDS("data/pivot_table_2024.RDS") %>%
                       mutate(year = rep(2024, nrow(.)))

# 2. bind_rows for verification_all (lines 30–38)
verification_all <- bind_rows(
  verification_2017, ..., verification_2023, verification_2024
) %>% ...

# 3. Sankey/Severity load (lines 67–73)
df_error_2024 <- readRDS("data/snap_error_2024.RDS") %>%
                  mutate(year = rep(2024, nrow(.)))

# 4. do.call("rbind", ...) for df_error (lines 75–81)
df_error <- do.call("rbind", list(df_error_2017, ..., df_error_2024)) %>% ...

# 5. Demographics load (lines 101–107) and bind (lines 109–115)
df_2024 <- readRDS("data/snap_demographics_2024.RDS") %>%
             mutate(year = rep(2024, nrow(.)))
df <- do.call("rbind", list(df_2017, ..., df_2024))
```

### Step 5 — Verify the year-default selectors

Two `selectizeInput` / `selectInput` calls hard-code the default year
selection as **all years 2017–2023**:

- `year_sankey` at `app.R:318–322`:
  `selected = c(2023, 2022, 2021, 2020, 2019, 2018, 2017)`
- `year_bar` at `app.R:431–433`: same.

Update both to include `2024`. The Severity tab (`year_amt`) uses
`max(df_error$year)`, which auto-updates when you add the year — no
change needed there.

### Step 6 — Test, deploy

Run through [`07-running-locally.md`](07-running-locally.md) section 7.5
(quick smoke test) with the new year selected. Then deploy per
[`08-deployment.md`](08-deployment.md).

---

## 9.2 Updating an existing year's threshold

If USDA-FNS retroactively revises a threshold (rare but possible),
change the value in `threshold_by_year` (`app.R:11`) and redeploy. No
data change is needed; the `over_threshold` columns are recomputed at
load time using the in-memory threshold lookup.

---

## 9.3 Updating logos / static assets

Logos live under `www/`:

- `bgl_top_logo.png` — small banner logo at the top of every page.
- `BetterGovernmentLab-Logo-WithUniversities-Orange.png` — large logo in
  the footer.

To replace either, drop the new file in `www/` with the **same filename**.
If you need to use a different filename, update the `img(src = ...)`
references at `app.R:280` and `app.R:757`.

Image dimensions are CSS-controlled at the same lines; tweak
`max-height` / `height` there if the new logo has a different aspect
ratio.

---

## 9.4 Adding a new tab

The pattern is:

1. Add a new `tabPanel("My New Tab", sidebarLayout(sidebarPanel(...),
   mainPanel(...)))` inside `navbarPage` (between lines 308 and 747).
2. Add the matching `output$myNewPlot <- renderPlotly({ ... })` (or
   `renderPlot`, `renderDT`, etc.) inside `server()`.
3. If it filters by year/state/threshold, follow the same pattern as
   the existing tabs — read the relevant in-memory frame
   (`verification_all` / `df_error` / `df` / `base_case_all` /
   `base_cat_all`), apply filters, then render.
4. **Always** include a "Only include cases above yearly error-rate
   threshold" checkbox if your tab has any dollar-amount semantics.
   Defaulting to ON is the project convention.

---

## 9.5 Common gotchas (in priority order)

### G1. Threshold checkbox default

Every threshold checkbox is **`value = TRUE`** today (default ON). Do
not silently flip the default to OFF — it changes every published
number. If you must change it, document the change in the deployment
PR.

### G2. `case_id` integer vs. character

`case_id` is `integer` in every `.RDS` but is cast to `character`
defensively before joins on the Base Rates tab. If you copy-paste join
code from one tab to another, keep the cast.

### G3. The `year` vs `Year` casing trap

`verification_all` uses **`Year`** (capital). Every other frame uses
**`year`** (lowercase). The threshold lookup uses
`as.character(Year)` / `as.character(year)` — keep the case correct
when copying code between tabs, otherwise the threshold will silently
not apply.

### G4. The two parallel reactive trees

The Pivot Table tab and the Base Rates tab each have their own set of
filter inputs (`flt__*` and `br_flt__*`). They are independent. If you
ever try to "share" them you will create a circular reactive
dependency, because the dynamic-UI generator depends on the field
selection picker which itself depends on the data. Keep them separate.

### G5. The "All States" multi-select on the Demographics tab

On the Demographics tab, `"All States"` and individual states can be
selected together — and this combination produces both per-state
panels and an aggregated panel. Do not rewrite the data() reactive
unless you understand this contract.

### G6. State name strings, not abbreviations

State filters use full English state names (`"Connecticut"`,
`"District of Columbia"`, etc.). If the upstream ETL ever switches to
two-letter codes, every filter will silently match nothing.

### G7. Drop-NA on demographics

The demographics frame drops any row missing any one of 11 demographic
columns (`app.R:117–128`). If a future demographic gets added with
many NAs, the row count drops sharply and Demographics-tab numbers
will diverge from the other tabs. Either backfill the new column or
remove it from the `drop_na` chain.

### G8. The legacy `pivot_table_data.RDS`

This file is **not loaded** by the current app, but is still in
`data/` and shipped with every deploy (~1 MB). Safe to delete after
confirming no external notebook depends on it.

---

## 9.6 Known minor issues / dead code

These are cosmetic; none break the app. Address opportunistically.

| Issue | Location | Severity |
|---|---|---|
| `output$totalDollarSummary` referenced in UI but never defined in server. | `app.R:538` (UI), no server output. | Cosmetic — renders nothing. Either implement (sum of dollar amount in current Severity selection) or remove the `textOutput` call. |
| Unused `colnames(verification_all)` console probe at startup. | `app.R:60` | Trivial. Remove. |
| Commented-out `pivot_agg` selector and base-rates picker in the Pivot Table sidebar. | `app.R:600–617`, `app.R:2277` | Trivial. Either re-enable or remove. |
| Duplicated `base_*` reactives that mirror `br_*` versions and drive no output. | `app.R:1772–1898` | Worth deleting (~125 lines) once you confirm no other code references them. |
| `# includeHTML("google-analytics.html")` commented include. | `app.R:158` | If you want analytics, restore the file and uncomment. |

---

## 9.7 Troubleshooting playbook

### "App opens but the Sankey is blank"

- Open the browser console. Plotly errors are logged there.
- Likely cause: filters are too restrictive (year × state × min-flow
  threshold combine to leave 0 rows). Lower the threshold or widen the
  year/state selection.
- If you recently added a new year and the Sankey is blank for that
  year only: confirm `threshold_by_year` has the new year (G1 above).

### "Base Rates table shows N=0 for everything"

- Confirm `base_cat_all` has data for the years you selected.
- Confirm `case_id` types match between `base_case_all` and
  `base_cat_all` after the character cast (G2 above).
- Confirm an error-dimension picker isn't empty (an empty multi-select
  intentionally returns 0 rows — see
  [`06-key-modules.md`](06-key-modules.md) section 6.4, point 2).

### "Demographics tab counts don't match the Pivot Table tab"

- This is **expected**. The demographics frame drops rows with any
  NA in 11 demographic columns; the pivot frame keeps them.
- Document the discrepancy if a stakeholder asks; do not "fix" by
  removing the `drop_na` chain — the bar chart is meaningless without
  filled-in demographics.

### "Deploy succeeded but the live app is showing the old version"

- Hard-reload (Ctrl-Shift-R / Cmd-Shift-R) to defeat the browser
  cache.
- Open in incognito.
- Check the shinyapps.io dashboard's "Last Deployed" timestamp matches
  what `deployApp()` reported.

### "Local R session crashes on startup"

- Most likely cause: not enough RAM. The app keeps every dataset in
  memory; total resident size is ~250 MB.
- Confirm you are on R ≥ 4.1.
- Try `Sys.setenv(R_MAX_VSIZE = "8Gb")` before `runApp()`.

---

## 9.8 Where to ask for help

- Original developers: **Zhaowen Guo**, **Xiao Xu**.
- Lab page: [Better Government Lab — SNAP QC Error
  Viewer](https://www.bettergovernmentlab.org/resources/snap-quality-control-error-viewer).
- USDA-FNS QC database (data definitions): [fns.usda.gov/snap/qc/database](https://www.fns.usda.gov/snap/qc/database).
- Reference sheet for example error scenarios:
  [Google Sheets](https://docs.google.com/spreadsheets/d/1zLF0h30Ic2Y9cv4sRDV3UTuInR-QgeN9RWrvRA0bT3k/edit?usp=sharing).
