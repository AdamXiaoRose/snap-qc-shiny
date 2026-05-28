# 5. Code Architecture (`app.R`)

The entire application lives in a single 2,334-line file: **`app.R`**. There
is no `global.R`, no `ui.R`/`server.R` split, no R modules, and no helper
files. This document is a top-down map so you can find the code for any
behavior in seconds.

> All line numbers below refer to the version of `app.R` at the head of
> the `main` branch as of the latest commit. They drift as the file
> changes; if a line number is off, search for the nearest function name
> or string literal mentioned alongside it.

---

## 5.1 File layout (zoom-out)

```
app.R
├── 1–9     Library loads
├── 11–19   threshold_by_year — yearly QC error-rate cutoff
├── 22–60   Pivot Table data load (verification_all)
├── 62–64   Base Rates data load (base_case_all, base_cat_all)
├── 67–98   Sankey/Severity data load (df_error)
├── 92–98   error_threshold_flags — case-level threshold flag
├── 100–132 Demographics data load + drop_na + threshold join (df)
├── 134–152 type_to_responsibility — Type → Client/Agency/Technical map
├── 154–783 ui  (fluidPage, header, navbarPage, 5 tabPanels, footer)
│           ├── 154–271  CSS in <head>
│           ├── 273–306  Header (logo + h1 + caveat)
│           ├── 308–747  navbarPage with 5 tabs
│           │   ├── 313–423  Error Pathways
│           │   ├── 426–470  Error Demographics
│           │   ├── 473–541  Error Severity
│           │   ├── 544–649  Pivot Table
│           │   └── 652–746  Base Rates
│           └── 750–782  Footer
├── 787–2325 server  (one big function)
│           ├── 789–797   base_case_all threshold flag (server-side)
│           ├── 799–950   element/nature/type choice lists (sidebar pickers)
│           ├── 952–977   output$dynamicFilters (Sankey)
│           ├── 979–988   output$threshold_ui (Sankey, metric-aware)
│           ├── 991–1007  summarise_links (Sankey aggregator)
│           ├── 1010–1212 output$sankeyPlot
│           ├── 1216–1378 output$error_dropdown_bar (Demographics)
│           ├── 1380–1499 data() reactive + output$barPlot
│           ├── 1503–1588 output$errorDollarPlot (Severity)
│           ├── 1591–1635 output$download_dollar_data
│           ├── 1638–1681 utility helpers (.sanitize_id, .pretty_list,
│           │              .pretty_years, .clean_cat_or_na, .get_vals)
│           ├── 1683–1712 output$dynamic_filters (Pivot Table)
│           ├── 1714–1749 output$br_dynamic_filters (Base Rates)
│           ├── 1752–1770 filtered_df reactive (Pivot Table)
│           ├── 1772–1898 base_case_filtered, base_cat_filtered,
│           │              base_rates_summary reactives (legacy/dup of br_*)
│           ├── 1900–1962 br_base_case_filtered, br_base_cat_filtered
│           ├── 1964–2107 br_base_rates_summary
│           ├── 2110–2170 br_base_rates_simple (State × Error Nature)
│           ├── 2173–2212 output$pvt_title
│           ├── 2214–2231 output$base_rates_table (DT)
│           ├── 2233–2248 output$download_base_error_rates_csv
│           ├── 2250–2264 output$base_rates_simple_table (DT)
│           ├── 2266–2274 output$download_simple_view_csv
│           └── 2276–2324 output$pvt_table (pivottabler)
└── 2327     shinyApp(ui = ui, server = server)
```

---

## 5.2 Data-loading block (top of file)

The first ~150 lines load every dataset into module-level variables and
derive a few cross-cutting columns. These are computed **once at app
startup**, not per session — every Shiny session shares the same in-memory
copies.

The four "global" data frames are:

| Variable | Source files | Used by |
|---|---|---|
| `verification_all` | `pivot_table_<year>.RDS` × 8 + recoded columns | Pivot Table tab |
| `df_error` | `snap_error_<year>.RDS` × 8 + threshold | Sankey, Severity |
| `df` | `snap_demographics_<year>.RDS` × 8 + drop_na + threshold join | Demographics |
| `base_case_all`, `base_cat_all` | `base_case_2017_2024.rds`, `base_cat_2017_2024.rds` | Base Rates |

A fifth helper, `error_threshold_flags`, is a per-case flag (1 if any
error in that case is over threshold) used to make the threshold checkbox
work on the demographics frame, which has no dollar column of its own.

The **`type_to_responsibility`** named list at lines 134–152 is the
authoritative mapping of Error Type → Client / Agency / Technical
responsibility. If USDA-FNS adds a new Type label upstream, that label
must be added to this list, otherwise the Severity tab's Responsibility
filter will silently treat it as missing.

---

## 5.3 UI structure

The UI is built with `fluidPage` + `navbarPage`. Each tab is a `tabPanel`
with a `sidebarLayout(sidebarPanel, mainPanel)`.

### Notable UI patterns

- **Inline CSS in `tags$head`** at lines 159–268. Includes the styling
  for the navbar, the sticky footer, the Base Rates layout (two-column
  with controlled overflow), and the Sankey link hover effect. There is
  no separate `.css` file.
- **Static images** under `www/` are referenced as `src = "bgl_top_logo.png"`
  and `src = "BetterGovernmentLab-Logo-WithUniversities-Orange.png"`.
  Shiny serves the `www/` folder as static assets at the root URL.
- **Tooltips** use `shinyBS::bsTooltip`. Each `bsTooltip` is bound to an
  input ID and provides hover help text. They sit *next to* the inputs in
  the source — search for `bsTooltip` to find them.
- **Conditional headings above the Sankey** (lines 386–418): each
  pathway has its own labeled column-header strip ("What went wrong",
  "How it happened", "Why it happened") wrapped in a `conditionalPanel`
  on `input.combination`.

### Dynamic UI

Several `uiOutput(...)` placeholders are rendered server-side:

| `uiOutput` | Server function | What it builds |
|---|---|---|
| `dynamicFilters` | lines 952–977 | The Element/Nature/Type sidebar pickers in the Sankey tab — different combos depending on the selected pathway. |
| `threshold_ui` | lines 981–988 | The min-flow numeric input — switches label and default between "occurrences" and "dollars" based on the metric radio. |
| `error_dropdown_bar` | lines 1216–1378 | The "Select Error Type/Nature/Element" picker on the Demographics tab — its choices change with the radio buttons above it. |
| `dynamic_filters` | lines 1683–1712 | The per-field multi-select pickers under "Choose filters" on the Pivot Table tab. |
| `br_dynamic_filters` | lines 1714–1749 | Same idea, for the Base Rates tab. |
| `pvt_title` | lines 2173–2212 | The title bar above the pivot table that summarizes which filters are active. |

---

## 5.4 Server reactive graph

A simplified diagram of the dependencies between reactives and outputs
(arrows point from upstream to downstream):

```
┌─────────────────────────────────────────────────────────────────┐
│                   GLOBAL CONSTANTS (loaded once)                │
│  threshold_by_year  type_to_responsibility                      │
│  verification_all   df_error   df   base_case_all   base_cat_all│
└─────────────────────────────────────────────────────────────────┘
                       │
        ┌──────────────┼─────────────┬─────────────┬─────────────┐
        ▼              ▼             ▼             ▼             ▼
   sankeyPlot     barPlot       errorDollarPlot    pvt_table   base rates
   (Sankey)      (Demographics)  (Severity)        (Pivot)     (Base Rates)
        ▲              ▲             ▲             ▲             ▲
        │              │             │             │             │
   year_sankey    year_bar       year_amt      filter_vars   br_filter_vars
   state_sankey   state_bar      state_amt     pivot_rows    br_base_group_vars
   combination    error_select   status_amt    pivot_cols    br_over_threshold_only
   flow_by        demographic    error_resp    flt__<id>     br_flt__<id>
   threshold      include_other  view_amt_by   pivot_over_…
   status_filter  bar_over_…     amt_sort_by
   element/nature/type filters   amt_top_n
   sankey_over_threshold_only    severity_over_threshold_only
```

### Reactives in the Pivot Table tab

```
filter_vars (input)  ──► output$dynamic_filters (renders flt__<id> pickers)
                          │
                          ▼
                   filtered_df  ──►  output$pvt_table  ──►  pvt rendering
                                 ──►  output$pvt_title (subtitle text)
```

### Reactives in the Base Rates tab

```
br_filter_vars   br_flt__<id>   br_over_threshold_only    br_base_group_vars
       │              │                  │                         │
       ▼              ▼                  ▼                         │
br_base_case_filtered  ◄──┐                                        │
       │                  │                                        │
       ├─► br_base_cat_filtered (semi_joined to denom)             │
       │            │                                              │
       └────────────┴──► br_base_rates_summary  ──────► base_rates_table (DT)
                                              ──────► download_base_error_rates_csv
                                              ──────► (also: br_base_rates_simple)
                    ─────────────────────────► br_base_rates_simple ─► simple_table (DT)
                                                                    ─► download_simple_view_csv
```

### "Two parallel reactive trees" in the Base Rates code

You will notice **two** sets of nearly identical reactives:

- `base_case_filtered`, `base_cat_filtered`, `base_rates_summary` (lines
  1772–1898), driven by the **Pivot Table** tab inputs (`filter_vars`,
  `flt__<id>`, `pivot_over_threshold_only`).
- `br_base_case_filtered`, `br_base_cat_filtered`,
  `br_base_rates_summary` (lines 1900–2107), driven by the **Base Rates**
  tab inputs (`br_filter_vars`, `br_flt__<id>`,
  `br_over_threshold_only`).

The first set is **legacy / unused** by the live UI — the Pivot Table tab
no longer renders the base-rates output (note the commented-out
`pickerInput("br_base_group_vars", ...)` block at lines 600–617). It is
safe to delete the unused `base_*_filtered` / `base_rates_summary`
reactives once you have confirmed no other code references them; this
will simplify the file by ~125 lines.

---

## 5.5 Outputs by tab — quick lookup

| Tab | Output IDs | Server functions |
|---|---|---|
| Error Pathways | `dynamicFilters`, `threshold_ui`, `sankeyPlot` | 952–977, 981–988, 1010–1212 |
| Error Demographics | `error_dropdown_bar`, `barPlot` | 1216–1378, 1380–1499 |
| Error Severity | `errorDollarPlot`, `download_dollar_data`, `totalDollarSummary` (note: `totalDollarSummary` is referenced in UI at line 538 but **not implemented** in the server — see Maintenance doc) | 1503–1588, 1591–1635 |
| Pivot Table | `dynamic_filters`, `pvt_title`, `pvt_table` | 1683–1712, 2173–2212, 2276–2324 |
| Base Rates | `br_dynamic_filters`, `base_rates_table`, `base_rates_simple_table`, `download_base_error_rates_csv`, `download_simple_view_csv` | 1714–1749, 2214–2231, 2250–2264, 2233–2248, 2266–2274 |

---

## 5.6 Style and rendering layer

- **Plotly** powers the Sankey (`renderPlotly` + `plot_ly(type="sankey")`)
  and the Severity dot plot (`plot_ly(type="scatter", mode="markers")`).
  Both call `config(displayModeBar = FALSE)` to hide the Plotly toolbar.
- **ggplot2** (loaded via `tidyverse`) powers the Demographics bar chart,
  inside `renderPlot`. Color palette is hard-coded at `app.R:1481–1488`.
- **pivottabler** builds the Pivot Table, with a small inline theme at
  `app.R:2308–2320`. Cells are unique-case counts; sum/mean dollar metrics
  exist in code paths but the UI selector for them is currently disabled.
- **DT** renders the two Base Rates tables. Both enable `scrollX = TRUE`.
- **shinyBS** supplies tooltips. **shinyWidgets** supplies `pickerInput`.
  **bslib** is loaded but the project does not use a `bs_theme()` — the
  page picks up the default Bootstrap from Shiny.

---

## 5.7 Things that look like dead code

If you decide to clean up `app.R`, double-check before deleting:

- The `colnames(verification_all)` call at line 60 — leftover console probe.
- Per-tab pickers in the *Pivot Table sidebar* commented out at lines
  600–617 (the old "Base rates inside the Pivot tab" experiment).
- The `# req(!isTRUE(input$show_base_rates))` line at 2277 — refers to an
  input that no longer exists.
- The `base_case_filtered`, `base_cat_filtered`, `base_rates_summary`
  reactives at 1772–1898 — duplicated by the `br_*` versions and not
  driving any output.
- The `totalDollarSummary` output declared in the UI at line 538 but not
  implemented in the server — currently renders nothing.

None of these break the app, but cleaning them up will shave the file
down meaningfully and remove confusion when reading.
