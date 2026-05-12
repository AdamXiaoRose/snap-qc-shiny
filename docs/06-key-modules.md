# 6. Key Modules — Deep Dives

This document explains the non-obvious pieces of logic in `app.R`. If you
need to change one of them, read the relevant section here first.

---

## 6.1 The yearly threshold flag

**What it is.** Each year's federal QC error-rate threshold is the dollar
cutoff above which a payment variance counts as a *reportable* error.
Every dataset that has a per-row dollar amount gets a pair of derived
columns:

- `threshold` — the per-row threshold pulled from `threshold_by_year`.
- `over_threshold` — `1L` if `Dollar Amount in Error > threshold`, else
  `0L`.

The "Only include cases above yearly error-rate threshold" checkbox on
every tab simply filters on `over_threshold == 1`.

### Where it is computed

| Object | Computed at | Notes |
|---|---|---|
| `verification_all` | `app.R:50–57` (load time) | Used by Pivot Table tab. |
| `df_error` | `app.R:83–90` (load time) | Used by Sankey + Severity tabs. |
| `error_threshold_flags` | `app.R:92–98` (load time) | Per-case flag (max of `over_threshold` per `(case_id, year)`), then **left-joined onto `df`** at lines 130–132 so the Demographics frame gets a case-level threshold flag. |
| `base_case_all` | `app.R:789–797` (inside `server()`) | The flag is computed *server-side* here, not at load time. Defensive: `has_error == 1` AND dollar > threshold. |

### Why the demographics frame uses a per-case flag

`df` (demographics) does not contain a dollar amount. The threshold
checkbox on the Demographics tab needs to mean "include only cases that
*have at least one* over-threshold error" — which is precisely what the
`error_threshold_flags` left-join provides. After the join, NA values are
coalesced to `0L` so a case with no error rows simply has
`over_threshold == 0` and is filtered out when the checkbox is on.

### Adding a new year

You must add the new year to **`threshold_by_year`** (`app.R:11–19`).
Forgetting to do this will produce `NA` for `threshold`, then `0` for
`over_threshold` (because `NA > x` is `NA`, then the `if_else` sees it
not strictly greater), and the threshold checkbox will silently exclude
the entire new year.

---

## 6.2 The Sankey aggregator

The Sankey diagram is computed by a single helper, `summarise_links`,
defined at `app.R:991–1007`:

```r
summarise_links <- function(df, ..., mode = c("count", "dollars"),
                            use_abs = TRUE) {
  mode <- match.arg(mode)
  grps <- rlang::quos(...)
  if (mode == "count") {
    df %>% group_by(!!!grps) %>%
      summarise(value = n(), .groups = "drop")
  } else {
    df %>% group_by(!!!grps) %>%
      summarise(
        value = {
          v <- .data[["Dollar Amount in Error"]]
          if (use_abs) sum(abs(v), na.rm = TRUE) else sum(v, na.rm = TRUE)
        },
        .groups = "drop"
      )
  }
}
```

### What the modes do

- **`mode = "count"`** — link width = number of case–error rows. Pure
  occurrence count.
- **`mode = "dollars"`** — link width = `sum(abs(Dollar Amount in Error))`.
  The absolute value is intentional: under-issuance and over-issuance
  errors must both contribute to total magnitude. Setting `use_abs =
  FALSE` would let signed dollars cancel out, which is rarely what you
  want for a flow diagram.

### How the diagram is assembled

`output$sankeyPlot` (lines 1010–1212):

1. Apply year, state, status, threshold, and Element/Nature/Type
   filters to `df_error`.
2. Pick the metric (`mode_now`) from the radio.
3. Branch on `input$combination`:
   - `"Nature → Type"` — one call to `summarise_links(.., Nature, Type)`.
   - `"Element → Nature"` — one call to `summarise_links(.., Element,
     Nature)`.
   - `"Element → Type"` — one call to `summarise_links(.., Element, Type)`.
   - `"Element → Nature → Type"` — *two* calls, one for `Element →
     Nature` and one for `Nature → Type`, then `bind_rows` the link
     tables.
4. After computing each link table, drop links whose `value` is below the
   user's `threshold` input.
5. Build a `nodes` data frame with `name`, `x`, `y` so plotly knows where
   to place each node. Nodes are placed at `x = 0.01` (left), `0.50`
   (middle), or `0.99` (right) according to their column.
6. Translate Element / Nature / Type labels to integer node indices via
   `match(name, nodes$name) - 1` (Plotly uses 0-based node indexing).
7. Render with `plot_ly(type = "sankey", arrangement = "snap", …)`. Node
   colors are picked by x-position: pink for left, purple for middle,
   gold for right.

### Hover formats

Lines 1168–1176:

```r
link_hover <- if (mode_now == "count")
  "%{source.label} → %{target.label}: %{value:,.0f} case-errors<extra></extra>"
else
  "%{source.label} → %{target.label}: $%{value:,.0f}<extra></extra>"
```

---

## 6.3 The dynamic filter system (Pivot Table & Base Rates)

Both tabs let the user pick a *set* of fields from a master picker, and
then dynamically render a per-field multi-select picker beneath. The
mechanism is identical except for the `flt__` vs `br_flt__` prefix.

### The pieces

```
input$filter_vars    # which fields the user wants to filter on
       │
       ▼
output$dynamic_filters  # for each chosen field v, render a pickerInput
                        # with id = paste0("flt__", .sanitize_id(v))
       │
       ▼
input$flt__<sanitized>  # the user's selection for field v
       │
       ▼
filtered_df()           # iterate over input$filter_vars, look up each
                        # input by its constructed id, apply the filter
```

### Helpers

- **`.sanitize_id(x)`** at `app.R:1639` — turns `"Status of Error
  Findings"` into `"status_of_error_findings"` so it can be used as an
  input ID. Lowercases and replaces non-alphanumerics with `_`.
- **`.get_vals(df, var)`** at `app.R:1671–1681` — pulls all unique
  non-empty values of `var` from `df`, sorted. Used to populate each
  picker. Treats `""` and whitespace-only as `NA`.
- **`.clean_cat_or_na(x)`** at `app.R:1661–1669` — strips whitespace,
  empties → NA, and unwraps `haven_labelled` columns. Applied to the
  three error-dimension columns (`Error Type`, `Error Nature`, `Error
  Element`) on the Base Rates side. Important because `base_cat_all`
  carries some labelled columns from the upstream SAS read.
- **`.error_dim_vars`** at `app.R:1659` — the list `c("Error Type",
  "Error Nature", "Error Element")`. Used to decide whether a filter
  should hit `base_case_all` (the case table) or `base_cat_all` (the
  category table) on the Base Rates tab.
- **`.pretty_list`** / **`.pretty_years`** at `app.R:1640–1657` — render
  filter selections as a short summary string in the pivot title bar
  (e.g., "2017–2023" instead of listing all 7 years).

### Why two sets exist

The same pattern appears twice with slightly different defaults — see
[`05-code-architecture.md`](05-code-architecture.md) section 5.4. The
first set (no prefix) is leftover code; only the `br_*` set drives
visible output today.

---

## 6.4 The Base Rates joins (the trickiest module)

This is where most bugs will hide. It implements **two-sided filtering**:
filters that apply to the case table (denominator) act differently from
filters that apply to the category table (numerator).

### The two source frames

- `base_case_all` — one row per reviewed case. Columns include `Case ID`,
  `State`, `Year`, `Case Type`, `Status of Error Findings`, `has_error`,
  `Dollar Amount in Error`, plus the derived `over_threshold` flag.
- `base_cat_all` — one row per (case × error category). Columns include
  `Case ID`, `Error Type`, `Error Nature`, `Error Element`, `Error
  Discovery`, `Error Timing`, `Error Responsibility`, plus repeats of
  `State`, `Year`, etc.

### The pipeline (`br_base_rates_summary`, `app.R:1964–2107`)

```
  ┌────────────────────────┐    ┌────────────────────────┐
  │ br_base_case_filtered  │    │ br_base_cat_filtered   │
  │ — apply CASE-side       │    │ — apply CATEGORY-side  │
  │   filters to             │    │   filters to            │
  │   base_case_all          │    │   base_cat_all          │
  │ — does NOT apply error- │    │ — semi_join to keep     │
  │   dimension filters     │    │   only Case IDs that    │
  │                          │    │   survive the case      │
  │                          │    │   filters above         │
  └─────────┬──────────────┘    └──────────┬──────────────┘
            │                              │
            │ denominator                  │ numerator candidates
            ▼                              ▼
  ┌──────────────────────────────────────────────────────────┐
  │ br_base_rates_summary                                    │
  │  1. error_case_df = denom_df where has_error == 1        │
  │     (and over_threshold == 1 if checkbox is on)          │
  │  2. valid_error_case_ids = unique Case IDs in cat_df     │
  │  3. filtered_error_case_df = semi_join(error_case_df,    │
  │                                  valid_error_case_ids)   │
  │  4. CASE 1: no error-dim grouping in output              │
  │     -- group filtered_error_case_df by denom_group       │
  │     -- group denom_df by denom_group                     │
  │     -- error_rate = error_n / reviewed_n                 │
  │     CASE 2: output groups by Error Type/Nature/Element   │
  │     -- group cat_df2 by (denom_group_no_err, err_group)  │
  │     -- denom_s = group denom_df by denom_group_no_err    │
  │     -- left_join, error_rate = error_n / reviewed_n      │
  └──────────────────────────────────────────────────────────┘
```

### Subtleties to preserve when modifying this code

1. **Defensive `Case ID` casts.** Both reactives cast `Case ID` to
   `character` before any join (`app.R:1789, 1798, 1917, 1929`). The
   stored column is integer, but other joins in the codebase use
   character; mixing types in `dplyr::semi_join` silently produces
   zero-row results on some R/dplyr versions. Don't remove these casts.
2. **Empty selection = no rows.** If the user clears all values from an
   error-dimension picker, `br_base_cat_filtered` returns
   `cat_df[0, , drop = FALSE]` (line 1954). This is *intentional* — it
   forces the numerator to zero rather than to "everything." Without it,
   an empty multi-select would behave the same as "no filter," which is
   surprising.
3. **Threshold filter applies to the numerator only.** The `over_threshold`
   filter restricts which cases count as *errors* (numerator) but does
   NOT shrink the *reviewed cases* (denominator). This is correct:
   reviewed cases should always be counted; the threshold only governs
   what "error" means.
4. **Error-dimension filters never shrink the denominator.** The for-loop
   at `app.R:1904–1913` skips filters that are in `.error_dim_vars`
   when filtering `base_case_all`. Don't undo this.
5. **`coalesce(error_n, 0L)`** after the left join. A grouping cell may
   exist in `denom_df` but not in `error_df` — meaning some reviewed
   cases for that group had no error. The coalesce ensures the resulting
   rate is `0`, not `NA`.

### The "simple view" pivot (`br_base_rates_simple`, `app.R:2110–2170`)

A specialized version of the base-rates summary that always pivots on
**Error Nature** and renders one row per state. Steps:

1. Get `case_df` and `cat_df` from the same two reactives above.
2. Restrict to `has_error == 1` (and over-threshold if applicable).
3. Restrict to cases that survive the category filters
   (`semi_join` on `Case ID`).
4. Per state: count distinct error cases (`state_counts$n`).
5. Per (state, Error Nature): count distinct cases with that nature
   (`nature_counts$n_cases`).
6. Per state: `total = sum(n_cases)` across natures.
7. `pct = round(100 * n_cases / total, 1)`.
8. `pivot_wider` into a State × Nature table; missing combos filled with
   `0`.
9. Stick the `n` column right after `State` so users can see the
   denominator.

The `n` here is the **number of distinct error cases per state** under
the current filters; it is NOT the number of reviewed cases.

---

## 6.5 The bar-chart `data()` reactive

The Demographics tab's `data()` reactive (`app.R:1380–1464`) is more
involved than it looks because it builds **up to four** sub-frames and
binds them:

- `selected_group` — for the chosen error, per-state proportions for
  *individually picked states*.
- `other_group` — same, but for all *other* errors (only if the
  comparison checkbox is on).
- `selected_group_all` — for the chosen error, **aggregated across all
  53 states**, but only if the user picked `"All States"`.
- `other_group_all` — same, for other errors.

This lets users get both per-state panels *and* a national panel side by
side. The four sub-frames are stitched with `bind_rows`, and rows that
round to 0% are dropped to keep the chart readable.

If you ever need to drop the national-vs-individual feature, removing
the `if (include_all_states)` block at `app.R:1425–1452` is sufficient.

---

## 6.6 Plotly theming and palettes

- **Sankey nodes:** color by column index — `#d48f90` (left, pink),
  `#ab84a5` (middle, purple), `#d8b847` (right, gold). Lines 1195–1198.
- **Severity dots:** semi-transparent blue, `rgba(93, 164, 214, 0.7)`,
  size = `sqrt(n_cases) * 3`. Lines 1551–1557.
- **Bar chart palette:** `#5b859e #1e395f #75884b #1e5a46 #df8d71
  #af4f2f #b38711 #732f30`, then `#59385c` repeated 100× for fallback.
  Line 1481–1487.
- **Pivot Table theme:** `headerBackgroundColor = "rgba(93, 164, 214,
  0.2)"`, `totalBackgroundColor = "rgba(93, 164, 214, 0.3)"`,
  `borderColor = "rgba(93, 164, 214, 0.5)"`. Lines 2308–2320.

These palettes were chosen for accessible contrast on a white
background; if you change them, re-check against WCAG AA.
