# 3. Data File Reference

This document catalogs every `.RDS` file in `data/`: its grain (what one
row means), its columns and types, which app tab consumes it, and how it
is loaded.

All schemas were verified by reading the actual files with
`readRDS()` on R 4.4.1 against the FFY 2023 release.

---

## 3.1 File inventory

| File | Grain | Rows × Cols (sample) | Used by |
|------|-------|----------------------|---------|
| `pivot_table_<year>.RDS` (×7, one per FFY 2017–2023) | one row per (case, error finding) | e.g., 2023 = 14,110 × 12 | **Pivot Table** tab |
| `snap_error_<year>.RDS` (×7) | one row per (case, error finding) | e.g., 2023 = 14,110 × 8 | **Error Pathways** (Sankey) and **Error Severity** tabs |
| `snap_demographics_<year>.RDS` (×7) | one row per (case, error finding) joined with household demographics | e.g., 2023 = 11,503 × 17 | **Error Demographics** tab |
| `base_case_2017_2023.rds` | one row per *reviewed case* across all 7 years | 254,637 × 7 | **Base Rates** tab (denominator) |
| `base_cat_2017_2023.rds` | one row per (reviewed case × error category) across all 7 years | 270,959 × 13 | **Base Rates** tab (numerator) |
| `pivot_table_data.RDS` | legacy/precursor combined file | 72,072 × 13 | **Not used by the current app** — safe to delete after verifying no external script depends on it. |

> **Why the row counts differ between `pivot_table_<year>.RDS` and
> `snap_demographics_<year>.RDS`.** The demographics frame drops rows where
> any of the demographic columns are NA (`drop_na(...)` calls in
> `app.R:117–128`). Net effect for FFY 2023: 14,110 → 11,503.

---

## 3.2 `pivot_table_<year>.RDS`

**Grain.** One row per (reviewed case × error finding). A single case may
appear multiple times if it had multiple errors.

**Columns** (in the order they appear in the file):

| Column | Type | Notes |
|---|---|---|
| `case_id` | integer | Case identifier within the year (NOT globally unique across years; the app keys on `(case_id, year)`). |
| `agency` | character | Internal flag; **dropped** in the app via `select(-agency)` (`app.R:39`). |
| `Type` | character | Error Type — *why* the error happened. Free-text label, e.g. `"Computer programming error"`. |
| `Nature` | character | Error Nature — *how* the error happened. |
| `Element` | character | Error Element — *what* program area the error is in. |
| `Time Period` | character | When the underlying event occurred (renamed to `Error Timing` in `app.R:47`). |
| `Verification` | character | How the error was discovered (renamed to `Error Discovery` in `app.R:46`). NA values are recoded to `"Other"` (`app.R:51`). |
| `Dollar Amount in Error` | integer | Absolute dollar amount in error for this finding. |
| `State` | character | State name, full English (e.g. `"Connecticut"`). 53 jurisdictions. |
| `Status of Error Findings` | character | One of `"Underissuance"`, `"Overissuance"`, `"Amount correct"`. |
| `Action Type` | character | Renamed to `Case Type` in `app.R:48`. Values include `"Certification"`, `"Recertification"`. |
| `Responsibility` | character | Renamed to `Error Responsibility` in `app.R:45`. One of `"Client Errors"`, `"Agency Errors"`, `"Technical Errors"`. |

**How the app loads it.** `app.R` lines 22–58:

```r
verification_2023 <- readRDS("data/pivot_table_2023.RDS") %>%
                       mutate(year = rep(2023, nrow(.)))   # one per year
verification_all  <- bind_rows(verification_2017, ..., verification_2023) %>%
  select(-agency) %>%
  rename(`Case ID` = case_id,
         `Error Discovery` = Verification,
         `Error Responsibility` = Responsibility,
         `Error Type` = Type, `Error Nature` = Nature, `Error Element` = Element,
         `Error Timing` = `Time Period`, `Case Type` = `Action Type`,
         `Year` = year) %>%
  mutate(`Error Discovery` = ifelse(is.na(`Error Discovery`), "Other", `Error Discovery`),
         threshold = unname(threshold_by_year[as.character(Year)]),
         over_threshold = if_else(!is.na(`Dollar Amount in Error`)
                                  & `Dollar Amount in Error` > threshold, 1L, 0L))
```

The result, `verification_all`, is the **canonical "case–error" frame**
used by the Pivot Table tab.

---

## 3.3 `snap_error_<year>.RDS`

**Grain.** One row per (case × error finding) — same grain as
`pivot_table_<year>.RDS` but a leaner schema. Created upstream from the
same source rows; the row counts match.

**Columns:**

| Column | Type | Notes |
|---|---|---|
| `case_id` | integer | |
| `agency` | character | Not used downstream. |
| `Type` | character | Error Type. |
| `Nature` | character | Error Nature. |
| `Element` | character | Error Element. |
| `dollar_amount` | integer | Renamed to `Dollar Amount in Error` in `app.R:82`. |
| `State` | character | |
| `Status of Error Findings` | character | |

**How the app loads it.** `app.R` lines 67–98:

```r
df_error_2023 <- readRDS("data/snap_error_2023.RDS") %>% mutate(year = 2023)
# ... per year ...
df_error <- do.call("rbind", list(df_error_2017, ..., df_error_2023)) %>%
  rename(`Dollar Amount in Error` = dollar_amount) %>%
  mutate(threshold = unname(threshold_by_year[as.character(year)]),
         over_threshold = if_else(!is.na(`Dollar Amount in Error`)
                                  & `Dollar Amount in Error` > threshold, 1L, 0L))

# Build a case-level "any error over threshold?" flag, used to attach the
# threshold filter to the demographics frame (which has no dollar column).
error_threshold_flags <- df_error %>%
  group_by(case_id, year) %>%
  summarise(over_threshold = max(over_threshold, na.rm = TRUE), .groups = "drop")
```

The combined `df_error` frame drives the **Sankey** and **Error Severity**
plots.

---

## 3.4 `snap_demographics_<year>.RDS`

**Grain.** One row per (case × error finding), with the household's
demographic columns appended.

**Columns:**

| Column | Type | Notes |
|---|---|---|
| `case_id` | integer | Joined back to `error_threshold_flags` to attach `over_threshold`. |
| `agency` | character | Unused downstream. |
| `error_type` | character | Lowercase column name (`error_type` not `Error Type`). |
| `error_nature` | character | |
| `error_element` | character | |
| `Race` | character | |
| `Gender` | character | |
| `Employment` | character | |
| `Age` | factor | Stored as a factor with ordered age bands (e.g., `"Late Midlife (40-59)"`). |
| `Disability (Household Head)` | character | |
| `Disability (Household Member)` | character | |
| `Unit Composition` | character | e.g. `"Child(ren) and one female adult"`. |
| `Homelessness` | character | |
| `Expedited Service` | character | |
| `Application or Renewal` | character | |
| `Status of Error Findings` | character | |
| `State` | character | |

**How the app loads it.** `app.R` lines 101–132:

```r
df_2023 <- readRDS("data/snap_demographics_2023.RDS") %>% mutate(year = 2023)
# ... per year ...
df <- do.call("rbind", list(df_2017, ..., df_2023))

df <- df %>%
  drop_na(Employment) %>% drop_na(Race) %>% drop_na(Gender) %>%
  drop_na(Age) %>% drop_na(`Disability (Household Member)`) %>%
  drop_na(`Disability (Household Head)`) %>% drop_na(`Unit Composition`) %>%
  drop_na(`Homelessness`) %>% drop_na(`Expedited Service`) %>%
  drop_na(`Application or Renewal`) %>% drop_na(`Status of Error Findings`)

df <- df %>%
  left_join(error_threshold_flags, by = c("case_id", "year")) %>%
  mutate(over_threshold = coalesce(over_threshold, 0L))
```

The combined `df` frame drives the **Error Demographics** bar chart.

> **Caveat for analysts:** the `drop_na()` chain removes any case whose
> demographic record is incomplete on *any* of the listed dimensions.
> For FFY 2023 this drops about 18% of the case–error rows. If you ever
> need exact case counts that line up with the other tabs, do not use
> `df` — use `df_error` or `verification_all`.

---

## 3.5 `base_case_2017_2023.rds`

**Grain.** One row per *reviewed case*, across all 7 years. This is the
**denominator** for the Base Rates tab.

**Columns:**

| Column | Type | Notes |
|---|---|---|
| `Case ID` | integer | Case identifier. **Cast to character** in the app (`app.R:1789`) before any join. |
| `State` | character | |
| `Status of Error Findings` | character | `"Overissuance"`, `"Underissuance"`, `"Amount correct"`. |
| `Case Type` | character | `"Certification"`, `"Recertification"`. |
| `Year` | numeric | Federal fiscal year. |
| `has_error` | integer | `1` if the case has at least one recorded error finding, else `0`. |
| `Dollar Amount in Error` | numeric | Total dollar amount in error for the case (sum across error rows). `0` for cases with no error. Used to compute the case-level `over_threshold` flag. |

**Row distribution by year** (from the actual file):

```
 2017   2018   2019   2020   2021   2022   2023
45530  43738  43258  27112   9832  41391  43776
```

(2020 and 2021 are smaller because of pandemic-era reductions in QC
review activity.)

**How the app loads it.** `app.R` lines 63 and 789–797:

```r
base_case_all <- readRDS("data/base_case_2017_2023.rds")
# inside server():
base_case_all <- base_case_all %>%
  dplyr::mutate(
    threshold = unname(threshold_by_year[as.character(Year)]),
    over_threshold = if_else(
      has_error == 1 & !is.na(`Dollar Amount in Error`)
                    & `Dollar Amount in Error` > threshold,
      1L, 0L))
```

---

## 3.6 `base_cat_2017_2023.rds`

**Grain.** One row per *(reviewed case × error category)*. A case with
two distinct error findings produces two rows here. The Base Rates tab
joins this against `base_case_*` to compute "share of reviewed cases that
had error X."

**Columns:**

| Column | Type | Notes |
|---|---|---|
| `Case ID` | integer | Cast to character before joining. |
| `Error Type` | character | |
| `Error Discovery` | character | |
| `Error Responsibility` | character | |
| `Error Nature` | character | |
| `Error Element` | character | |
| `Error Timing` | character | |
| `has_this_error` | integer | Always `1` (rows in this file are by definition errors); used as a sentinel column. |
| `State` | character | |
| `Case Type` | character | |
| `Status of Error Findings` | character | |
| `Year` | numeric | |
| `ErrorGroup` | character | Coarse rollup label. |

**How the app loads it.** `app.R` line 64:

```r
base_cat_all <- readRDS("data/base_cat_2017_2023.rds")
```

The Base Rates tab applies its filters in two places — once on the
*case* side (to size the denominator) and once on the *category* side
(to filter the numerator). See [`06-key-modules.md`](06-key-modules.md),
section "Base Rates joins."

---

## 3.7 `pivot_table_data.RDS` (legacy)

A single combined file with all 7 years (72,072 rows × 13 cols). This file
is **not referenced anywhere in `app.R`**; it appears to be a pre-cursor
that was superseded by the per-year `pivot_table_<year>.RDS` files. Leave
it in place until you have confirmed that no external notebook or analysis
script depends on it; otherwise it can be deleted to shrink the deployment
bundle.

---

## 3.8 Naming conventions and traps

- **Year column casing.** `verification_all` uses `Year` (capitalized);
  `df_error`, `df`, and the per-year objects use `year` (lowercase). The
  threshold lookup uses `as.character(Year)` / `as.character(year)` so be
  careful when copying code between tabs.
- **Case ID type.** `case_id` / `Case ID` is **integer** in every `.RDS`.
  The Base Rates code defensively casts to character before every join
  (`app.R:1789, 1798, 1917, 1929`) because joins on integer can silently
  drop with mixed types.
- **`agency` column.** Present in `pivot_table_*` and `snap_error_*` but
  unused — the app drops or ignores it.
- **State count.** 53 distinct states (50 states + DC + 2 territories).
  Build state filter pickers from the data, never from a hard-coded list.
- **Per-year file extensions.** `pivot_table_<year>.RDS` and
  `snap_error_<year>.RDS` use uppercase `.RDS`; `base_case_*.rds` and
  `base_cat_*.rds` use lowercase `.rds`. R is case-insensitive on Windows
  but case-sensitive on Linux (which is what shinyapps.io runs). Keep the
  exact case used today when adding new files.
