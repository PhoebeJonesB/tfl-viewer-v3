# Plan: TFL Viewer v3 — ARS-Compliant App with Variant Creator

**Date:** 2026-03-13  
**Scope:** Build `tfl-viewer-v3` from v2, adding ARS compliance, ARS JSON export, frontend TFL variant creation, and a README.

---

## What Is Being Built

Three interconnected features layered on top of v2:

1. **ARS Compliance** — extend the `tfl_metadata` schema with structured ARS fields; auto-derive `filter_code` from them; have `runner.R` use them to pre-filter data.
2. **ARS JSON Export** — a download button that emits a standards-compliant ARS JSON file capturing the current TFL + all active user filter selections.
3. **Frontend Variant Creator** — a UI workflow where a user applies filters, clicks "Save as Variant", names it, and the app writes a new `tfl_*_b.R` script + its ARS JSON to disk. The variant appears in the registry on the next Refresh.
4. **README.md** — top-level markdown file with CDISC ARS links, motivation, and v3 feature overview.

---

## File Structure (v3)

```
tfl-viewer-v3/
├── global.R                  # Updated: sources ars.R; loads jsonlite
├── ui.R                      # Updated: ARS export button, Variant Creator modal
├── server.R                  # Updated: export + variant server logic
├── README.md                 # NEW
├── tfl-viewer-v3.Rproj
│
├── R/
│   ├── data_gen.R            # Unchanged from v2
│   ├── runner.R              # Updated: ARS metadata validation + analysis_sets pre-filter
│   ├── utils.R               # Updated: ars_to_filter_code() helper
│   └── ars.R                 # NEW: ARS JSON builder
│
├── tfls/
│   ├── _template.R           # Updated: full ARS metadata template
│   ├── tfl_t14_1_1.R         # Updated: ARS metadata fields added
│   ├── tfl_t14_3_1.R         # Updated: ARS metadata fields added
│   ├── tfl_f14_1_1.R         # Updated: ARS metadata fields added
│   └── tfl_f14_2_1.R         # Updated: ARS metadata fields added
│
└── ars_outputs/              # NEW: generated ARS JSON files land here
    └── .gitkeep
```

---

## Implementation Steps

### Phase 1 — ARS Infrastructure

**Step 1 — Create `R/ars.R`**

New module with two exported functions:

- `build_ars_json(tfl_result, active_filters, study_id, timestamp)` — builds a named R list
  matching the ARS object model:
  - `reportingEvent` → top-level container (study id, generated timestamp)
  - `outputs[]` → one entry per TFL (id, name, outputType, displayTitle, subtitle, footnotes, fileSpec)
  - `analyses[]` → from `tfl_metadata$analyses` (id, label, analysisSetId, dataset, variable, groupingVar, methodId)
  - `analysisSets[]` → **union** of `tfl_metadata$analysis_sets` + active user filters from the sidebar
  - `dataSubsets[]` → from `tfl_metadata$data_subsets`
  - `methods[]` → from `tfl_metadata$methods`
  - `userSession` → baseTflId, timestamp, list of applied user filters (for full traceability)

- `ars_to_json_string(ars_obj)` — serialises via `jsonlite::toJSON(ars_obj, pretty = TRUE, auto_unbox = TRUE)`

The key design point: `analysisSets` in the exported JSON is the union of what the script declared AND
what the user applied in the UI. A download therefore always fully documents what population was actually analysed.

---

**Step 2 — Add `ars_to_filter_code()` to `R/utils.R`**

```r
ars_to_filter_code <- function(dataset_name, analysis_sets, data_subsets = NULL)
```

Converts structured `analysis_sets` + `data_subsets` into a dplyr `filter()` code string.
Comparator map: `EQ → "=="`, `NE → "!="`, `IN → "%in% c(...)"`, `NOTIN → "!... %in% c(...)"`.

Used by `runner.R` to auto-populate `filter_code` when the script omits it.

---

**Step 3 — Update `R/runner.R`**

Three additions:

a. **`validate_ars_metadata(metadata)`** — checks required base fields (id, name, type, datasets,
   description) are present; fills in `NULL` defaults for all optional ARS fields so downstream
   code can always assume a consistent shape. No error thrown for missing optional fields.

b. **Auto-apply `analysis_sets` as pre-filters** — if `metadata$analysis_sets` is non-NULL after
   validation, apply those conditions to the injected ADaM data *before* the script body runs.
   This means scripts no longer need to hardcode `filter(SAFFL == "Y")` — the metadata
   declaration is sufficient. Scripts that still hardcode the filter continue to work
   (they filter already-filtered data, which is idempotent).

c. **Auto-derive `filter_code`** — if the executed script did not define `filter_code` but
   `analysis_sets` is present, call `ars_to_filter_code()` to generate it automatically.

---

### Phase 2 — Update TFL Scripts

**Step 4–7 — Update all four existing TFL scripts** with full ARS metadata blocks.
The existing filter/output logic in each script is left entirely unchanged — ARS fields are additive.

| Script | `analysis_sets` | `data_subsets` | grouping_var |
|---|---|---|---|
| `tfl_t14_1_1.R` | ITTFL == "Y" | none | TRT01A |
| `tfl_t14_3_1.R` | SAFFL == "Y" | distinct per subj/PT noted | TRT01A |
| `tfl_f14_1_1.R` | SAFFL == "Y", ANL01FL == "Y" | PARAMCD == "ALT", post-baseline | TRT01A |
| `tfl_f14_2_1.R` | SAFFL == "Y" | PARAMCD == "TTFAE" | TRT01A |

**Step 8 — Update `_template.R`** with the full ARS metadata skeleton, all optional fields
included as commented-out examples.

---

### Phase 3 — ARS JSON Export Feature

**Step 9 — Add "Export ARS JSON" download button**

Location: **Filter Code tab**, below the Live Filter Code panel (natural position — this is
where the active filter state is already displayed to the user).

```r
# UI
downloadButton("btn_export_ars", "Export ARS JSON", icon = icon("file-code"))

# Server
output$btn_export_ars <- downloadHandler(
  filename = function() paste0("ars_", current_tfl()$metadata$id,
                               "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json"),
  content  = function(file) {
    ars <- build_ars_json(current_tfl(), active_pop_flags(), STUDY_ID)
    writeLines(ars_to_json_string(ars), file)
  }
)
```

The exported JSON always captures the **current sidebar filter state**, so users can document
exactly what population they were looking at when they exported.

---

### Phase 4 — Frontend Variant Creator

A variant is a first-class TFL: it gets its own `.R` file in `tfls/`, its own registry row,
and its own ARS JSON. The variant inherits the base TFL's output-generation code unchanged.
Only the metadata and analysis population differ. Variants are append-only — the app writes
new files; it never modifies existing ones.

**Step 10 — "Save as Variant" button in TFL Viewer sidebar**

```r
actionButton("btn_save_variant", "Save as Variant", icon = icon("code-fork"))
```

Shown only when a TFL is selected and at least one population filter is active (i.e. not "All").

**Step 11 — Variant Creator modal (`modalDialog`)**

| Field | Input | Notes |
|---|---|---|
| Base TFL | Static text | Read-only; shows base id |
| Variant suffix | `textInput` | Default: next available letter (a, b, c…) |
| Variant label | `textInput` | e.g. "Elderly Subgroup (Age ≥ 65)" |
| Description | `textAreaInput` | Pre-filled from base; editable |
| Filters to bake in | `checkboxGroupInput` | Lists each active filter; user can deselect any |
| Confirm | `actionButton("btn_confirm_variant", "Create Variant")` | |

A warning note in the modal: *"This writes a new file to `tfls/`. Intended for development
environments — do not use on shared deployments without access controls."*

**Step 12 — Variant creation server logic**

`generate_variant_script(base_result, variant_id, variant_label, variant_desc, baked_filters)`:

1. Read the base TFL's `script_lines`.
2. Locate the `tfl_metadata <- list(...)` block by line number.
3. Build a replacement metadata block with:
   - `id` = `variant_id` (e.g. `"t14_1_1_b"`)
   - `name` updated to include variant label
   - `description` updated
   - `analysis_sets` = base `analysis_sets` merged with baked_filters
   - `variant_of` = base TFL id (provenance field)
4. Prepend a comment header: auto-generated, base TFL, date, baked filters.
5. Replace the metadata block in the script lines; write to `tfls/tfl_{variant_id}.R`.
6. Call `build_ars_json()` → write to `ars_outputs/{variant_id}.json`.
7. Show a `showNotification()` success toast.
8. Auto-trigger `rv$tfl_list` refresh so the new variant appears in the registry immediately.

Helper for suffix suggestion:
```r
next_variant_suffix <- function(base_id, tfl_dir) {
  existing <- list.files(tfl_dir, pattern = paste0("^tfl_", base_id, "_[a-z]\\.R$"))
  if (length(existing) == 0L) return("a")
  used <- sub(paste0(".*", base_id, "_([a-z])\\.R$"), "\\1", existing)
  letters[max(match(used, letters)) + 1]
}
```

---

### Phase 5 — README.md

**Step 13 — Create `README.md`** at the project root with these sections:

1. **What is TFL Viewer v3?** — one-paragraph pitch
2. **Why does this matter?** — traceability argument: regulatory submissions require results
   traceable back to data and methods; ARS makes that machine-readable; this app makes ARS
   accessible without leaving R/Shiny; useful for programmers, biostatisticians, reviewers,
   and auditors
3. **CDISC ARS Standard** — with links:
   - CDISC ARS landing page: https://www.cdisc.org/standards/foundational/ars
   - CDISC ARS GitHub spec: https://github.com/cdisc-org/analysis-results-standard
   - CDISC Library API: https://www.cdisc.org/cdisc-library
4. **What's New in v3** — bullet list
5. **Quick Start** — install, run
6. **TFL Script Contract** — updated table: required vs optional ARS fields
7. **ARS JSON Export** — how-to
8. **Variant Creator** — how-to
9. **Package Dependencies** — updated table including `jsonlite`

---

## New Package Dependency

One new package required (add to `global.R` and README install instructions):

```r
install.packages("jsonlite")
```

All other v3 features use packages already in v2.

---

## What Is NOT Changing

- `R/data_gen.R` — unchanged
- The four-object TFL script contract — fully backward compatible; all v2 scripts load without modification
- The three-panel UI structure (Registry / Viewer / About)
- All existing v2 functionality (population filters, dataset explorer, re-run, live filter code)

---

## Implementation Order Summary

| # | File | Action |
|---|---|---|
| 1 | `R/ars.R` | Create new |
| 2 | `R/utils.R` | Add `ars_to_filter_code()` |
| 3 | `R/runner.R` | Add `validate_ars_metadata()`, auto-pre-filter, auto-filter_code |
| 4 | `tfls/tfl_t14_1_1.R` | Add ARS metadata fields |
| 5 | `tfls/tfl_t14_3_1.R` | Add ARS metadata fields |
| 6 | `tfls/tfl_f14_1_1.R` | Add ARS metadata fields |
| 7 | `tfls/tfl_f14_2_1.R` | Add ARS metadata fields |
| 8 | `tfls/_template.R` | Update to ARS metadata template |
| 9 | `ars_outputs/.gitkeep` | Create directory marker |
| 10 | `global.R` | Add `library(jsonlite)`, source `R/ars.R` |
| 11 | `ui.R` | Add Export ARS button; Variant Creator button + modal |
| 12 | `server.R` | Add export + variant creation handlers |
| 13 | `README.md` | Create |
