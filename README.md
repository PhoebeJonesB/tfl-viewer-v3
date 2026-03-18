# Clinical TFL Viewer v3.1

An ARS-compliant interactive R Shiny application for exploring, filtering, and generating TFLs from CDISC ADaM datasets — with built-in review workflow and audit trail.

---

## Why Does This Matter?

Clinical trial submissions to regulators (FDA, EMA, PMDA) require that analysis results are fully traceable back to the data and methods that produced them. Traditionally, this traceability lives only in scripts a reviewer must manually inspect.

The **CDISC Analysis Results Standard (ARS)** provides a machine-readable format encoding who was analysed, which variable, grouped how, and by which statistical method. This enables:

- Automated validation of outputs against the protocol
- Regulatory system ingestion without re-running code
- Full provenance for sensitivity analyses and subgroup variants
- Cell-level traceability: trace any result back to its population and method

**TFL Viewer v3** brings ARS into an interactive Shiny app. Every TFL carries structured ARS metadata, active filters export as ARS JSON, and new TFL variants can be created and documented from the browser.

---

## CDISC ARS Standard

| Resource | Link |
|---|---|
| CDISC ARS Landing Page | https://www.cdisc.org/standards/foundational/ars |
| ARS GitHub Specification | https://github.com/cdisc-org/analysis-results-standard |
| CDISC Library API | https://www.cdisc.org/cdisc-library |
| CDISC Glossary | https://www.cdisc.org/standards/glossary |

---

## What Is New in v3

| Feature | Description |
|---|---|
| **ARS metadata schema** | TFL scripts carry structured analysis_sets, data_subsets, analyses, methods, and display fields |
| **Auto-pre-filtering** | runner.R reads analysis_sets and pre-filters ADaM data before the script body runs |
| **Auto filter_code** | Derived from analysis_sets and data_subsets when omitted by the script |
| **Export ARS JSON** | Download the current TFL + active sidebar filters as a CDISC ARS-compliant JSON file |
| **Variant Creator** | Apply filters, click Save as Variant, app writes a new TFL script + ARS JSON to disk |
| **Registry enhancements** | Registry table now shows Population and Grouped By columns from ARS metadata |

All v2 scripts load without modification. ARS fields are optional.

---

## What Is New in v3.1

| Feature | Description |
|---|---|
| **Source Data tab** | View the `analysis_data` — the actual filtered dataset that produced each TFL output |
| **Review comment system** | Reviewers leave comments per TFL, persisted as JSON in `reviews/`, loaded when the TFL is viewed |
| **Role-based audit trail** | Comments tagged with reviewer role (Statistics Lead, Safety Lead, Medical Lead, Programming Lead, Clinical Operations Lead, Other); absence of comment = not reviewed |
| **Threaded responses** | Reply to any comment; responses linked to parent in UI and in the Excel export via `Thread_ID` |
| **Audit status bar** | Per-TFL badge display showing which roles have reviewed (green) and which have not (grey) |
| **Review Status in Registry** | Registry table now shows "N/6 reviewed" for each TFL at a glance |
| **Excel audit export** | Download a two-sheet workbook: Review Log (all comments with thread linkage) + Audit Summary (one row per TFL, one column per role) |

No new package dependencies. Uses `jsonlite` and `writexl` from v3.0.

---

## Review Workflow

1. Open a TFL in the Viewer and navigate to the **Reviews** tab
2. Enter your name and select your role (persists for the session)
3. Write a comment and click **Submit Comment**
4. To respond to an existing comment, click its **Reply** button
5. The **Audit Status** bar shows which reviewer roles have commented
6. Click **Download Review Audit (Excel)** to export all reviews across all TFLs

Comments are stored as JSON files in the `reviews/` directory (one file per TFL).

---

## Quick Start



---

## TFL Script Contract

| Object | Required | Description |
|---|---|---|
| tfl_metadata | Yes | Named list with base + optional ARS fields |
| source_datasets | Yes | Named list of ADaM data frames |
| filter_code | Optional (v3) | Auto-derived from analysis_sets if omitted |
| tfl_output | Yes | data.frame or ggplot |

### Required base fields

| Field | Example |
|---|---|
| id | "t14_1_1" |
| name | "Table 14.1.1 - Subject Disposition" |
| type | "Table" / "Figure" / "Listing" |
| datasets | c("adsl", "adae") |
| description | One sentence summary |

### Optional ARS extension fields (v3)

| Field | ARS Concept | Description |
|---|---|---|
| display | Output.displays | title, subtitle, footnotes |
| analysis_sets | AnalysisSet + WhereClause | Population conditions |
| data_subsets | DataSubset | Record-level filters |
| analyses | Analysis | Variable, population, grouping, method linkage |
| methods | AnalysisMethod | Statistical method and operations |
| variant_of | Provenance | Base TFL id (auto-set on generated variants) |

---

## ARS JSON Export

In the **Filter Code** tab, click **Export ARS JSON**. The file captures:

- Output metadata (id, name, type, display title, footnotes)
- analysisSets: script-declared populations merged with any user sidebar filters
- dataSubsets, analyses, methods from the TFL metadata
- userSession block: timestamp, base TFL id, list of user-applied filters

---

## Variant Creator

1. Select a TFL and apply population flag filters in the sidebar
2. Click **Save as Variant** (appears when at least one filter is active)
3. Enter a suffix (a, b, c...), a short label, choose filters to bake in
4. Click **Create Variant**

The app writes:
- tfls/tfl_{base_id}_{suffix}.R with baked-in analysis_sets
- ars_outputs/{id}.json capturing the variant as an ARS reporting event

Click **Refresh** in the Registry to load it.

> Note: Variant creation writes files to disk. Intended for development environments only.

---

## Connecting to Real Study Data



---

## Package Reference

| Package | Purpose |
|---|---|
| shiny | Reactive web framework |
| bslib | Bootstrap 5 UI |
| DT | Interactive DataTables |
| dplyr | Data manipulation |
| ggplot2 | Figures and exploratory plots |
| survival | Kaplan-Meier estimation |
| scales | Axis formatting |
| writexl | Excel export |
| forcats | Frequency-ordered factors |
| jsonlite | ARS JSON serialisation (v3) |
| stringr | String utilities for variant script generation (v3) |

---

---

## Changelog

### v3.1.0 — 2026-03-18

- **Source Data tab**: view the filtered `analysis_data` behind each TFL
- **Review system**: role-tagged comments per TFL, persisted as JSON
- **Audit trail**: per-role review status badges; Excel audit export (Review Log + Audit Summary)
- **Threaded responses**: reply to comments with parent linkage
- **Registry**: new "Review Status" column
- New file: `R/reviews.R`
- Updated: `R/runner.R` (captures `analysis_data`), `global.R`, `ui.R`, `server.R`

### v3.0.0 — 2026-03-13

- ARS-compliant metadata schema for all TFLs
- ARS JSON export
- Frontend Variant Creator
- Auto-pre-filtering and auto-derived filter_code from analysis_sets
- New files: `R/ars.R`, `ars_outputs/`
- New dependency: `jsonlite`, `stringr`

### v2.0.0 — 2026-03-13

- Initial release: TFL Registry, Viewer, Dataset Explorer, Investigation tools, Filter Code, Re-run with Filters

---

*TFL Viewer v3.1 — built on CDISC ADaM and ARS standards.*
