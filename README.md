# ChesICAT-outputs

Comparison plots for ChesICAT Ecopath / Ecosim / Ecospace model runs.

## Objectives

Ecopath with Ecosim (EwE) writes each Ecospace scenario to its own folder
of CSVs. This repo turns those raw outputs into apples-to-apples visual
comparisons across scenarios, against the Ecosim fit and — for the
time-series view — the observed fitted timeseries.

Two complementary views are provided:

| Script | View | Answers |
|---|---|---|
| `plot-outputs-overtime.R` | Time-series line plots | *How does each scenario's trajectory evolve over the full run?* |
| `plot-outputs-finalyears.R` | Bar-and-whisker of the last N years | *At steady state, how do the scenarios differ?* |

Both scripts share the same input directory conventions, the same
scenario auto-detection, and the same panel layout, so the two PDFs are
directly cross-referenceable.

## Directory layout

```text
ChesICAT-outputs/
├── functions.R                    shared helpers (CSV parsing, name cleanup)
├── plot-outputs-overtime.R        time-series comparison plots
├── plot-outputs-finalyears.R      final-N-years bar-and-whisker comparison
├── ChesICAT-outputs.Rproj         RStudio project (sets working directory)
├── ewe-outputs/                   (git-ignored) drop model outputs here
│   ├── timeseries/
│   │   └── ts_v1.4.csv              observed fitted timeseries
│   └── model-setups/
│       ├── basic_estimates.csv      GroupNo → Group lookup
│       ├── ecosim_<sim_scenario>/   Ecosim outputs (one folder)
│       └── spa_<name>/              Ecospace scenarios (any number)
├── scenario-comparisons/          PDF outputs land here
└── hoard/                         (git-ignored) archived scratch
```

**Scenario auto-detection.** Any subfolder of `ewe-outputs/model-setups/`
whose name starts with `spa_` is picked up as an Ecospace scenario. To
add a run, drop the folder and re-run — no other edits required.

## Prerequisites

- R (tested with R 4.5). Only external package: `dplyr`.
- Ecospace CSVs must include the standard EwE header block; the scripts
  skip metadata rows dynamically via `f.find_start_line()` in
  `functions.R`.

## Getting model outputs in place

Because `ewe-outputs/` is git-ignored, populate it manually from your EwE
model. The scripts expect these files:

**Ecosim** (inside `ewe-outputs/model-setups/ecosim_<sim_scenario>/`):

- `biomass_annual.csv`, `catch_annual.csv`
- `biomass_monthly.csv`, `catch_monthly.csv`
- `catch-fleet-group_annual.csv`

**Ecospace** (per `spa_<name>/` folder):

- `Ecospace_Annual_Average_Biomass.csv`
- `Ecospace_Annual_Average_Catch.csv`
- `Ecospace_Average_Biomass.csv`
- `Ecospace_Average_Catch.csv`

**Shared:**

- `ewe-outputs/timeseries/ts_v1.4.csv` (path is user-set)
- `ewe-outputs/model-setups/basic_estimates.csv` (functional-group lookup)

## Running

Open `ChesICAT-outputs.Rproj` in RStudio (this sets the working directory
to the repo root) and source either script:

```r
source("plot-outputs-overtime.R")
source("plot-outputs-finalyears.R")
```

Or from a terminal at the repo root:

```text
Rscript plot-outputs-finalyears.R
```

Each script first prints an auto-detected scenario table. Verify the
`ecosim_scen`, `timeseries`, `start_year`, `n_years`, and `map` columns
match your expectations before it plots — scenarios whose annual run
length differs from Ecosim are skipped with a `message()`.

### User-editable knobs (at the top of each script)

| Variable | Script | Default | Meaning |
|---|---|---|---|
| `ewe_out_fold` | both | `"ewe-outputs/model-setups"` | Root folder for model runs |
| `sim_scenario` | both | `"ecosim_sim_01.3_SM2-fit"` | Ecosim subfolder name |
| `obs_TS_name` | both | `"ewe-outputs/timeseries/ts_v1.4.csv"` | Observed timeseries CSV |
| `srt_year` | both | `2001` | Simulation start year |
| `scen_labels` | both | (2 entries) | Folder-name → legend-label overrides |
| `dir_out` | both | `"./scenario-comparisons/model-setups/"` | PDF output folder |
| `append_pdfs` | both | `TRUE` | Combined PDF vs. two separate PDFs |
| `init_years_toscale` | both | `1` | Number of leading years used as scaling denominator |
| `final_n_years` | finalyears | `5` | Number of trailing years to summarize |
| `scale_to_init` | finalyears | `TRUE` | Divide each series by its own initial-years mean |
| `overlay_points` | finalyears | `TRUE` | Overlay individual annual values on each bar |
| `pdf_ncol` / `pdf_nrow` | both | `4` / `8` | Page tiling (letter portrait) |

## Outputs

PDFs are written to `./scenario-comparisons/model-setups/`.

With `append_pdfs = TRUE` (default):

- `ecospace_out_xY.PDF` — over-time biomass + catch, combined
- `ecospace_out_final.PDF` — final-years bar-and-whisker, combined

With `append_pdfs = FALSE`:

- `BxY_scaled.PDF`, `CxY_by_fleet-group_scaled.PDF` (over-time)
- `Bfinal_scaled.PDF`, `Cfinal_by_fleet-group_scaled.PDF` (final-years)

Panel content:

- **Biomass panels:** one per functional group.
- **Catch panels:** one per `fleet | group` column, filtered to those with
  non-zero Ecosim catch (keeps the PDF from filling with empty panels).
- **Legend panel:** the first slot on each page.

## Workflow / how to extend

- **Add a new Ecospace scenario.** Drop it in
  `ewe-outputs/model-setups/spa_<name>/`, optionally add an entry to
  `scen_labels` at the top of each script to give it a nicer legend
  label, and re-run.
- **Change the scaling window** (both scripts): edit `init_years_toscale`.
- **Change the summary window** (finalyears only): edit `final_n_years`.
- **Flip to raw units** (finalyears only): set `scale_to_init <- FALSE`.
- **Change page tiling:** edit `pdf_ncol` / `pdf_nrow`.

### Shared helpers (`functions.R`)

- `f.find_start_line(filename, flag)` — locates the data-start row in EwE
  CSVs by matching the first field of each line (e.g. `"Year"`,
  `"TimeStep"`, or a numeric year).
- `f.read_ecosim_timeseries(filename, num_row_header = 5)` — reads the
  observed fitted timeseries and splits into biomass/catch data frames
  plus their header metadata.
- `f.standardize_group_names(names)` — cleans EwE functional-group column
  names (collapses double dots, converts `.yr` to `+yr`, etc.).

## Housekeeping

- `hoard/` is a git-ignored scratch area for archived scripts and old
  PDFs.
- License: CC0 (see `LICENSE`).
