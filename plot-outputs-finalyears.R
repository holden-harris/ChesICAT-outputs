## Naming conventions
## 'sim' --> Related to Ecosim
## 'spa' --> Related to Ecospace
## 'obs' --> Related to observed timeseries data, i.e., that Ecosim was fitted to.
## 'B'   --> Denotes biomass
## 'C'   --> Denotes catch
##
## Purpose:
## Companion to plot-outputs-overtime.R. Instead of one panel per FG (or
## fleet|group) showing biomass/catch as time series, each panel here
## summarizes the final `final_n_years` years of each scenario as a
## bar (mean) + whisker (min-max) sitting side-by-side, so scenarios can
## be compared at steady-state without eye-balling the right edge of a
## time-series line.

## NOTE: relative paths below assume the working directory is the repo root.
## Opening ChesICAT-outputs.Rproj in RStudio sets this automatically.

## Setup -----------------------------------------------------------------------
#rm(list=ls())
source("./functions.R") ## Pull in functions
library(dplyr)

## Input set up ----------------------------------------------------------------
## Swap `scen_group` to point both the input folder (ewe-outputs/<scen_group>/)
## and the output folder (scenario-comparisons/<scen_group>/) at a different
## exploratory scenario set. `sim_scenario` and `obs_TS_file` are separate
## knobs because they can't always be inferred from `scen_group` alone.
scen_group   <- "incr-comm-harvest"
sim_scenario <- "ecosim_sim_comm-harv_stat-quo"
obs_TS_file  <- "ts_v1.5_statusquo2037.csv"
srt_year     <- 2001

ewe_out_fold <- file.path("ewe-outputs", scen_group)
obs_TS_path  <- file.path("ewe-outputs", "timeseries", obs_TS_file)

## Auto-detect Ecospace scenarios --------------------------------------------
## Any subfolder under ewe_out_fold whose name starts with "spa_" is treated
## as an Ecospace scenario. Metadata is read from each scenario's
## Ecospace_Annual_Average_Biomass.csv header so the user can confirm each
## run's provenance (Ecosim scenario, timeseries file, start year, run length,
## and Ecospace map dimensions) before plotting.
spa_scenarios <- list.dirs(ewe_out_fold, recursive = FALSE, full.names = FALSE)
spa_scenarios <- spa_scenarios[grepl("^spa_", spa_scenarios)]
if (length(spa_scenarios) == 0) {
  stop("No Ecospace scenarios (spa_*) found under '", ewe_out_fold,
       "'. Check scen_group.")
}

## Read a small set of metadata fields from the CSV header block.
f.read_scenario_meta <- function(folder){
  fp <- file.path(folder, "Ecospace_Annual_Average_Biomass.csv")
  if (!file.exists(fp)) {
    return(data.frame(ecosim_scen = NA, timeseries = NA, start_year = NA,
                      n_years = NA, map = NA, stringsAsFactors = FALSE))
  }
  lines <- readLines(fp)
  get_val <- function(key){
    m <- grep(paste0("^", key, ","), lines, value = TRUE)[1]
    if (is.na(m)) NA_character_ else sub(paste0("^", key, ","), "", m)
  }
  header_row <- which(grepl("^Year,", lines))[1]
  n_years    <- if (!is.na(header_row)) length(lines) - header_row else NA_integer_
  data.frame(
    ecosim_scen = get_val("EcosimScenario"),
    timeseries  = get_val("TimeSeries"),
    start_year  = get_val("StartYear"),
    n_years     = n_years,
    map         = paste0(get_val("MapRows"), "x", get_val("MapCols")),
    stringsAsFactors = FALSE
  )
}
scen_info <- cbind(
  folder = spa_scenarios,
  do.call(rbind, lapply(file.path(ewe_out_fold, spa_scenarios),
                        f.read_scenario_meta))
)

cat("\nDetected Ecospace scenarios under '", ewe_out_fold, "':\n", sep = "")
print(scen_info, row.names = FALSE)

## User-editable label overrides ----------------------------------------------
## Add entries to relabel scenarios in plot titles and legends. Any folder
## name not listed here is used as-is. Copy folder names from the table above.
scen_labels <- c(
  "spa_exp_status-quo" = "Status quo",
  "spa_exp_comm-eff-50" = "+50% harvest"
)

## Reorder so scenarios listed in `scen_labels` appear in that order first;
## any unlisted scenarios fall to the end. This controls the left-to-right
## bar order inside each panel of the final-years plots.
if (length(scen_labels) > 0) {
  labeled_order <- names(scen_labels)[names(scen_labels) %in% spa_scenarios]
  unlabeled     <- setdiff(spa_scenarios, labeled_order)
  spa_scenarios <- c(labeled_order, unlabeled)
}

spa_scen_names <- ifelse(spa_scenarios %in% names(scen_labels),
                         unname(scen_labels[spa_scenarios]),
                         spa_scenarios)
col_spa        <- hcl.colors(max(length(spa_scenarios), 1), palette = "Dark 3")

cat("\nPlot labels (edit scen_labels above to override):\n")
print(data.frame(folder = spa_scenarios, plot_label = spa_scen_names),
      row.names = FALSE)

## Output set up --------------------------------------------------------------
##
## Directory, filenames, and whether the biomass + catch PDFs are combined.
## Edit any of these to change where/what/how the PDFs are written; the plotting
## blocks below reference these variables and nothing else.

dir_out            <- paste0("./scenario-comparisons/", scen_group, "/")  ## Folder where output PDFs are written
append_pdfs        <- TRUE                                    ## TRUE -> one combined PDF; FALSE -> two separate PDFs

plot_name_final_xY       <- "Bfinal_scaled.PDF"                    ## Final-years biomass filename (append_pdfs FALSE)
plot_name_final_catches  <- "Cfinal_by_fleet-group_scaled.PDF"     ## Final-years catches filename (append_pdfs FALSE)
plot_name_final_combined <- "ecospace_out_final.PDF"               ## Combined final-years filename (append_pdfs TRUE)

if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)
pdf_file_final_xY       <- paste0(dir_out, plot_name_final_xY)
pdf_file_final_catches  <- paste0(dir_out, plot_name_final_catches)
pdf_file_final_combined <- paste0(dir_out, plot_name_final_combined)

## User-defined parameters for plotting ---------------------------------------
init_years_toscale = 1  ## When scale_to_init, values are divided by mean of first N years (per series)
final_n_years      = 5  ## Number of trailing years to summarize per panel
scale_to_init      = FALSE  ## FALSE -> native units (t/km2, t/km2/yr); TRUE -> divide each series by its init-year mean
overlay_points     = TRUE   ## TRUE -> overlay the N raw annual values as jittered dots

## Page layout: fixed 4 columns x up to 8 rows on 8.5x11 in letter portrait.
## Each page's first slot is reserved for the legend, so 31 data panels/page;
## additional panels spill onto further PDF pages automatically.
pdf_ncol      = 4
pdf_nrow      = 8
pdf_width_in  = 8.5
pdf_height_in = 11
plots_per_pg  = pdf_ncol * pdf_nrow

################################################################################
##
## Read in timeseries data, Ecosim outputs, and Ecospace outputs

## -----------------------------------------------------------------------------
##
## Read-in ANNUAL Observed, Ecosim, and Ecospace TS
dir_sim = paste0("./", ewe_out_fold, "/" , sim_scenario, "/")

## Read-in Ecosim annual biomass
filename = paste0(dir_sim, "biomass_annual.csv")
num_skip_sim = f.find_start_line(filename, flag = srt_year)

simB_xY <- read.csv(paste0(dir_sim, "biomass_annual.csv"), skip = num_skip_sim)
years = simB_xY$year.group ## Get date range from Ecosim
simB_xY$year.group = NULL

## Read-in Ecosim annual catches
simC_xY <- read.csv(paste0(dir_sim, "catch_annual.csv"), skip = num_skip_sim)
simC_xY$year.group = NULL

## Prepare months and date series objects
start_y <- min(years)
end_y   <- max(years)
date_series <- seq(as.Date(paste0(start_y, "-01-01")), as.Date(paste0(end_y,   "-12-01")), by = "1 month")
year_series <- seq(as.Date(paste0(start_y, "-01-01")), as.Date(paste0(end_y,   "-12-01")), by = "1 year")
ym_series <- format(date_series, "%Y-%m")

## Read in Ecosim monthly biomasses (kept for parity with overtime script; not used here)
simB_xM <- read.csv(paste0(dir_sim, "biomass_monthly.csv"), skip = num_skip_sim); simB_xM$timestep.group = NULL
simC_xM <- read.csv(paste0(dir_sim, "catch_monthly.csv"), skip = num_skip_sim); simC_xM$timestep.group = NULL
rownames(simB_xM) = ym_series

## -----------------------------------------------------------------------------
##
## Read-in observed fitted timeseries ------------------------------------------
obs_ls    = f.read_ecosim_timeseries(obs_TS_path, num_row_header = 5)
obsB.head = obs_ls$obsB.head; obsB = obs_ls$obsB
obsC.head = obs_ls$obsC.head; obsC = obs_ls$obsC
obs_years = as.numeric(rownames(obsB))
obs_dates = as.Date(paste0(obs_years, "-01-01"))

## Read-in functional-group lookup (pool code -> group name) ------------------
fnm_be    = paste0(ewe_out_fold, "/basic_estimates.csv")
be_lines  = readLines(fnm_be)
skip_be   = which(grepl("^GroupNo,", be_lines))[1] - 1
fg_lookup = read.csv(fnm_be, skip = skip_be)

## -----------------------------------------------------------------------------
##
## Read-in Ecospace annual biomass and catches ---------------------------------

ls_spaB_xY <- list(); ls_spaC_xY <- list()
kept_scenarios <- character()

for (i in 1:length(spa_scenarios)) {
  (dir_spa = paste0("./", ewe_out_fold, "/", spa_scenarios[i], "/"))

  ## Read in Annual Biomass
  filename <- paste0("Ecospace_Annual_Average_Biomass.csv")
  filepath <- paste0(dir_spa, filename)
  num_skip_spa <- f.find_start_line(filepath, flag = "Year")
  spaB_xY <- read.csv(filepath, skip = num_skip_spa, header = TRUE); spaB_xY$Year = NULL

  ## Skip scenarios whose annual run length doesn't match Ecosim.
  if (nrow(spaB_xY) != length(years)) {
    message("Skipping ", spa_scenarios[i], ": ", nrow(spaB_xY),
            " Ecospace years but Ecosim has ", length(years), ".")
    next
  }

  ## Read in Annual Catches. check.names = FALSE preserves the "fleet|group"
  ## column-name delimiter (default sanitization would turn '|' into '.').
  filename <- paste0("Ecospace_Annual_Average_Catch.csv")
  filepath <- paste0(dir_spa, filename)
  num_skip_spa <- f.find_start_line(filepath, flag = "Year")
  spaC_xY <- read.csv(filepath, skip = num_skip_spa, header = TRUE, check.names = FALSE)
  spaC_xY[["Year"]] <- NULL

  ## Standardize Ecospace FG names
  fg_names = f.standardize_group_names(colnames(spaB_xY))
  num_fg = length(fg_names)
  fg_df <- data.frame(
    pool_code  = 1:num_fg,
    group_name = paste(sprintf("%02d", 1:num_fg),
                       gsub("_", " ", fg_names))
  )

  ## Set row and column names
  rownames(spaB_xY) = rownames(spaC_xY) = years
  colnames(simB_xY) = fg_df$group_name

  ## Add to lists (append so skipped scenarios don't leave NULL slots)
  slot <- length(kept_scenarios) + 1
  ls_spaB_xY[[slot]] <- spaB_xY
  ls_spaC_xY[[slot]] <- spaC_xY
  kept_scenarios     <- c(kept_scenarios, spa_scenarios[i])
}

## Compact scenario metadata to just the kept ones.
if (length(kept_scenarios) < length(spa_scenarios)) {
  keep_mask      <- spa_scenarios %in% kept_scenarios
  spa_scen_names <- spa_scen_names[keep_mask]
  col_spa        <- col_spa[keep_mask]
  spa_scenarios  <- kept_scenarios
}
if (length(spa_scenarios) == 0) {
  stop("No Ecospace scenarios survived the length-match filter. ",
       "Every scenario under '", ewe_out_fold,
       "' had a different annual run length than Ecosim (", length(years),
       " years). Re-run Ecospace with a matching span, or point ",
       "`sim_scenario` at an Ecosim run of the same length.")
}
n_scen <- length(spa_scenarios)

## Guardrail: if any scenario has fewer years than final_n_years, note it and
## clamp the effective window rather than crashing on tail().
min_years_available <- min(sapply(ls_spaB_xY, nrow))
if (final_n_years > min_years_available) {
  message("final_n_years (", final_n_years, ") exceeds shortest scenario length (",
          min_years_available, "); using ", min_years_available, " instead.")
  final_n_years <- min_years_available
}

## -----------------------------------------------------------------------------
##
## Derive catch-panel auxiliaries (mirrors overtime script lines ~355-392) -----
## Parse fleet and group from Ecospace catch column names ("fleet|group"),
## map each column to (fleet_id, group_id), pull matching Ecosim catch series,
## and filter panels to columns where Ecosim reports any catch.

spaC_cols       <- colnames(ls_spaC_xY[[1]])
parts           <- do.call(rbind, strsplit(spaC_cols, "\\|"))
fleet_of_col    <- parts[, 1]
group_of_col    <- parts[, 2]
fleet_names     <- unique(fleet_of_col)
fleet_id_of_col <- match(fleet_of_col, fleet_names)
group_id_of_col <- match(trimws(group_of_col), trimws(fg_lookup$Group))
if (any(is.na(group_id_of_col))) {
  message("Fleet|group columns with no GroupNo match: ",
          paste(unique(group_of_col[is.na(group_id_of_col)]), collapse = "; "))
}

fnm_efg   <- paste0(dir_sim, "catch-fleet-group_annual.csv")
efg_hdr   <- which(grepl("^year,fleet,group,value", readLines(fnm_efg)))[1] - 1
efg       <- read.csv(fnm_efg, skip = efg_hdr)
efg_years <- sort(unique(efg$year))

sim_fg_by_col <- lapply(seq_along(spaC_cols), function(k){
  rows = efg$fleet == fleet_id_of_col[k] & efg$group == group_id_of_col[k]
  out  = rep(0, length(efg_years))
  if (any(rows)) {
    m = efg[rows, ]
    out[match(m$year, efg_years)] = m$value
  }
  out
})

obsC_fleet <- as.numeric(obsC.head[["Pool_code"]])
obsC_group <- as.numeric(obsC.head[["Pool_code_2"]])

active_cols <- which(sapply(sim_fg_by_col, sum, na.rm = TRUE) > 0)
num_catch_panels <- length(active_cols)

## -----------------------------------------------------------------------------
##
## Helpers for the final-years summary

## Divide by mean of first init_years_toscale (per series) when scale_to_init;
## otherwise identity. Series with a non-positive or non-finite denom pass
## through unchanged, matching the overtime script's f.scale_or_raw.
f.scale_or_raw <- function(v){
  if (!scale_to_init) return(v)
  denom <- mean(v[1:init_years_toscale], na.rm = TRUE)
  if (is.finite(denom) && denom > 0) v / denom else v
}

## Summarize the last n finite values of a numeric series.
f.final_years_summary <- function(series, n){
  vals      <- series[is.finite(series)]
  tail_vals <- if (length(vals) == 0) numeric(0) else utils::tail(vals, n)
  list(vals = tail_vals,
       mean = if (length(tail_vals)) mean(tail_vals)   else NA_real_,
       min  = if (length(tail_vals)) min(tail_vals)    else NA_real_,
       max  = if (length(tail_vals)) max(tail_vals)    else NA_real_,
       sd   = if (length(tail_vals) > 1) stats::sd(tail_vals) else NA_real_)
}

## -----------------------------------------------------------------------------
##
## Plot styling (parallels overtime script) -----------------------------------

main_cex = 1.0; leg_cex = 1.0; leg_pos = 'topleft'
x_cex = 0.75; y_cex = 0.9; y_break = 4; x_las = 2
bar_alpha = 0.5
bar_halfwidth = 0.35
whisker_halfwidth = 0.15

## Build the ordered list of bar entries for a panel: Ecospace scenarios only.
## Each entry is a list with `label`, `color`, and `summ` (an
## f.final_years_summary() result).
f.build_entries <- function(spa_summ_ls){
  entries <- list()
  for (j in seq_along(spa_summ_ls)) {
    entries[[length(entries) + 1]] <- list(label = as.character(j),
                                           color = col_spa[j],
                                           summ  = spa_summ_ls[[j]])
  }
  entries
}

## Draw a legend panel (called at the start of each PDF page). Scenario
## entries are stacked at the top-left of the legend slot so all rows share
## a common left edge (`inset` hugs the slot's left edge; `x.intersp`
## tightens spacing between swatch and label).
f.draw_final_legend <- function(y_units = NULL){
  plot(0, 0, type = 'n', xlim = c(0, 1), ylim = c(0, 1),
       xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', bty = 'n')
  scen_labels_num <- paste0(seq_len(n_scen), "  ", spa_scen_names)
  legend(leg_pos, inset = c(0.02, 0.05), bg = "gray90", box.lty = 0,
         legend    = scen_labels_num,
         pch       = rep(15, n_scen),
         pt.cex    = rep(1.6, n_scen),
         col       = col_spa,
         cex       = leg_cex,
         x.intersp = 0.6,
         y.intersp = 1.1,
         xjust     = 0,
         yjust     = 0)
  if (!is.null(y_units)) {
    text(x = 0.5, y = 0.15, labels = y_units,
         cex = leg_cex * 0.9, adj = c(0.5, 0.5))
  }
}

## Draw one bar-and-whisker panel. `entries` is a list of
## {label, color, summ} in the intended left-to-right order.
f.draw_panel <- function(panel_title, entries){
  finite_summ <- Filter(function(e) is.finite(e$summ$mean), entries)
  if (length(finite_summ) == 0) {
    plot.new()
    title(main = panel_title, line = -0.6, cex.main = main_cex * 0.85)
    return(invisible())
  }

  all_vals <- unlist(lapply(finite_summ, function(e) c(e$summ$min, e$summ$max)))
  all_vals <- all_vals[is.finite(all_vals)]
  yr       <- range(all_vals)
  if (!all(is.finite(yr))) yr <- c(0, 1)
  pad      <- 0.10 * diff(yr)
  if (pad == 0) pad <- 0.05 * abs(yr[1]) + 1e-9
  ylim     <- c(yr[1] - pad, yr[2] + pad)
  if (!scale_to_init && ylim[1] > 0) ylim[1] <- 0  ## bars grow from 0 in raw mode

  n_bars <- length(entries)
  xlim   <- c(0.5, n_bars + 0.5)
  plot(NA, xlim = xlim, ylim = ylim, xaxt = 'n', yaxt = 'n',
       xlab = '', ylab = '', bty = 'n')
  title(main = panel_title, line = -0.6, cex.main = main_cex * 0.85)

  axis(1, at = seq_len(n_bars),
       labels = vapply(entries, `[[`, character(1), "label"),
       cex.axis = x_cex, las = x_las, tick = TRUE)

  y_ticks <- pretty(ylim, n = y_break)
  axis(2, at = y_ticks, labels = y_ticks, las = 1, cex.axis = y_cex)
  if (scale_to_init) abline(h = 1, col = 'lightgray')

  bar_bottom <- if (scale_to_init) 0 else ylim[1]

  for (k in seq_len(n_bars)) {
    e <- entries[[k]]
    s <- e$summ
    if (!is.finite(s$mean)) next
    rect(k - bar_halfwidth, bar_bottom, k + bar_halfwidth, s$mean,
         col = adjustcolor(e$color, alpha.f = bar_alpha),
         border = e$color)
    if (is.finite(s$min) && is.finite(s$max)) {
      segments(k, s$min, k, s$max, col = e$color, lwd = 1.5)
      segments(k - whisker_halfwidth, s$min,
               k + whisker_halfwidth, s$min, col = e$color, lwd = 1.5)
      segments(k - whisker_halfwidth, s$max,
               k + whisker_halfwidth, s$max, col = e$color, lwd = 1.5)
    }
    if (overlay_points && length(s$vals) > 0) {
      jx <- k + stats::runif(length(s$vals), -0.10, 0.10)
      points(jx, s$vals, pch = 16, cex = 0.5, col = 'black')
    }
  }
}

## -----------------------------------------------------------------------------
##
## Plot final-years biomass ---------------------------------------------------

if (append_pdfs) {
  pdf(pdf_file_final_combined, width = pdf_width_in, height = pdf_height_in, onefile = TRUE)
  print(paste("Writing", pdf_file_final_combined))
} else {
  pdf(pdf_file_final_xY, width = pdf_width_in, height = pdf_height_in, onefile = TRUE)
  print(paste("Writing", pdf_file_final_xY))
}
par(mfrow = c(pdf_nrow, pdf_ncol), mar = c(2, 2.4, 1, 1))

for (i in 1:num_fg) {
  grp         <- fg_df$group_name[i]
  spaB_ls     <- lapply(ls_spaB_xY, function(df) df[, i])
  spaB_use_ls <- lapply(spaB_ls, f.scale_or_raw)
  summ_ls     <- lapply(spaB_use_ls, f.final_years_summary, n = final_n_years)
  entries     <- f.build_entries(summ_ls)

  ## Legend at first slot of each page (biomass block)
  if (i %in% seq(1, num_fg, by = plots_per_pg - 1)) {
    f.draw_final_legend(y_units = if (scale_to_init) "Y-axis: relative to init"
                                  else expression("Y-axis: Biomass (t/km"^2*")"))
  }

  f.draw_panel(grp, entries)
}

if (!append_pdfs) dev.off()

## -----------------------------------------------------------------------------
##
## Plot final-years catches by FLEET | GROUP ----------------------------------

if (!append_pdfs) {
  pdf(pdf_file_final_catches, width = pdf_width_in, height = pdf_height_in, onefile = TRUE)
  print(paste("Writing", pdf_file_final_catches))
  par(mfrow = c(pdf_nrow, pdf_ncol), mar = c(2, 2.4, 1, 1))
} else {
  ## Append mode: force a fresh page so the catch block starts with a legend
  ## at slot (1,1). Fill remaining slots on the current (biomass) page with
  ## blank frames so par(mfrow) rolls to a new page.
  biomass_slots <- num_fg + ceiling(num_fg / (plots_per_pg - 1))
  remainder     <- ((biomass_slots - 1) %% plots_per_pg) + 1
  for (i in seq_len(plots_per_pg - remainder)) plot.new()
}

for (k in seq_along(active_cols)) {
  col_idx     <- active_cols[k]
  panel_title <- paste0(fleet_of_col[col_idx], " | ", group_of_col[col_idx])
  spaC_ls     <- lapply(ls_spaC_xY, function(df) df[, col_idx])
  spaC_use_ls <- lapply(spaC_ls, f.scale_or_raw)
  summ_ls     <- lapply(spaC_use_ls, f.final_years_summary, n = final_n_years)
  entries     <- f.build_entries(summ_ls)

  ## Legend at first slot of each page (catch block)
  if (k %in% seq(1, num_catch_panels, by = plots_per_pg - 1)) {
    f.draw_final_legend(y_units = if (scale_to_init) "Y-axis: relative to init"
                                  else expression("Y-axis: Catch (t/km"^2*"/yr)"))
  }

  f.draw_panel(panel_title, entries)
}

dev.off()
