## Naming conventions
## 'sim' --> Related to Ecosim
## 'spa' --> Related to Ecospace
## 'obs' --> Related to observed timeseries data, i.e., that Ecosim was fitted to.
## 'B'   --> Denotes biomass
## 'C'   --> Denotes catch
##
## Purpose:
## Compare Ecospace scenario outputs against the Ecosim baseline and observed
## fitted timeseries. Produces two views per functional group / fleet|group:
##   1) Over-time    -- annual line plots across the full run
##   2) Final-years  -- bar+whisker of the last N years per Ecospace scenario
## Both views share the same input data and scenario detection; toggles below
## let the user skip a view.
##
## NOTE: relative paths below assume the working directory is the repo root.
## Opening ChesICAT-outputs.Rproj in RStudio sets this automatically.

## Setup -----------------------------------------------------------------------
rm(list = ls())
source("./functions.R")

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

## Auto-detect Ecospace scenarios ---------------------------------------------
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

## Scenario labels + ordering -------------------------------------------------
## Add entries to relabel scenarios in plot titles and legends. Any folder not
## listed here is used as-is. Copy folder names from the table printed above.
scen_labels <- c(
  "spa_exp_status-quo"  = "Status quo",
  "spa_exp_comm-eff-50" = "+50% harvest"
)

## Reorder so scenarios listed in `scen_labels` appear in that order first;
## any unlisted scenarios fall to the end. This controls left-to-right bar
## order in final-years panels and line/legend order in over-time panels.
if (length(scen_labels) > 0) {
  labeled_order <- names(scen_labels)[names(scen_labels) %in% spa_scenarios]
  unlabeled     <- setdiff(spa_scenarios, labeled_order)
  spa_scenarios <- c(labeled_order, unlabeled)
}

spa_scen_names <- ifelse(spa_scenarios %in% names(scen_labels),
                         unname(scen_labels[spa_scenarios]),
                         spa_scenarios)
col_spa <- hcl.colors(max(length(spa_scenarios), 1), palette = "Dark 3")

cat("\nPlot labels (edit scen_labels above to override):\n")
print(data.frame(folder = spa_scenarios, plot_label = spa_scen_names),
      row.names = FALSE)

## Output set up --------------------------------------------------------------
## `append_pdfs = TRUE` produces one combined PDF holding all enabled views.
## `append_pdfs = FALSE` writes one PDF per view/block, using the per-block
## filenames below. Set either `make_*` toggle to FALSE to skip that view.
dir_out         <- paste0("./scenario-comparisons/", scen_group, "/")
append_pdfs     <- TRUE
make_overtime   <- TRUE
make_finalyears <- TRUE

plot_name_overtime_B   <- "BxY_scaled.PDF"
plot_name_overtime_C   <- "CxY_by_fleet-group_scaled.PDF"
plot_name_finalyears_B <- "Bfinal.PDF"
plot_name_finalyears_C <- "Cfinal_by_fleet-group.PDF"
plot_name_combined     <- "ecospace_out.PDF"

if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)
pdf_file_overtime_B   <- paste0(dir_out, plot_name_overtime_B)
pdf_file_overtime_C   <- paste0(dir_out, plot_name_overtime_C)
pdf_file_finalyears_B <- paste0(dir_out, plot_name_finalyears_B)
pdf_file_finalyears_C <- paste0(dir_out, plot_name_finalyears_C)
pdf_file_combined     <- paste0(dir_out, plot_name_combined)

## Plot parameters ------------------------------------------------------------
## Two independent scale toggles: over-time lines and final-years bars can be
## in relative-to-init or native units independently of each other.
init_years_toscale <- 1     ## When scale is TRUE, series are divided by mean of first N years
final_n_years      <- 5     ## Trailing years summarized per final-years panel
scale_overtime     <- TRUE  ## TRUE -> lines divided by init mean; FALSE -> native units (t/km2)
scale_finalyears   <- FALSE ## TRUE -> bars divided by init mean; FALSE -> native units (t/km2)
overlay_points     <- TRUE  ## Overlay raw annual values as jittered dots on final-years bars

## Page layout: fixed 4 cols x 8 rows on 8.5x11 in letter portrait.
## Each page's first slot is reserved for the legend, so 31 data panels/page;
## additional panels spill onto further PDF pages automatically.
pdf_ncol      <- 4
pdf_nrow      <- 8
pdf_width_in  <- 8.5
pdf_height_in <- 11
plots_per_pg  <- pdf_ncol * pdf_nrow

## Per-view panel margins (bottom, left, top, right)
mar_overtime   <- c(1, 2, 1, 2)
mar_finalyears <- c(2, 2.4, 1, 1)

## Line/point styling shared by both views
col_sim  <- rgb(0.2, 0.7, 0.1, alpha = 0.6)
sim_lty  <- 1; spa_lty <- 1
sim_lwd  <- 2; spa_lwd <- 1
obs_pch  <- 16; obs_cex <- 0.8
main_cex <- 1.0; leg_cex <- 1.0
leg_pos  <- 'topleft'
x_break  <- 5; x_las <- 2
x_cex    <- 0.9; y_cex <- 0.9; y_break <- 4

## Final-years bar styling
bar_alpha         <- 0.5
bar_halfwidth     <- 0.35
whisker_halfwidth <- 0.15

################################################################################
##
## Read in timeseries data, Ecosim outputs, and Ecospace outputs

## Read-in Ecosim annual + monthly biomass and catches ------------------------
dir_sim <- paste0("./", ewe_out_fold, "/", sim_scenario, "/")

fnm_simB     <- paste0(dir_sim, "biomass_annual.csv")
num_skip_sim <- f.find_start_line(fnm_simB, flag = srt_year)

simB_xY <- read.csv(fnm_simB, skip = num_skip_sim)
years   <- simB_xY$year.group
simB_xY$year.group <- NULL

simC_xY <- read.csv(paste0(dir_sim, "catch_annual.csv"), skip = num_skip_sim)
simC_xY$year.group <- NULL

## Date/year series derived from Ecosim's run length
start_y     <- min(years)
end_y       <- max(years)
date_series <- seq(as.Date(paste0(start_y, "-01-01")),
                   as.Date(paste0(end_y,   "-12-01")), by = "1 month")
year_series <- seq(as.Date(paste0(start_y, "-01-01")),
                   as.Date(paste0(end_y,   "-12-01")), by = "1 year")

## Read-in observed fitted timeseries -----------------------------------------
obs_ls    <- f.read_ecosim_timeseries(obs_TS_path, num_row_header = 5)
obsB.head <- obs_ls$obsB.head; obsB <- obs_ls$obsB
obsC.head <- obs_ls$obsC.head; obsC <- obs_ls$obsC
obs_years <- as.numeric(rownames(obsB))
obs_dates <- as.Date(paste0(obs_years, "-01-01"))

## Read-in functional-group lookup (pool code -> group name) ------------------
## basic_estimates.csv has metadata rows above the GroupNo header; locate the
## header dynamically rather than hardcoding the skip count.
fnm_be    <- paste0(ewe_out_fold, "/basic_estimates.csv")
be_lines  <- readLines(fnm_be)
skip_be   <- which(grepl("^GroupNo,", be_lines))[1] - 1
fg_lookup <- read.csv(fnm_be, skip = skip_be)

## Read-in Ecospace annual biomass and catches per scenario -------------------
ls_spaB_xY     <- list(); ls_spaC_xY <- list()
kept_scenarios <- character()

for (i in seq_along(spa_scenarios)) {
  dir_spa <- paste0("./", ewe_out_fold, "/", spa_scenarios[i], "/")

  filepath     <- paste0(dir_spa, "Ecospace_Annual_Average_Biomass.csv")
  num_skip_spa <- f.find_start_line(filepath, flag = "Year")
  spaB_xY      <- read.csv(filepath, skip = num_skip_spa, header = TRUE)
  spaB_xY$Year <- NULL

  ## Skip scenarios whose annual run length doesn't match Ecosim.
  ## Aligning heterogeneous run lengths would need a per-scenario time axis
  ## (out of scope); flag it loudly so the user can revisit.
  if (nrow(spaB_xY) != length(years)) {
    message("Skipping ", spa_scenarios[i], ": ", nrow(spaB_xY),
            " Ecospace years but Ecosim has ", length(years), ".")
    next
  }

  ## Ecospace catch: check.names = FALSE preserves the "fleet|group" column
  ## delimiter (default sanitization would turn '|' into '.').
  filepath     <- paste0(dir_spa, "Ecospace_Annual_Average_Catch.csv")
  num_skip_spa <- f.find_start_line(filepath, flag = "Year")
  spaC_xY      <- read.csv(filepath, skip = num_skip_spa, header = TRUE, check.names = FALSE)
  spaC_xY[["Year"]] <- NULL

  rownames(spaB_xY) <- rownames(spaC_xY) <- years

  slot <- length(kept_scenarios) + 1
  ls_spaB_xY[[slot]] <- spaB_xY
  ls_spaC_xY[[slot]] <- spaC_xY
  kept_scenarios     <- c(kept_scenarios, spa_scenarios[i])
}

## Compact scenario metadata to just the kept ones so downstream plotting
## (line counts, colors, legend labels) stays consistent.
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

## Functional-group naming (stable across scenarios; use first kept scenario)
fg_names <- f.standardize_group_names(colnames(ls_spaB_xY[[1]]))
num_fg   <- length(fg_names)
fg_df    <- data.frame(
  pool_code  = seq_len(num_fg),
  group_name = paste(sprintf("%02d", seq_len(num_fg)),
                     gsub("_", " ", fg_names))
)
colnames(simB_xY) <- fg_df$group_name

## Guardrail: if any scenario has fewer years than final_n_years, clamp the
## effective window rather than crashing on tail().
min_years_available <- min(sapply(ls_spaB_xY, nrow))
if (final_n_years > min_years_available) {
  message("final_n_years (", final_n_years, ") exceeds shortest scenario length (",
          min_years_available, "); using ", min_years_available, " instead.")
  final_n_years <- min_years_available
}

## Catch-panel auxiliaries -----------------------------------------------------
## Parse fleet and group from Ecospace catch column names ("fleet|group"),
## map each column to (fleet_id, group_id), pull the matching Ecosim catch
## series, and filter panels to columns where Ecosim reports any catch.
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
  rows <- efg$fleet == fleet_id_of_col[k] & efg$group == group_id_of_col[k]
  out  <- rep(0, length(efg_years))
  if (any(rows)) {
    m <- efg[rows, ]
    out[match(m$year, efg_years)] <- m$value
  }
  out
})

obsC_fleet <- as.numeric(obsC.head[["Pool_code"]])
obsC_group <- as.numeric(obsC.head[["Pool_code_2"]])

active_cols      <- which(sapply(sim_fg_by_col, sum, na.rm = TRUE) > 0)
num_catch_panels <- length(active_cols)

################################################################################
##
## Helpers

## Scale by mean of first init_years_toscale (per series) when `scale` is TRUE;
## otherwise identity. Series with a non-positive or non-finite denominator
## pass through unchanged (avoids divide-by-zero on late-starting fisheries).
f.scale_or_raw <- function(v, scale) {
  if (!scale) return(v)
  denom <- mean(v[1:init_years_toscale], na.rm = TRUE)
  if (is.finite(denom) && denom > 0) v / denom else v
}

## Scale observed series with the same rule, but drop any series whose init
## denom is missing/zero (they'd plot as a flat NaN band otherwise).
f.scale_obs_ls <- function(obsB_or_C, col_idx_vec, scale) {
  out <- list()
  for (m in col_idx_vec) {
    series <- as.numeric(obsB_or_C[, m])
    if (scale) {
      denom <- mean(series[1:init_years_toscale], na.rm = TRUE)
      if (is.finite(denom) && denom > 0) out[[length(out) + 1]] <- series / denom
    } else {
      out[[length(out) + 1]] <- series
    }
  }
  out
}

## Summarize the last n finite values of a numeric series.
f.final_years_summary <- function(series, n) {
  vals      <- series[is.finite(series)]
  tail_vals <- if (length(vals) == 0) numeric(0) else utils::tail(vals, n)
  list(vals = tail_vals,
       mean = if (length(tail_vals)) mean(tail_vals)   else NA_real_,
       min  = if (length(tail_vals)) min(tail_vals)    else NA_real_,
       max  = if (length(tail_vals)) max(tail_vals)    else NA_real_,
       sd   = if (length(tail_vals) > 1) stats::sd(tail_vals) else NA_real_)
}

## Bar entries for a final-years panel -- one per Ecospace scenario.
f.build_entries <- function(spa_summ_ls) {
  entries <- list()
  for (j in seq_along(spa_summ_ls)) {
    entries[[length(entries) + 1]] <- list(label = as.character(j),
                                           color = col_spa[j],
                                           summ  = spa_summ_ls[[j]])
  }
  entries
}

## Draw the legend used at slot (1,1) of each over-time PDF page.
f.draw_overtime_legend <- function(y_units = NULL) {
  plot(0, 0, type = 'n', xlim = c(0, 1), ylim = c(0, 1),
       xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', bty = 'n')
  legend(leg_pos, inset = 0.1, bg = "gray90", box.lty = 0,
         legend = c('Observed', 'Ecosim', spa_scen_names),
         lty    = c(NA, sim_lty, rep(spa_lty, n_scen)),
         lwd    = c(NA, sim_lwd + 1, rep(spa_lwd + 1, n_scen)),
         pch    = c(obs_pch, NA, rep(NA, n_scen)),
         col    = c('black', col_sim, col_spa),
         cex    = leg_cex)
  if (!is.null(y_units)) {
    text(x = 0.5, y = 0.02, labels = y_units,
         cex = leg_cex * 0.85, adj = c(0.5, 0))
  }
}

## Draw the legend used at slot (1,1) of each final-years PDF page. Scenario
## entries are stacked at the top-left of the legend slot so all rows share
## a common left edge.
f.draw_final_legend <- function(y_units = NULL) {
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

## Draw one over-time panel: Ecosim (green), Ecospace scenarios (colored),
## observed dots (black). Scaling is decided by the caller.
f.draw_overtime_panel <- function(panel_title, sim_scaled, spa_scaled_ls,
                                  obs_scaled_ls, title_cex_mult = 1) {
  ymin <- min(sim_scaled, unlist(spa_scaled_ls), unlist(obs_scaled_ls),
              na.rm = TRUE) * 0.9
  ymax <- max(sim_scaled, unlist(spa_scaled_ls), unlist(obs_scaled_ls),
              na.rm = TRUE) * 1.1
  plot(year_series, rep("", length(year_series)), type = 'b',
       ylim = c(ymin, ymax), xaxt = 'n', yaxt = 'n',
       xlab = '', ylab = '', bty = 'n')
  title(main = panel_title, line = -0.6, cex.main = main_cex * title_cex_mult)

  x_ticks <- pretty(year_series, n = round((end_y - start_y) / x_break))
  axis(1, at = x_ticks,
       labels = paste0("'", substring(format(x_ticks, "%Y"),
                                      nchar(format(x_ticks, "%Y")) - 1)),
       cex.axis = x_cex, las = x_las)
  y_ticks <- pretty(seq(ymin, ymax, by = (ymax - ymin) / 10), n = y_break)
  axis(2, at = y_ticks, labels = y_ticks, las = 1, cex.axis = y_cex)
  if (scale_overtime) abline(h = 1, col = 'lightgray')

  lines(year_series, sim_scaled, lty = sim_lty, lwd = sim_lwd, col = col_sim)
  for (j in seq_along(spa_scaled_ls)) {
    lines(year_series, spa_scaled_ls[[j]],
          lty = spa_lty, lwd = spa_lwd, col = col_spa[j])
  }
  for (obs_scaled in obs_scaled_ls) {
    points(obs_dates, obs_scaled, pch = obs_pch, cex = obs_cex, col = 'black')
  }
}

## Draw one final-years bar-and-whisker panel. `entries` is a list of
## {label, color, summ} in the intended left-to-right order.
f.draw_final_panel <- function(panel_title, entries) {
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
  if (!scale_finalyears && ylim[1] > 0) ylim[1] <- 0

  n_bars <- length(entries)
  xlim   <- c(0.5, n_bars + 0.5)
  plot(NA, xlim = xlim, ylim = ylim, xaxt = 'n', yaxt = 'n',
       xlab = '', ylab = '', bty = 'n')
  title(main = panel_title, line = -0.6, cex.main = main_cex * 0.85)

  axis(1, at = seq_len(n_bars),
       labels = vapply(entries, `[[`, character(1), "label"),
       cex.axis = 0.75, las = x_las, tick = TRUE)
  y_ticks <- pretty(ylim, n = y_break)
  axis(2, at = y_ticks, labels = y_ticks, las = 1, cex.axis = y_cex)
  if (scale_finalyears) abline(h = 1, col = 'lightgray')

  bar_bottom <- if (scale_finalyears) 0 else ylim[1]

  for (k in seq_len(n_bars)) {
    e <- entries[[k]]
    s <- e$summ
    if (!is.finite(s$mean)) next
    rect(k - bar_halfwidth, bar_bottom, k + bar_halfwidth, s$mean,
         col = adjustcolor(e$color, alpha.f = bar_alpha), border = e$color)
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

## PDF block open/close: when append_pdfs is FALSE, open a fresh per-block
## PDF; when TRUE, reset par() so the next block's first plot starts a fresh
## page in the already-open combined PDF.
f.start_pdf_block <- function(per_block_file, mar) {
  if (!append_pdfs) {
    pdf(per_block_file, width = pdf_width_in, height = pdf_height_in,
        onefile = TRUE)
    print(paste("Writing", per_block_file))
  }
  par(mfrow = c(pdf_nrow, pdf_ncol), mar = mar)
}

f.end_pdf_block <- function(panels_drawn) {
  if (!append_pdfs) {
    dev.off()
  } else {
    ## Fill remaining slots so the next block starts on a fresh page.
    remainder <- ((panels_drawn - 1) %% plots_per_pg) + 1
    fill_n    <- plots_per_pg - remainder
    if (fill_n > 0) for (i in seq_len(fill_n)) plot.new()
  }
}

################################################################################
##
## Plotting
## Note: make sure PDF readers are closed before running pdf()

if (!make_overtime && !make_finalyears) {
  stop("Nothing to plot: both make_overtime and make_finalyears are FALSE.")
}

if (append_pdfs) {
  pdf(pdf_file_combined, width = pdf_width_in, height = pdf_height_in,
      onefile = TRUE)
  print(paste("Writing", pdf_file_combined))
}

## Over-time views ------------------------------------------------------------

if (make_overtime) {
  ## Over-time biomass
  f.start_pdf_block(pdf_file_overtime_B, mar_overtime)
  y_units_ot_B <- if (scale_overtime) NULL
                  else expression("Y-axis: Biomass (t/km"^2*")")

  for (i in seq_len(num_fg)) {
    grp     <- fg_df$group_name[i]
    simB    <- simB_xY[, i]
    spaB_ls <- lapply(ls_spaB_xY, function(df) df[, i])

    simB_scaled    <- f.scale_or_raw(simB, scale_overtime)
    spaB_scaled_ls <- lapply(spaB_ls, function(v) f.scale_or_raw(v, scale_overtime))

    obs_cols      <- which(as.numeric(obsB.head[["Pool_code"]]) == i)
    obs_scaled_ls <- f.scale_obs_ls(obsB, obs_cols, scale_overtime)

    if (i %in% seq(1, num_fg, by = plots_per_pg - 1)) {
      f.draw_overtime_legend(y_units_ot_B)
    }
    f.draw_overtime_panel(grp, simB_scaled, spaB_scaled_ls, obs_scaled_ls)
  }

  panels_ot_B <- num_fg + ceiling(num_fg / (plots_per_pg - 1))
  f.end_pdf_block(panels_ot_B)

  ## Over-time catch
  f.start_pdf_block(pdf_file_overtime_C, mar_overtime)
  y_units_ot_C <- if (scale_overtime) NULL
                  else expression("Y-axis: Catch (t/km"^2*"/yr)")
  legend_step  <- max(plots_per_pg - 1, 1)

  for (k in seq_along(active_cols)) {
    col_idx     <- active_cols[k]
    fid         <- fleet_id_of_col[col_idx]
    gid         <- group_id_of_col[col_idx]
    panel_title <- paste0(fleet_of_col[col_idx], " | ", group_of_col[col_idx])

    simC    <- sim_fg_by_col[[col_idx]]
    spaC_ls <- lapply(ls_spaC_xY, function(df) df[, col_idx])

    simC_scaled    <- f.scale_or_raw(simC, scale_overtime)
    spaC_scaled_ls <- lapply(spaC_ls, function(v) f.scale_or_raw(v, scale_overtime))

    obs_cols      <- which(obsC_fleet == fid & obsC_group == gid)
    obs_scaled_ls <- f.scale_obs_ls(obsC, obs_cols, scale_overtime)

    if (k %in% seq(1, num_catch_panels, by = legend_step)) {
      f.draw_overtime_legend(y_units_ot_C)
    }
    f.draw_overtime_panel(panel_title, simC_scaled, spaC_scaled_ls,
                          obs_scaled_ls, title_cex_mult = 0.75)
  }

  panels_ot_C <- num_catch_panels + ceiling(num_catch_panels / (plots_per_pg - 1))
  f.end_pdf_block(panels_ot_C)
}

## Final-years views ----------------------------------------------------------

if (make_finalyears) {
  ## Final-years biomass
  f.start_pdf_block(pdf_file_finalyears_B, mar_finalyears)
  y_units_fy_B <- if (scale_finalyears) "Y-axis: relative to init"
                  else expression("Y-axis: Biomass (t/km"^2*")")

  for (i in seq_len(num_fg)) {
    grp         <- fg_df$group_name[i]
    spaB_ls     <- lapply(ls_spaB_xY, function(df) df[, i])
    spaB_use_ls <- lapply(spaB_ls, function(v) f.scale_or_raw(v, scale_finalyears))
    summ_ls     <- lapply(spaB_use_ls, f.final_years_summary, n = final_n_years)
    entries     <- f.build_entries(summ_ls)

    if (i %in% seq(1, num_fg, by = plots_per_pg - 1)) {
      f.draw_final_legend(y_units_fy_B)
    }
    f.draw_final_panel(grp, entries)
  }

  panels_fy_B <- num_fg + ceiling(num_fg / (plots_per_pg - 1))
  f.end_pdf_block(panels_fy_B)

  ## Final-years catch
  f.start_pdf_block(pdf_file_finalyears_C, mar_finalyears)
  y_units_fy_C <- if (scale_finalyears) "Y-axis: relative to init"
                  else expression("Y-axis: Catch (t/km"^2*"/yr)")

  for (k in seq_along(active_cols)) {
    col_idx     <- active_cols[k]
    panel_title <- paste0(fleet_of_col[col_idx], " | ", group_of_col[col_idx])
    spaC_ls     <- lapply(ls_spaC_xY, function(df) df[, col_idx])
    spaC_use_ls <- lapply(spaC_ls, function(v) f.scale_or_raw(v, scale_finalyears))
    summ_ls     <- lapply(spaC_use_ls, f.final_years_summary, n = final_n_years)
    entries     <- f.build_entries(summ_ls)

    if (k %in% seq(1, num_catch_panels, by = plots_per_pg - 1)) {
      f.draw_final_legend(y_units_fy_C)
    }
    f.draw_final_panel(panel_title, entries)
  }

  panels_fy_C <- num_catch_panels + ceiling(num_catch_panels / (plots_per_pg - 1))
  f.end_pdf_block(panels_fy_C)
}

if (append_pdfs) dev.off()
