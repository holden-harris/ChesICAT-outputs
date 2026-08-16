## Naming conventions
## 'sim' -<- Related to Ecosim
## 'spa' -<- Related to Ecospace
## 'obs' -<- Related to observed timeseries data, i.e., that Ecosim was fitted to.
## 'B'   -<- Denotes biomass
## 'C'   -<- Denotes catch

## NOTE: relative paths below assume the working directory is the repo root.
## Opening ChesICAT-outputs.Rproj in RStudio sets this automatically.

## Setup -----------------------------------------------------------------------
#rm(list=ls())
source("./functions.R") ## Pull in functions
library(dplyr)

## Input set up ----------------------------------------------------------------
##
ewe_out_fold <- "ewe-outputs/model-setups"
sim_scenario <- "ecosim_sim_01.3_SM2-fit"
obs_TS_name  <- "ewe-outputs/timeseries/ts_v1.4.csv"
srt_year     <- 2001

## Auto-detect Ecospace scenarios --------------------------------------------
## Any subfolder under ewe_out_fold whose name starts with "spa_" is treated
## as an Ecospace scenario. Metadata is read from each scenario's
## Ecospace_Annual_Average_Biomass.csv header so the user can confirm each
## run's provenance (Ecosim scenario, timeseries file, start year, run length,
## and Ecospace map dimensions) before plotting.
spa_scenarios <- list.dirs(ewe_out_fold, recursive <- FALSE, full.names <- FALSE)
spa_scenarios <- spa_scenarios[grepl("^spa_", spa_scenarios)]

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
   "spa_03_BCF-inv01"    = "Initial run",
   "spa_03_BCF-inv01-PP" = "With PP forcing"
)

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

dir_out            <- "./scenario-comparisons/model-setups/"    ## Folder where output PDFs are written
append_pdfs        <- TRUE                              ## TRUE <- one combined PDF; FALSE <- two separate PDFs

plot_name_xY       <- "BxY_scaled.PDF"                   ## Biomass-by-year filename (used when append_pdfs <- FALSE)
plot_name_catches  <- "CxY_by_fleet-group_scaled.PDF"    ## Catches-by-fleet|group filename (used when append_pdfs <- FALSE)
plot_name_combined <- "ecospace_out_xY.PDF"           ## Combined filename (used when append_pdfs <- TRUE)

if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)
pdf_file_name_xY      <- paste0(dir_out, plot_name_xY)
pdf_file_name_catches <- paste0(dir_out, plot_name_catches)
pdf_file_combined     <- paste0(dir_out, plot_name_combined)

## User-defined parameters for plotting ---------------------------------------
init_years_toscale = 1 ## In plotting, this sets the "1 line" to the average of this number of years

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

## Read in Ecosim monthly biomasses
simB_xM <- read.csv(paste0(dir_sim, "biomass_monthly.csv"), skip = num_skip_sim); simB_xM$timestep.group = NULL
simC_xM <- read.csv(paste0(dir_sim, "catch_monthly.csv"), skip = num_skip_sim); simC_xM$timestep.group = NULL
rownames(simB_xM) = ym_series

## -----------------------------------------------------------------------------
##
## Read-in observed fitted timeseries ------------------------------------------
obs_ls    = f.read_ecosim_timeseries(obs_TS_name, num_row_header = 5)
obsB.head = obs_ls$obsB.head; obsB = obs_ls$obsB
obsC.head = obs_ls$obsC.head; obsC = obs_ls$obsC
obs_years = as.numeric(rownames(obsB))
obs_dates = as.Date(paste0(obs_years, "-01-01"))

## Read-in functional-group lookup (pool code <- group name) ------------------
## basic_estimates.csv has several metadata lines above the GroupNo header;
## locate the header dynamically rather than hardcoding the skip count.
fnm_be    = paste0(ewe_out_fold, "/basic_estimates.csv")
be_lines  = readLines(fnm_be)
skip_be   = which(grepl("^GroupNo,", be_lines))[1] - 1
fg_lookup = read.csv(fnm_be, skip = skip_be)

## -----------------------------------------------------------------------------
##
## Read-in Ecospace annual biomass and catches ---------------------------------

## Initialize empty lists (4) for biomasses and catches by year and month
ls_spaB_xY <- list(); ls_spaC_xY <- list()
kept_scenarios <- character()  ## scenarios whose run length matches Ecosim

## Loop through scenarios to populate lists
for (i in 1:length(spa_scenarios)) {
  (dir_spa = paste0("./", ewe_out_fold, "/", spa_scenarios[i], "/"))

  ## Read in Annual Biomass
  filename <- paste0("Ecospace_Annual_Average_Biomass.csv")    ## Set filename
  filepath <- paste0(dir_spa, filename)                        ## Set filepath based on directory
  num_skip_spa <- f.find_start_line(filepath, flag = "Year")   ## Apply function to find the start line to start reading table
  spaB_xY <- read.csv(filepath, skip = num_skip_spa, header = TRUE); spaB_xY$Year = NULL

  ## Skip scenarios whose annual run length doesn't match Ecosim.
  ## Aligning heterogeneous run lengths would require a per-scenario time axis
  ## (out of scope here); flag it loudly so the user can revisit.
  if (nrow(spaB_xY) != length(years)) {
    message("Skipping ", spa_scenarios[i], ": ", nrow(spaB_xY),
            " Ecospace years but Ecosim has ", length(years), ".")
    next
  }
  
  ## Read in Monthly Biomass
  filename <- paste0("Ecospace_Average_Biomass.csv")
  filepath <- paste0(dir_spa, filename)
  num_skip_spa <- f.find_start_line(filepath, flag = "TimeStep")
  spaB_xM <- read.csv(filepath, skip = num_skip_spa, header = TRUE); spaB_xM$TimeStep = NULL
  
  ## Read in Annual Catches. check.names = FALSE preserves the "fleet|group"
  ## column-name delimiter (default sanitization would turn '|' into '.').
  filename <- paste0("Ecospace_Annual_Average_Catch.csv")
  filepath <- paste0(dir_spa, filename)
  num_skip_spa <- f.find_start_line(filepath, flag = "Year")
  spaC_xY <- read.csv(filepath, skip = num_skip_spa, header = TRUE, check.names = FALSE)
  spaC_xY[["Year"]] <- NULL
  
  ## Read in Monthly Catches
  filename <- paste0("Ecospace_Average_Catch.csv")
  filepath <- paste0(dir_spa, filename)
  num_skip_spa <- f.find_start_line(filepath, flag = "TimeStep")
  spaC_xM <- read.csv(filepath, skip = num_skip_spa, header = TRUE); spaC_xM$TimeStep = NULL
  
  ## Standardize Ecospace FG names ------------------------------
  fg_names = f.standardize_group_names(colnames(spaB_xY))
  num_fg = length(fg_names)
  fg_df <- data.frame(
    pool_code  = 1:num_fg,
    group_name = paste(sprintf("%02d", 1:num_fg),
                       gsub("_", " ", fg_names))
  )
  
  ## Set row and column names
  rownames(spaB_xY) = rownames(spaC_xY) = years
  rownames(spaB_xM) = rownames(spaC_xM) = ym_series
  colnames(spaB_xM) = colnames(simB_xY) = fg_df$group_name
  
  ## Add to lists (append so skipped scenarios don't leave NULL slots)
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


## -----------------------------------------------------------------------------
##
## Plotting parameters (shared by biomass and catch blocks)
## Note: Make sure PDF readers are closed before running pdf()

col_sim = rgb(0.2, 0.7, .1, alpha = 0.6) ## rgb (red, green, blue, alpha)

x_break = 5; y_break = 4; x_cex = 0.9; y_cex = 0.9; x_las = 2
sim_lty = 1; spa_lty = 1
sim_lwd = 2; spa_lwd = 1; obs_pch = 16; obs_cex = 0.8
main_cex = 1.0; leg_cex = 1.0; leg_pos = 'topleft'; leg_inset = 0.1

x = year_series

## -----------------------------------------------------------------------------
##
## Plot biomasses by year (xY) ------------------------------------------------

if (append_pdfs) {
  pdf(pdf_file_combined, width = pdf_width_in, height = pdf_height_in, onefile = TRUE)
  print(paste("Writing", pdf_file_combined))
} else {
  pdf(pdf_file_name_xY, width = pdf_width_in, height = pdf_height_in, onefile = TRUE)
  print(paste("Writing", pdf_file_name_xY))
}
par(mfrow = c(pdf_nrow, pdf_ncol), mar = c(1, 2, 1, 2))

for(i in 1:num_fg){
  grp  = fg_df$group_name[i]
  simB = simB_xY[,i]
  spaB_ls <- lapply(ls_spaB_xY, function(df) df[, i]) ## Extract the i column from each data frame in the list

  ## Scale to the average of a given timeframe
  simB_scaled = simB / mean(simB[1:init_years_toscale], na.rm = TRUE)
  spaB_scaled_ls = list()
  for(j in 1:length(spa_scenarios)){
    spaB               <- spaB_ls[[j]]
    spaB_scaled        <- spaB / mean(spaB[1:init_years_toscale], na.rm = TRUE)
    spaB_scaled_ls[[j]] <- spaB_scaled
  }

  ## Observed series matching this pool code (may be zero, one, or several) ----
  obs_cols   = which(as.numeric(obsB.head[["Pool_code"]]) == i)
  obs_scaled_ls = list()
  for(m in obs_cols){
    obs_series = as.numeric(obsB[, m])
    denom      = mean(obs_series[1:init_years_toscale], na.rm = TRUE)
    if(is.finite(denom) && denom > 0){
      obs_scaled_ls[[length(obs_scaled_ls) + 1]] = obs_series / denom
    }
  }

  ##-----------------------------------------------------------------------------
  ## PLOT

  ## Legend plots -------------------------------------------
  if(i %in% seq(1, num_fg, by = plots_per_pg-1)) {
    plot(0, 0, type='n', xlim=c(0,1), ylim=c(0,1), xaxt='n', yaxt='n',
         xlab='', ylab='', bty='n')
    legend(leg_pos, inset = 0.1, bg="gray90", box.lty = 0,
           legend = c('Observed', 'Ecosim', spa_scen_names),
           lty    = c(NA, sim_lty, rep(spa_lty, length(spaB_scaled_ls))),
           lwd    = c(NA, sim_lwd+1, rep(spa_lwd+1, length(spaB_scaled_ls))),
           pch    = c(obs_pch, NA, rep(NA, length(spaB_scaled_ls))),
           col    = c('black', col_sim, col_spa),
           cex    = leg_cex)
  }

  ## Data plots -------------------------------------------
  ## Determine y-axis range and initialize plot
  min = min(simB_scaled, unlist(spaB_scaled_ls), unlist(obs_scaled_ls), na.rm=T) * 0.9
  max = max(simB_scaled, unlist(spaB_scaled_ls), unlist(obs_scaled_ls), na.rm=T) * 1.1
  plot(x, rep("", length(x)), type='b',
       ylim = c(min, max), xaxt = 'n', yaxt = 'n',
       xlab = '', ylab='', bty = 'n')
  title(main = grp, line=-.6, cex.main = main_cex)

  ## Get years from date series
  posx = as.POSIXlt(date_series)
  x_years = unique(posx$year + 1900)
  end_y = max(x_years)
  start_y = min(x_years)

  ## Setup X-axis
  year_series <- seq(as.Date(paste0(start_y, "-01-01")), as.Date(paste0(end_y,   "-12-01")), by = "1 year")
  num_breaks_x <- round((end_y - start_y) / x_break)
  x_ticks <- pretty(x, n = num_breaks_x)
  xlab = paste0("'", substring(format(x_ticks, "%Y"), nchar(format(x_ticks, "%Y")) - 1))
  axis(1, at = x_ticks, labels = xlab, cex.axis = x_cex, las = x_las)

  ## Setup Y-axis
  y_ticks = pretty(seq(min, max, by = (max-min)/10), n = y_break)
  axis(2, at = y_ticks, labels = y_ticks, las = 1, cex.axis = y_cex)
  abline(h=1, col='lightgray')

  ## Plot outputs: Ecosim (green line), Ecospace (colored lines), Observed (black dots)
  lines(x, simB_scaled, lty=sim_lty, lwd = sim_lwd, col = col_sim)
  for(j in seq_along(spaB_scaled_ls)) {
    lines(x, spaB_scaled_ls[[j]], lty=spa_lty, lwd=spa_lwd, col=col_spa[j])
  }
  for(obs_scaled in obs_scaled_ls) {
    points(obs_dates, obs_scaled, pch = obs_pch, cex = obs_cex, col = 'black')
  }
}
if (!append_pdfs) dev.off()

## -----------------------------------------------------------------------------
##
## Plot catches by FLEET | GROUP (unaggregated) -------------------------------
## One panel per Ecospace fleet|group column. No summing across groups.

f.scale_or_raw <- function(v){
  denom = mean(v[1:init_years_toscale], na.rm = TRUE)
  if(is.finite(denom) && denom > 0) v / denom else v
}

## Parse fleet and group from Ecospace catch column names ("fleet|group")
spaC_cols       <- colnames(ls_spaC_xY[[1]])
parts           <- do.call(rbind, strsplit(spaC_cols, "\\|"))
fleet_of_col    <- parts[, 1]
group_of_col    <- parts[, 2]
fleet_names     <- unique(fleet_of_col)
fleet_id_of_col <- match(fleet_of_col, fleet_names)

## Map Ecospace group name <- Ecosim GroupNo via fg_lookup (basic_estimates.csv)
group_id_of_col <- match(trimws(group_of_col), trimws(fg_lookup$Group))
if(any(is.na(group_id_of_col))){
  message("Fleet|group columns with no GroupNo match: ",
          paste(unique(group_of_col[is.na(group_id_of_col)]), collapse = "; "))
}

## Ecosim per-(fleet, group) annual catch from long-format CSV
fnm_efg <- paste0(dir_sim, "catch-fleet-group_annual.csv")
efg_hdr <- which(grepl("^year,fleet,group,value", readLines(fnm_efg)))[1] - 1
efg     <- read.csv(fnm_efg, skip = efg_hdr)
efg_years <- sort(unique(efg$year))

sim_fg_by_col <- lapply(seq_along(spaC_cols), function(k){
  rows = efg$fleet == fleet_id_of_col[k] & efg$group == group_id_of_col[k]
  out  = rep(0, length(efg_years))
  if(any(rows)){
    m = efg[rows, ]
    out[match(m$year, efg_years)] = m$value
  }
  out
})

## Observed catch series: Pool_code = fleet ID, Pool_code_2 = group (pool) ID
obsC_fleet <- as.numeric(obsC.head[["Pool_code"]])
obsC_group <- as.numeric(obsC.head[["Pool_code_2"]])

## Restrict to fleet|group panels with any Ecosim catch
active_cols <- which(sapply(sim_fg_by_col, sum, na.rm = TRUE) > 0)
num_panels  <- length(active_cols)

legend_step <- max(plots_per_pg - 1, 1)

if (!append_pdfs) {
  pdf(pdf_file_name_catches, width = pdf_width_in, height = pdf_height_in, onefile = TRUE)
  print(paste("Writing", pdf_file_name_catches))
  par(mfrow = c(pdf_nrow, pdf_ncol), mar = c(1, 2, 1, 2))
} else {
  ## Append mode: force a fresh page so the catch block starts with a legend
  ## at slot (1,1). Fill remaining slots on the current (biomass) page with
  ## blank frames so par(mfrow) rolls to a new page.
  biomass_slots <- num_fg + ceiling(num_fg / (plots_per_pg - 1))
  remainder     <- ((biomass_slots - 1) %% plots_per_pg) + 1
  for (i in seq_len(plots_per_pg - remainder)) plot.new()
}

for(k in seq_along(active_cols)){
  col_idx <- active_cols[k]
  fid     <- fleet_id_of_col[col_idx]
  gid     <- group_id_of_col[col_idx]
  panel_title <- paste0(fleet_of_col[col_idx], " | ", group_of_col[col_idx])

  simC    <- sim_fg_by_col[[col_idx]]
  spaC_ls <- lapply(ls_spaC_xY, function(df) df[, col_idx])

  simC_scaled    = f.scale_or_raw(simC)
  spaC_scaled_ls = lapply(spaC_ls, f.scale_or_raw)

  ## Observed series for this (fleet, group)
  obs_cols      <- which(obsC_fleet == fid & obsC_group == gid)
  obs_scaled_ls <- list()
  for (m in obs_cols) {
    obs_series = as.numeric(obsC[, m])
    denom      = mean(obs_series[1:init_years_toscale], na.rm = TRUE)
    if (is.finite(denom) && denom > 0) {
      obs_scaled_ls[[length(obs_scaled_ls) + 1]] = obs_series / denom
    }
  }

  ## Legend page (once per page)
  if(k %in% seq(1, num_panels, by = legend_step)){
    plot(0, 0, type = 'n', xlim = c(0, 1), ylim = c(0, 1),
         xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', bty = 'n')
    legend(leg_pos, inset = 0.1, bg = "gray90", box.lty = 0,
           legend = c('Observed', 'Ecosim', spa_scen_names),
           lty    = c(NA, sim_lty, rep(spa_lty, length(spaC_scaled_ls))),
           lwd    = c(NA, sim_lwd + 1, rep(spa_lwd + 1, length(spaC_scaled_ls))),
           pch    = c(obs_pch, NA, rep(NA, length(spaC_scaled_ls))),
           col    = c('black', col_sim, col_spa),
           cex    = leg_cex)
  }

  ## Data panel
  min = min(simC_scaled, unlist(spaC_scaled_ls), unlist(obs_scaled_ls), na.rm = TRUE) * 0.9
  max = max(simC_scaled, unlist(spaC_scaled_ls), unlist(obs_scaled_ls), na.rm = TRUE) * 1.1
  plot(x, rep("", length(x)), type = 'b',
       ylim = c(min, max), xaxt = 'n', yaxt = 'n',
       xlab = '', ylab = '', bty = 'n')
  title(main = panel_title, line = -.6, cex.main = main_cex * 0.75)

  x_ticks = pretty(x, n = round((end_y - start_y) / x_break))
  axis(1, at = x_ticks,
       labels   = paste0("'", substring(format(x_ticks, "%Y"),
                                        nchar(format(x_ticks, "%Y")) - 1)),
       cex.axis = x_cex, las = x_las)
  y_ticks = pretty(seq(min, max, by = (max - min) / 10), n = y_break)
  axis(2, at = y_ticks, labels = y_ticks, las = 1, cex.axis = y_cex)
  abline(h = 1, col = 'lightgray')

  lines(x, simC_scaled, lty = sim_lty, lwd = sim_lwd, col = col_sim)
  for(j in seq_along(spaC_scaled_ls)){
    lines(x, spaC_scaled_ls[[j]], lty = spa_lty, lwd = spa_lwd, col = col_spa[j])
  }
  for(obs_scaled in obs_scaled_ls){
    points(obs_dates, obs_scaled, pch = obs_pch, cex = obs_cex, col = 'black')
  }
}
dev.off()

