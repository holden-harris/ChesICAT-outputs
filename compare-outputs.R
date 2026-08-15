## Naming conventions
## 'sim' --> Related to Ecosim
## 'spa' --> Related to Ecospace
## 'obs' --> Related to observed timeseries data, i.e., that Ecosim was fitted to.
## 'B'   --> Denotes biomass
## 'C'   --> Denotes catch

## NOTE: relative paths below assume the working directory is the repo root.
## Opening ChesICAT-outputs.Rproj in RStudio sets this automatically.

## Setup -----------------------------------------------------------------------
rm(list=ls())
source("./functions.R") ## Pull in functions
library(dplyr)

## Input set up ----------------------------------------------------------------
ewe_out_fold = "ewe-outputs/model-setups"
sim_scenario = "ecosim_sim_01.3_SM2-fit"
obs_TS_name  = "ewe-outputs/timeseries/ts_v1.4.csv"
srt_year     = 2001

## Auto-detect Ecospace scenarios --------------------------------------------
## Any subfolder under ewe_out_fold whose name starts with "spa_" is treated
## as an Ecospace scenario to be compared.
spa_scenarios  = list.dirs(ewe_out_fold, recursive = FALSE, full.names = FALSE)
spa_scenarios  = spa_scenarios[grepl("^spa_", spa_scenarios)]
spa_scen_names = spa_scenarios  ## override manually if prettier labels are wanted
out_file_notes = "auto-detected"
dir_out        = "./Compare-outputs/"
col_spa        = hcl.colors(max(length(spa_scenarios), 1), palette = "Dark 3")

## Check if the files for the scenarios exist
for (i in 1:length(spa_scenarios)) {
  dir_spa <- paste0("./", ewe_out_fold, "/", spa_scenarios[i], "/")
  if (dir.exists(dir_spa)) print(paste0("Directory exists: ", dir_spa))
  else message("Directory does NOT exist: ", dir_spa)
}

## Create the output folder if it doesn't exist
if (!dir.exists(dir_out)) {
  dir.create(dir_out, recursive = TRUE)  ## Create the folder if it doesn't exist
}

## User-defined parameters for plotting-----------------------------------------
num_plot_pages = 1 ## Sets number of pages for PDF file
init_years_toscale = 1 ## In plotting, this sets the "1 line" to the average of this number of years
plot_name_xY = paste0("BxY_scaled_", init_years_toscale, "y-", out_file_notes, ".PDF")

## Plot output names
dir_pdf_out  = paste0(dir_out)  ## Folder for plots
dir_tab_out  = paste0(dir_out) ## Folder for tables with fit metrics
if (!dir.exists(dir_pdf_out)) dir.create(dir_pdf_out, recursive = TRUE) ## Create the folder if it doesn't exist
if (!dir.exists(dir_tab_out)) dir.create(dir_tab_out, recursive = TRUE) ## Create the folder if it doesn't exist
folder_name <- paste0(dir_tab_out, "Scaled_", init_years_toscale, "y") ## Folder name based on `init_years_toscale`
pdf_file_name_xY = paste0(dir_pdf_out, plot_name_xY)




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

## Read-in functional-group lookup (pool code -> group name) ------------------
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

## Loop through scenarios to populate lists
for (i in 1:length(spa_scenarios)) {
  (dir_spa = paste0("./", ewe_out_fold, "/", spa_scenarios[i], "/"))
  
  ## Read in Annual Biomass
  filename <- paste0("Ecospace_Annual_Average_Biomass.csv")    ## Set filename
  filepath <- paste0(dir_spa, filename)                        ## Set filepath based on directory 
  num_skip_spa <- f.find_start_line(filepath, flag = "Year")   ## Apply function to find the start line to start reading table
  spaB_xY <- read.csv(filepath, skip = num_skip_spa, header = TRUE); spaB_xY$Year = NULL
  
  ## Read in Monthly Biomass
  filename <- paste0("Ecospace_Average_Biomass.csv")
  filepath <- paste0(dir_spa, filename)
  num_skip_spa <- f.find_start_line(filepath, flag = "TimeStep")
  spaB_xM <- read.csv(filepath, skip = num_skip_spa, header = TRUE); spaB_xM$TimeStep = NULL
  
  ## Read in Annual Catches
  filename <- paste0("Ecospace_Annual_Average_Catch.csv")        ## Set filename
  filepath <- paste0(dir_spa, filename)                          ## Set filepath based on directory 
  num_skip_spa <- f.find_start_line(filepath, flag = "Year") ## Apply function to find the start line to start reading table
  spaC_xY <- read.csv(filepath, skip = num_skip_spa, header = TRUE); spaC_xY$Year = NULL
  
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
  
  ## Add to list
  ls_spaB_xY[[i]] <- spaB_xY
  #  ls_spaC_xY[[i]] <- spaC_xY
}


## -----------------------------------------------------------------------------
##
## Plot biomasses
## Note: Make sure PDF readers are closed before running pdf()

## Setup for plots -----------------------------------------------------------

## Plotting parameters
col_sim = rgb(0.2, 0.7, .1, alpha = 0.6) ## rgb (red, green, blue, alpha)
#col_spa <- adjustcolor(col_spa, alpha.f = 1) ## Adjust transparancy

num_plot_pages = 1; x_break = 5; y_break = 4; x_cex = 0.9; y_cex = 0.9; x_las = 2;
sim_lty = 1; spa_lty = 1
sim_lwd = 2; spa_lwd = 1; obs_pch = 16; obs_cex = 0.8;
main_cex = 0.85; leg_cex = 0.9; leg_pos = 'topleft';leg_inset = 0.1
#simB_scaled = spaB_scaled_ls

## Set number of plots per page
set.mfrow = f.get_plot_dims(x=num_fg / num_plot_pages, round2=4)
par(mfrow=set.mfrow, mar=c(1, 2, 1, 2))
plots_per_pg = 9

## -----------------------------------------------------------------------------
##
## Plot by Year (xY)

pdf_file_name_xY = "Scenario-comp.pdf"
pdf(pdf_file_name_xY, onefile = TRUE)

print(paste("Writing", pdf_file_name_xY))
x = year_series

## Set number of plots per page
set.mfrow = f.get_plot_dims(x=num_fg / num_plot_pages, round2=4)
par(mfrow=set.mfrow, mar=c(1, 2, 1, 2))
plots_per_pg = set.mfrow[1] * set.mfrow[2]

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
dev.off()
