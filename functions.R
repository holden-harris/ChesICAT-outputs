##-------------------------------------------------------------------------------
## READ ECOSIM TIMESERIES FILE
##-------------------------------------------------------------------------------
## Reads an Ecosim fitted-timeseries CSV and returns four data frames:
## obsB.head, obsB, obsC.head, obsC.
## Header rows contain 'Title', 'Weight', 'Pool code', ('Pool code 2',) 'Type'.
## In ChesICAT ts_v1.4 there are 5 header rows; older files may have 4.
## The Type row is always the last header row (indexed by num_row_header).
## Biomass and catch series are selected by matching Type against
## biomass_types / catch_types. ChesICAT uses Type=1 for observed biomass
## and Type=12 for observed catches/landings.

f.read_ecosim_timeseries = function(filename, num_row_header = 5,
                                    biomass_types = c(0, 1),
                                    catch_types   = c(6, 61, -6, 12)){
  fnm.obs_ts = filename
  obs.ts.head = as.data.frame(t(read.csv(fnm.obs_ts,header=F,nrows=num_row_header)))
  names(obs.ts.head) = obs.ts.head[1,]; obs.ts.head = obs.ts.head[-1,]
  obs.ts.head[,2:num_row_header] = as.numeric(as.matrix(obs.ts.head[,2:num_row_header]))
  obs.ts = read.csv(fnm.obs_ts,header=F,skip=num_row_header)
  rownames(obs.ts) = obs.ts[,1]; obs.ts[,1] = NULL
  if(nrow(obs.ts.head) != ncol(obs.ts)) print('HEADER AND TIMESERIES DIMENSION DO NOT MATCH!!!')

  ## Make four dataframes based on Type row (last header row)
  obsB.head = obs.ts.head[obs.ts.head[,num_row_header] %in% biomass_types,]
  obsB      = obs.ts[,which(obs.ts.head[,num_row_header] %in% biomass_types)]
  obsC.head = obs.ts.head[obs.ts.head[,num_row_header] %in% catch_types,]
  obsC      = obs.ts[,which(obs.ts.head[,num_row_header] %in% catch_types)]
  names(obsB.head) = names(obsC.head) = gsub(" ","_",names(obsB.head))
  ret = list(obsB.head,obsB,obsC.head,obsC)
  names(ret) = c('obsB.head','obsB','obsC.head','obsC')
  return(ret)
}

##-------------------------------------------------------------------------------  
## FUNCTION TO STANDARDIZE EWE FUNCTIONAL GROUP NAMES
##-------------------------------------------------------------------------------
f.standardize_group_names <- function(fg_names){
  fg_names = sub("\\.$", "", fg_names)
  fg_names = sub("\\.\\.", ".", fg_names)
  fg_names = gsub("(\\d)\\.(\\d)", "\\1-\\2", fg_names)
  fg_names = gsub("\\.yr", "+yr", fg_names)
  fg_names = gsub("\\.", "_", fg_names)
  fg_names = gsub("yr", "", fg_names)
}

##-------------------------------------------------------------------------------  
## FUNCTION TO SET MULTIPANEL PLOT DIMENSIONS
##-------------------------------------------------------------------------------
f.get_plot_dims = function(x=nrow(obsB.head),round2=4){
  #x=nrow(df.names)/2;round2=4
  x2=plyr::round_any(x,round2,f=ceiling)
  xfact = numeric()
  for(i in 1:x2){
    if((x2 %% i) == 0){
      if(i == sqrt(x2)){xfact = c(xfact,c(i,i))
      } else {xfact = c(xfact,i)}
    }
  }
  mfrow.pars = rev(xfact[(length(xfact)/2):(1+length(xfact)/2)])
  return(mfrow.pars)
}

##-------------------------------------------------------------------------------  
## FUNCTION TO SET SET SKIPLINES FOR READING IN ECOSPACE FILES
##-------------------------------------------------------------------------------
## Find the line number where 'Year' appears in the first column
f.find_start_line <- function(filename, flag = 1980) {
  con <- file(filename, open = "r")
  line_number <- 0
  while(TRUE) {
    line <- readLines(con, n = 1)
    line_number <- line_number + 1
    if(length(line) == 0) { # End of file
      break
    }
    fields <- strsplit(line, ",")[[1]]
    # Check if fields has any elements before attempting to access its first element
    if(length(fields) >= 1 && fields[1] == flag) {
      close(con)
      return(line_number - 2) # Minus 2 because headers are counted in read.csv and we want to start on the next line
    }
  }
  close(con)
  stop(paste(flag, "not found in the first column."))
}

