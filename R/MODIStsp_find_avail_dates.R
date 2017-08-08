MODIStsp_find_avail_dates <- function(download_range,
                                      yy,
                                      start_year,
                                      end_year,
                                      start_date,
                                      end_date) {
  if (download_range == "full") {
    # Create string representing the dates to be processed in the case
    # of continuous processing
    
    if (yy == start_year & yy == end_year) {
      dates <- c(start_date, end_date)
    }
    
    if (yy == start_year & yy != end_year) {
      dates <- c(start_date, paste0(as.character(yy),
                                    ".12.31"))
    }
    
    if (yy != start_year & yy != end_year) {
      dates <- c(paste0(as.character(yy), ".1.1"),
                 paste0(as.character(yy), ".12.31"))
    }
    
    if (yy != start_year & yy == end_year) {
      dates <- c(paste0(as.character(yy), ".1.1"),
                 end_date)
    }
    return(dates)
  } else if (download_range == "seasonal") {
    # Create string representing the dates to be processed in the case
    # of splitted processing
    #
    # the starting month-day
    start_seas <- as.Date(strftime(
      as.Date(start_date, format = "%Y.%m.%d"), "0-%m-%d")
    )
    # the ending month-day
    end_seas   <- as.Date(strftime(
      as.Date(end_date, format = "%Y.%m.%d"), "0-%m-%d")
    )
    
    # TRUE if the period includes new year's eve, fasle if not
    nye_incl   <- start_seas > end_seas 
    
    if (!nye_incl) {
      dates    <- c(gsub(paste0("^", start_year), yy, start_date),
                    gsub(paste0("^", end_year),  yy, end_date))
    } else {
      
      if (yy == start_year & yy != end_year) {
        dates  <- c(gsub(paste0("^", start_year), yy, start_date), paste0(as.character(yy),
                                                                          ".12.31"))
      }
      
      if (yy != start_year & yy != end_year) {
        dates  <- c(paste0(as.character(yy), ".1.1"), gsub(paste0("^", end_year), yy, end_date),
                    gsub(paste0("^", start_year), yy, start_date), paste0(as.character(yy),
                                                                          ".12.31"))
      }
      
      if (yy != start_year & yy == end_year) {
        dates  <- c(paste0(as.character(yy), ".1.1"), gsub(paste0("^", end_year), yy, end_date))
      }
      
    }
    return(dates)
  } else stop("download_range value not valid (only \"full\" and \"seasonal\" are admitted).")
}