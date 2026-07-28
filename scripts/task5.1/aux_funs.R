

reproject_xy <- function(df, input_rst, 
                         lon_col = "decimalLongitude", 
                         lat_col = "decimalLatitude") {
  # check inputs
  if (!inherits(input_rst, "SpatRaster")) {
    stop("input_rst must be a terra SpatRaster")
  }
  if (!all(c(lon_col, lat_col) %in% names(df))) {
    stop("df must contain columns: ", paste(lon_col, lat_col, collapse = ", "))
  }
  
  # convert df -> sf POINTS in WGS84
  pts_wgs84 <- st_as_sf(
    df,
    coords    = c(lon_col, lat_col),
    crs       = 4326,
    remove    = FALSE
  )
  
  # grab target CRS from the terra raster
  target_crs <- crs(input_rst) 
  # (returns PROJ string or WKT; sf::st_transform will accept it)
  
  # reproject
  pts_proj <- st_transform(pts_wgs84, crs = target_crs)
  
  # extract XY matrix and return as data.frame
  xy <- st_coordinates(pts_proj)
  out <- data.frame(x = xy[,1], y = xy[,2])
  colnames(out) <- c(lon_col, lat_col)
  return(out)
}



add_suffix <- function(paths, suffix) {
  # paths: character vector of file paths
  # suffix: character vector of suffixes (recycled to length(paths))
  
  # helper to process a single file name
  add_suffix_one <- function(path, suff) {
    dir   <- dirname(path)
    file  <- basename(path)
    
    # split off the last extension (if any)
    #   group 1: all up to but not including the final ".ext"  
    #   group 2: the dot + extension, or "" if none
    parts <- sub("^(.*?)(\\.[^.]*)?$", "\\1;\\2", file, perl = TRUE)
    split <- strsplit(parts, ";", fixed = TRUE)[[1]]
    name  <- split[1]
    ext   <- split[2]
    
    # build new file name
    newfile <- paste0(name, suff, ext)
    file.path(dir, newfile)
  }
  
  # vectorise over inputs
  mapply(add_suffix_one, paths, 
         suff = rep(suffix, length.out = length(paths)),
         USE.NAMES = FALSE)
}


match_patterns <- function(pattern1, pattern2, pattern3, strings) {
  keep <- grepl(pattern1, strings) &
    grepl(pattern2, strings) &
    grepl(pattern3, strings)
  strings[keep]
}

match_two_patterns <- function(pattern1, pattern2, strings) {
  keep <- grepl(pattern1, strings) &
    grepl(pattern2, strings)
  strings[keep]
}

drop_class <- function(x, drop, lvl) {
  # x    : a single‐layer, factor‐valued SpatRaster
  # drop : character vector of land_cover names to remove
  # lvl  : data.frame with columns ID and land_cover (your lulc_df)
  
  # sanity checks
  if (!inherits(x, "SpatRaster")) 
    stop("`x` must be a terra SpatRaster")
  if (nlyr(x) != 1) 
    stop("`x` must have exactly one layer")
  if (!is.factor(x)) 
    stop("`x` must be a factor‐valued raster (use as.factor() first)")
  if (!all(c("ID","land_cover") %in% names(lvl)))
    stop("`lvl` must have columns named ID and land_cover")
  
  drop <- as.character(drop)
  not_found <- setdiff(drop, lvl$land_cover)
  if (length(not_found) > 0) 
    warning("These classes not in `lvl` and will be ignored: ", 
            paste(not_found, collapse=", "))
  
  # only keep the ones that actually match
  to_drop <- intersect(drop, lvl$land_cover)
  if (length(to_drop) == 0) {
    warning("No matching classes to drop; returning original raster")
    return(x)
  }
  
  # look up their integer codes
  id_rm <- lvl$ID[lvl$land_cover %in% to_drop]
  
  # set those pixels to NA
  y <- x
  y[y %in% id_rm] <- NA
  
  # rebuild the levels table, keeping all other IDs intact
  new_lvls <- lvl[ ! lvl$ID %in% id_rm, , drop=FALSE ]
  levels(y) <- new_lvls
  
  return(y)
}


get_creation_date <- function(path) {
  if (!file.exists(path)) {
    stop("File does not exist: ", path)
  }
  
  sys <- Sys.info()[["sysname"]]
  
  # Windows: file.info()$ctime is the true creation time
  if (sys == "Windows") {
    return(file.info(path)$ctime)
    
    # macOS: use `stat -f %B` to get birth time in seconds since epoch
  } else if (sys == "Darwin") {
    out <- suppressWarnings(system(paste("stat -f %B", shQuote(path)),
                                   intern = TRUE))
    if (length(out) == 1 && grepl("^[0-9]+$", out)) {
      return(as.POSIXct(as.numeric(out), origin = "1970-01-01"))
    }
    
    # Linux/other Unix: `stat -c %w` prints birth time or "-" if unavailable
  } else {
    out <- suppressWarnings(system(paste("stat -c %w", shQuote(path)),
                                   intern = TRUE))
    if (length(out) == 1 && out != "-") {
      return(as.POSIXct(out))
    }
  }
  
  # Fallback: on Unix, ctime is "last status change", not creation
  warning("Creation time unavailable; returning status‑change time (ctime)")
  file.info(path)$ctime
}


# Check whether all model_info columns are filled for a given speciesKey
# -----------------------------------------------------------------------------

is_model_info_complete <- function(model_info, key) {
  
  # columns you populate in your script
  cols_to_check <- c(
    "Final_model", "Threshold", "AUC", "PCC",
    "Sensitivity", "Specificity", "Kappa",
    "Morans_I_method", "Morans_I", "Pvalue_Morans_I",
    "n_presences",
    "correlation_glm_gbm", "correlation_glm_rf", 
    "correlation_glm_earth", "correlation_gbm_rf", 
    "correlation_gbm_earth", "correlation_rf_earth"
  )
  
  # subset to the row for this key
  sub <- model_info[model_info$speciesKey == key, , drop = FALSE]
  
  # no such speciesKey?
  if (nrow(sub) == 0) {
    warning("No entry found for speciesKey = ", key)
    return(FALSE)
  }
  
  # pick only the cols we care about
  # (if some cols aren't in model_info, error out early)
  missing_cols <- setdiff(cols_to_check, names(sub))
  if (length(missing_cols)) {
    stop("model_info is missing these required columns: ", paste(missing_cols, collapse = ", "))
  }
  sub_vals <- sub[, cols_to_check, drop = FALSE]
  
  # check for any NAs
  nas_by_col <- sapply(sub_vals, function(x) any(is.na(x)))
  if (any(nas_by_col)) {
    bad <- names(nas_by_col)[nas_by_col]
    message("speciesKey ", key, " has missing values in: ", paste(bad, collapse = ", "))
    return(FALSE)
  }
  
  # all good!
  return(TRUE)
}
