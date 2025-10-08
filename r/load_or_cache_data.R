#load_or_cache_data.R
require(tidyverse)
require(xfun)
local_cache_dir <- "~/nflverse_data"
dir_create(local_cache_dir)  # ensure local cache directory exists
load_or_cache_data <- function(load_fun,
                               file_stub,
                               seasons = NULL,
                               refresh_seasons = NULL,
                               cache_dir = local_cache_dir) {
  fs::dir_create(cache_dir)
  results <- list()
  
  # --- Handle seasonless datasets ---
  if (is.null(seasons)) {
    cache_file <- file.path(cache_dir, paste0(file_stub, ".rds"))
    
    if (!file.exists(cache_file) || isTRUE(refresh_seasons)) {
      message("⬇️  Downloading ", file_stub, " from nflreadr...")
      data <- load_fun()
      saveRDS(data, cache_file)
    } else {
      message("💾 Loading ", file_stub, " from cache...")
      data <- readRDS(cache_file)
    }
    
    return(data)
  }
  
  # --- Handle seasonal datasets ---
  for (season in seasons) {
    cache_file <- file.path(cache_dir, paste0(file_stub, "_", season, ".rds"))
    
    if (!file.exists(cache_file) || season %in% refresh_seasons) {
      message("⬇️  Downloading ", file_stub, " for season ", season, "...")
      data <- load_fun(season)
      saveRDS(data, cache_file)
    } else {
      message("💾 Loading ", file_stub, " for season ", season, " from cache...")
      data <- readRDS(cache_file)
    }
    
    results[[as.character(season)]] <- data
  }
  
  # --- Reconcile column names ---
  all_cols <- unique(unlist(lapply(results, names)))
  
  # --- Detect column types across all data frames ---
  col_type_map <- lapply(all_cols, function(colname) {
    types <- sapply(results, function(df) {
      if (colname %in% names(df)) {
        typeof(df[[colname]])
      } else {
        NA
      }
    })
    if ("character" %in% types) return("character")
    if ("double" %in% types) return("double")
    if ("integer" %in% types) return("integer")
    if ("logical" %in% types) return("logical")
    return("character")  # default fallback
  })
  names(col_type_map) <- all_cols
  
  # --- Harmonize data frames ---
  results_harmonized <- lapply(results, function(df) {
    for (col in all_cols) {
      if (!col %in% names(df)) {
        df[[col]] <- NA
      }
      # Force to common type
      target_type <- col_type_map[[col]]
      if (target_type == "character") {
        df[[col]] <- as.character(df[[col]])
      } else if (target_type == "double") {
        df[[col]] <- as.numeric(df[[col]])
      } else if (target_type == "integer") {
        df[[col]] <- as.integer(df[[col]])
      } else if (target_type == "logical") {
        df[[col]] <- as.logical(df[[col]])
      }
    }
    
    # Reorder to consistent column order
    df <- df[, all_cols]
    return(df)
  })
  
  # --- Combine safely ---
  dplyr::bind_rows(results_harmonized)
}