import_mhi_extents_and_conditions_gs <- function(url_str) {
  imported_sheets <- gs4_get(url_str) %>%
    pluck("sheets")
  
  sheet_names_lower <- tolower(imported_sheets$name)
  
  sheets_data <- purrr::map(imported_sheets$name, ~ {
    Sys.sleep(2) 
    read_sheet(url_str, sheet = .x)
  })
  
  names(sheets_data) <- sheet_names_lower
  
  return(sheets_data)
}

get_up_to_date_mhi_data <- function(extents_path, conditions_path) {
  mhi_extents_dfs <- import_mhi_extents_and_conditions_gs(extents_path)
  
  mhi_conditions_dfs <- import_mhi_extents_and_conditions_gs(conditions_path)
  
  mhi_dfs <- list(
    conditions = mhi_conditions_dfs,
    extents = mhi_extents_dfs
  )
  
  return(mhi_dfs)
}

subset_mhi_dfs <- function(mhi_dfs, subset, island_str = NULL, moku_str = NULL, extent_str = NULL) {
  if (!subset %in% c("conditions", "extents")) {
    stop("Subset must be either 'conditions' or 'extents'")
  }
  
  dfs <- mhi_dfs[[subset]]
  
  filtered_dfs <- list()
  
  for (name in names(dfs)) {
    df <- dfs[[name]]
    
    conditions_met <- c(
      is.null(island_str) || grepl(island_str, name, ignore.case = TRUE),
      is.null(moku_str) || grepl(moku_str, name, ignore.case = TRUE),
      is.null(extent_str) || grepl(extent_str, name, ignore.case = TRUE)
    )
    
    if (all(conditions_met)) {
      filtered_dfs[[name]] <- df
    }
  }
  
  return(filtered_dfs)
}

set_temp_column_names <- function(df) {
  df <- rbind(colnames(df), df)
  
  colnames(df) <- paste("temp", seq_len(ncol(df)), sep = "_")
  
  return(df)
}

create_keyword_search_columns <- function(df, conditions) {
  reduce(conditions, function(df, cond) {
    df %>%
      mutate(across(everything(), as.character)) %>%
      mutate(
        !!sym(cond$new_col_name) := if_else(
          rowSums(select(., -matches(cond$target_col_name)) == cond$keyword, na.rm = TRUE) > 0,
          .[[cond$target_col_name]],
          NA_character_
        )
      )
  }, .init = df)
}

fill_cols_down <- function(df, cols) {
  df %>%
    tidyr::fill(!!!syms(cols), .direction = "down")
}

relocate_cols <- function(df, col_names, before_col) {
  existing_col_names <- col_names[col_names %in% names(df)]
  if (before_col %in% names(df)) {
    df %>% 
      relocate(all_of(existing_col_names), .before = all_of(before_col))
  } else {
    warning("The specified 'before_col' does not exist in the dataframe. Relocating to the beginning.")
    df %>% 
      relocate(all_of(existing_col_names))
  }
}

remove_undesired_vals <- function(df) {
  df %>% 
    slice(-1) %>% 
    filter(!(is.na(temp_1) & is.na(temp_2))) %>% 
    select(where(~ !all(is.na(.)))) 
}

create_column_from_regex <- function(df, source_column, new_column, regex_pattern) {
  df %>% 
    mutate(!!new_column := str_extract(!!sym(source_column), regex_pattern)) %>% 
    select(-source_column)
}

separate_columns <- function(df, specifications) {
  reduce(specifications, function(df, spec) {
    df %>% 
      separate(
        !!spec$source_column,
        into = spec$into,
        sep = spec$sep,
        convert = spec$convert
      ) %>%
      mutate(across(all_of(spec$into), str_trim))
  }, .init = df)
}

tidy_conditions_data <- function(df, keyword_searches, col_names, cols_to_sep) {
  df %>%
    set_temp_column_names() %>%
    dplyr::select(c(1:11)) %>% 
    create_keyword_search_columns(keyword_searches) %>%
    fill_cols_down(c("island", "moku", "ecosystem_type")) %>% 
    relocate_cols(., c("island", "moku", "ecosystem_type"), "temp_1") %>% 
    remove_undesired_vals() %>% 
    set_names(col_names) %>% 
    filter(!(descriptor %in% c("Indicators", "Descriptor")) & 
             !grepl("\\*", typology_class) & 
             !grepl("\\^", typology_class)) %>% 
    pivot_longer(., cols = c(7:14), names_to = "categories", values_to = "value"
    ) %>% 
    separate_columns(cols_to_sep) %>% 
    fill_cols_down(., c("class_code", "typology", "unit")) %>% 
    mutate(
      class_code = str_remove(class_code, "^\\(Class "),
      measurement = if_else(is.na(measurement), "Actively Managed or Conserved", measurement),
      island = gsub("Oahu", "Oʻahu", island),
      moku = gsub("Ewa", "ʻEwa", moku)
    ) %>% 
    mutate(
      across(where(is.character), str_trim), 
      across(where(is.character), 
             ~gsub("/\\s+", "/", .)), 
      across(c(1:9), as.factor), 
      unit = if_else(descriptor == "Rugosity (predicted)", "-", unit),
      category = if_else(ecosystem_type == "Beaches/Dunes" & category == "Actively Managed or Conserved", "Reference Extent Publicly Owned", category),
      measurement = if_else(ecosystem_type == "Beaches/Dunes" & category == "Reference Extent Publicly Owned", "Reference Extent Publicly Owned", measurement)
    ) %>% 
    filter(measurement != "Change in Indicator") %>% 
    mutate(value = strsplit(value, ",")) %>%
    unnest(value)
}

export_tidied_dfs_to_csv <- function(df, desc_name, dir_path = "data/tidied/") {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  file_path <- paste0(dir_path,
                      format(Sys.Date(), "%Y-%m-%d"),
                      "_tidied-", desc_name, ".csv")
  
  write.csv(df, file_path, row.names = FALSE)
  
  message("File saved to: ", file_path)
}