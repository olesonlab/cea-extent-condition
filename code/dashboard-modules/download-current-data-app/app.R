if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(googlesheets4, here, shiny, googlesheets4, dplyr, tidyr, purrr, stringr, yaml, progress, gt, readr)

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

conditions_keyword_searches <- list(
  list(keyword = "ISLAND", target_col_name = "temp_5", new_col_name = "island"),
  list(keyword = "MOKU", target_col_name = "temp_5", new_col_name = "moku"),
  list(keyword = "Ecosystem Type", target_col_name = "temp_5", new_col_name = "ecosystem_type")
)

rename_conditions_cols <- c("island", "moku", "ecosystem_type", "typology_class",
                            "descriptor", "unit", "Variable Values: Opening Value",
                            "Variable Values: Closing Value", 
                            "Actively Managed or Conserved", 
                            "Reference Level Values: Upper Level",
                            "Reference Level Values: Lower Level",
                            "Indicator Values (Rescaled): Opening Value",
                            "Indicator Values (Rescaled): Closing Value",
                            "Indicator Values (Rescaled): Change in Indicator")

conditions_cols_to_sep <- list(
  list(source_column = "categories", into = c("category", "measurement"), sep = ": ", convert = TRUE
  ),
  list(source_column = "typology_class", into = c("class_code", "typology"), sep = "\\) ", convert = TRUE
  )
)

# ui <- fluidPage(
#   # titlePanel(HTML("<b><center>Exploring the Condition Accounts of the Main Hawaiian Islands: A Data Viewing and Export Tool</center></b>")),
#   sidebarLayout(
#     sidebarPanel(
#       textInput("email", "Service Account Email:"),
#       fileInput("key", "Upload JSON Key File:"),
#       actionButton("authenticate", "Authenticate"),
#       div(style = "border-top: 1px solid #ccc; margin-top: 20px;"),
# 
#       div(style = "margin-top: 10px;"),
#       textInput("conditions_url", "Step 1: Enter Conditions Google Sheets URL"),
#       div(style = "margin-top: 10px;"),
#       actionButton("load_data", "Load Data"),
#       
#       div(style = "margin-top: 10px;"),
#       checkboxGroupInput("dataset_selector", "Step 2: Select Dataset", choices = NULL),
#       tags$div("This section will populate once Step 2 is complete.", style = "color: red;"),
#       div(style = "margin-top: 10px;"),
#       actionButton("toggle_selection", "Select/Deselect All"),
#       
#       div("Step 3: Download Dataset as CSV", style = "font-weight: bold; margin-top: 10px;"),
#       div(style = "margin-top: 10px;"),
#       textInput("file_name", "Enter File Name", value = paste0(format(Sys.Date(), "%Y-%m-%d"), "_name-of-dataset")),
#       downloadButton("download_data", "Download", style = "margin-top: 5px;"),
#       
#       div(id = "progress")
#     ),
#     mainPanel(
#       gt_output("selected_dataset") 
#     )
#   )
# )

ui <- fluidPage(
  # Reintroduced the title panel with HTML for centered and bold text
  # titlePanel(HTML("<b><center>Exploring the Condition Accounts of the Main Hawaiian Islands: A Data Viewing and Export Tool</center></b>")),
  
  sidebarLayout(
    sidebarPanel(
      # Step 1: Authenticate Google Service Account
      h4(HTML("<b>Step 1: Authenticate Google Service Account</b>")),
      textInput("email", "Service Account Email:"),
      fileInput("key", "Upload JSON Key File:"),
      actionButton("authenticate", "Authenticate"),
      
      # Divider
      div(style = "border-top: 1px solid #ccc; margin-top: 20px;"),
      div(style = "margin-top: 10px;"),
      
      # Step 2: Enter Conditions URL
      div(style = "margin-top: 20px;"),
      h4(HTML("<b>Step 2: Enter Conditions Google Sheets URL</b>"), style = "margin-top: 10px;"),
      textInput("conditions_url", "Conditions Google Sheets URL"),
      actionButton("load_data", "Load Data"),
      
      # Divider
      div(style = "border-top: 1px solid #ccc; margin-top: 20px;"),
      div(style = "margin-top: 10px;"),
      
      # Step 3: Selection Tools
      div(style = "margin-top: 20px;"),
      h4(HTML("<b>Step 3: Select Dataset(s)</b>"), style = "margin-top: 10px;"),
      tags$div("This section will populate once data is loaded. Once loaded, please allow a moment for the data tables to load when making a selection.", style = "color: red;"),
      div(style = "margin-top: 10px;"),
      checkboxGroupInput("dataset_selector", "Select Dataset", choices = NULL),
      actionButton("toggle_selection", "Select/Deselect All"),
      
      # Divider
      div(style = "border-top: 1px solid #ccc; margin-top: 20px;"),
      div(style = "margin-top: 10px;"),
      
      # Step 4: Download Dataset
      div(style = "margin-top: 20px;"),
      h4(HTML("<b>Step 4: Download Dataset as CSV</b>"), style = "margin-top: 10px;"),
      textInput("file_name", "Enter File Name", value = paste0(format(Sys.Date(), "%Y-%m-%d"), "_name-of-dataset")),
      downloadButton("download_data", "Download", style = "margin-top: 5px;"),
      
      # Placeholder for potential progress bars or other dynamic UI elements
      div(id = "progress")
    ),
    mainPanel(
      # Main panel for displaying the selected dataset
      gt_output("selected_dataset")
    )
  )
)

server <- function(input, output, session) {
  mhi_conditions_dfs <- reactiveVal()
  choices <- reactiveVal(character(0))
  file_name <- reactiveVal(paste0(format(Sys.Date(), "%Y-%m-%d"), "_name-of-datasets"))
  
  observeEvent(input$authenticate, {
    req(input$email, input$key)  # Ensure both email and key are provided
    
    # Validate the email format
    if (!grepl("^[a-z0-9._%+-]+@[a-z0-9.-]+\\.iam\\.gserviceaccount\\.com$", input$email, ignore.case = TRUE)) {
      showModal(modalDialog(
        title = HTML("<strong>Invalid Email Format</strong>"),
        HTML("<div style='text-align: center;'>The service account email should follow the format: 'something@<project-name>.iam.gserviceaccount.com'.<br><br>Please check your input and try again.</div>"),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }
    
    withProgress(message = 'Authenticating...', {
      setProgress(0.5)
      Sys.sleep(1)  # Simulate some delay
      
      key_path <- input$key$datapath
      tryCatch({
        gs4_auth(
          email = input$email, 
          path = key_path,
          scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")
        setProgress(1)
        showModal(modalDialog(
          title = HTML("<strong>Authentication Status</strong>"),
          HTML("<center>Authentication successful!</center>"),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }, error = function(e) {
        setProgress(1)
        showModal(modalDialog(
          title = HTML("<strong>Authentication Status</strong>"),
          HTML("<center>Authentication successful!</center>"),
          HTML(paste("<center>Authentication failed</center>:", e$message)),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      })
    })
  })
  
  observeEvent(input$authenticate, {
    email <- input$email
    key <- input$key
    if (is.null(email) || is.null(key)) {
      output$auth_message <- renderText("Please provide both email and key file.")
      return()
    }
    key_path <- key$datapath
    if (file.exists(key_path)) {
      tryCatch({
        gs4_auth(
          email = email, 
          path = key_path,
          scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")
        output$auth_message <- renderText("Authentication successful!")
      }, error = function(e) {
        output$auth_message <- renderText(paste("Authentication failed:", e$message))
      })
    } else {
      output$auth_message <- renderText("Key file not found.")
    }
  })
  
  observeEvent(input$file_name, {
    file_name(input$file_name)
  })
  
  observeEvent(input$load_data, {
    req(input$conditions_url)
    
    showModal(modalDialog(
      title = HTML("<strong>Loading Data</strong>"),
      div("Note: This step will take a few minues to complete.", icon("refresh", class = "fa-spin"), style = "text-align: center;")
    ))
    
    mhi_conditions_dfs_data <- import_mhi_extents_and_conditions_gs(input$conditions_url)
    
    Sys.sleep(2)
    
    mhi_conditions_dfs_data <- mhi_conditions_dfs_data[!grepl("^clean_", names(mhi_conditions_dfs_data))]
    
    mhi_conditions_dfs(mhi_conditions_dfs_data)
    
    choices(names(mhi_conditions_dfs_data))
    updateCheckboxGroupInput(session, "dataset_selector", "Step 2: Select Dataset", choices = choices(), selected = NULL)
    
    removeModal()
    
    showNotification("Data successfully loaded!", duration = 10)
  })
  
  observeEvent(input$toggle_selection, {
    if (length(input$dataset_selector) == length(choices())) {
      updateCheckboxGroupInput(session, "dataset_selector", choices = choices(), selected = NULL)
    } else {
      updateCheckboxGroupInput(session, "dataset_selector", choices = choices(), selected = choices())
    }
  })
  
  output$selected_dataset <- render_gt({
    req(input$dataset_selector)  
    withProgress(message = 'Generating table...', value = 0, {
      incProgress(1/3, detail = "Preparing data...")  
      
      datasets <- lapply(input$dataset_selector, function(selected_dataset) {
        tidy_conditions_data(mhi_conditions_dfs()[[selected_dataset]], conditions_keyword_searches, rename_conditions_cols, conditions_cols_to_sep) 
      })
      incProgress(1/3, detail = "Combining data...")  
      
      combined_dataset <- do.call(rbind, datasets)
      
      combined_dataset <- combined_dataset %>%
        mutate(
          moku = if_else(moku == "Ko‘olau", "Koʻolau", moku),
          moku_name2 = case_when(
            moku == "Kona" & island == "Hawaiʻi" ~ "KONA HAW",
            moku == "Kāʻu" & island == "Hawaiʻi" ~ "KAU",
            moku == "Puna" & island == "Hawaiʻi" ~ "PUNA HAW",
            moku == "Hilo" & island == "Hawaiʻi" ~ "HILO",
            moku == "Hāmākua" & island == "Hawaiʻi" ~ "HAMAKUA",
            moku == "Kohala" & island == "Hawaiʻi" ~ "KOHALA",
            moku == "Waiʻanae" & island == "Oʻahu" ~ "WAIANAE",
            moku == "ʻEwa" & island == "Oʻahu" ~ "EWA",
            moku == "Kona" & island == "Oʻahu" ~ "KONA OAH",
            moku == "Koʻolaupoko" & island == "Oʻahu" ~ "KOOLAUPOKO",
            moku == "Koʻolauloa" & island == "Oʻahu" ~ "KOOLAULOA",
            moku == "Waialua" & island == "Oʻahu" ~ "WAIALUA",
            moku == "Mana" & island == "Kauaʻi" ~ "MANA",
            moku == "Kona" & island == "Kauaʻi" ~ "KONA KAU",
            moku == "Puna" & island == "Kauaʻi" ~ "PUNA KAU",
            moku == "Koʻolau" & island == "Kauaʻi" ~ "KOOLAU KAU",
            moku == "Haleleʻa" & island == "Kauaʻi" ~ "HALELEA",
            moku == "Nāpali" & island == "Kauaʻi" ~ "NAPALI",
            moku == "Kona" & island == "Kahoʻolawe" ~ "KONA KAH",
            moku == "Koʻolau" & island == "Kahoʻolawe" ~ "KOOLAU KAH",
            moku == "Kona" & island == "Lānaʻi" ~ "KONA LAN",
            moku == "Koʻolau" & island == "Lānaʻi" ~ "KOOLAU LAN",
            moku == "Kualuakoʻi" & island == "Molokaʻi" ~ "KALUAKOI",
            moku == "Pālāʻau" & island == "Molokaʻi" ~ "PALAAU",
            moku == "Kona" & island == "Molokaʻi" ~ "KONA MOL",
            moku == "Hālawa" & island == "Molokaʻi" ~ "HALAWA",
            moku == "Koʻolau" & island == "Molokaʻi" ~ "KOOLAU MOL",
            moku == "Lāhainā" & island == "Maui" ~ "LAHAINA",
            moku == "Kealaloloa" & island == "Maui" ~ "KEALALOLOA",
            moku == "Kula" & island == "Maui" ~ "KULA",
            moku == "Honuaʻula" & island == "Maui" ~ "HONUAULA",
            moku == "Kahikinui" & island == "Maui" ~ "KAHIKINUI",
            moku == "Kaupo" & island == "Maui" ~ "KAUPO",
            moku == "Kīpahulu" & island == "Maui" ~ "KIPAHULU",
            moku == "Hāna" & island == "Maui" ~ "HANA",
            moku == "Koʻolau" & island == "Maui" ~ "KOOLAU MAU",
            moku == "Hāmākualoa" & island == "Maui" ~ "HAMAKUALOA",
            moku == "Hāmākuapoko" & island == "Maui" ~ "HAMAKUAPOKO",
            moku == "Wailuku" & island == "Maui" ~ "WAILUKU",
            moku == "Kāʻanapali" & island == "Maui" ~ "KAANAPALI"
          )
        ) %>% 
        relocate(moku_name2, .after = moku)
      
      incProgress(1/3, detail = "Rendering table...")  
      
      combined_dataset %>%
        gt() %>%
        opt_interactive(
          .,
          use_sorting = TRUE,
          use_search = TRUE,
          use_filters = TRUE,
          use_resizers = TRUE,
          page_size_default = 10
        )
    })
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(input$file_name, ".csv")
    },
    content = function(file) {
      datasets <- lapply(input$dataset_selector, function(selected_dataset) {
        df <- tidy_conditions_data(mhi_conditions_dfs()[[selected_dataset]], conditions_keyword_searches, rename_conditions_cols, conditions_cols_to_sep)
        
        df$moku <- iconv(df$moku, "UTF-8", "UTF-8", sub = "")
        
        df <- df %>%
          mutate(
            moku = if_else(moku == "Ko‘olau", "Koʻolau", moku),
            moku_name2 = case_when(
              moku == "Kona" & island == "Hawaiʻi" ~ "KONA HAW",
              moku == "Kāʻu" & island == "Hawaiʻi" ~ "KAU",
              moku == "Puna" & island == "Hawaiʻi" ~ "PUNA HAW",
              moku == "Hilo" & island == "Hawaiʻi" ~ "HILO",
              moku == "Hāmākua" & island == "Hawaiʻi" ~ "HAMAKUA",
              moku == "Kohala" & island == "Hawaiʻi" ~ "KOHALA",
              moku == "Waiʻanae" & island == "Oʻahu" ~ "WAIANAE",
              moku == "ʻEwa" & island == "Oʻahu" ~ "EWA",
              moku == "Kona" & island == "Oʻahu" ~ "KONA OAH",
              moku == "Koʻolaupoko" & island == "Oʻahu" ~ "KOOLAUPOKO",
              moku == "Koʻolauloa" & island == "Oʻahu" ~ "KOOLAULOA",
              moku == "Waialua" & island == "Oʻahu" ~ "WAIALUA",
              moku == "Mana" & island == "Kauaʻi" ~ "MANA",
              moku == "Kona" & island == "Kauaʻi" ~ "KONA KAU",
              moku == "Puna" & island == "Kauaʻi" ~ "PUNA KAU",
              moku == "Koʻolau" & island == "Kauaʻi" ~ "KOOLAU KAU",
              moku == "Haleleʻa" & island == "Kauaʻi" ~ "HALELEA",
              moku == "Nāpali" & island == "Kauaʻi" ~ "NAPALI",
              moku == "Kona" & island == "Kahoʻolawe" ~ "KONA KAH",
              moku == "Koʻolau" & island == "Kahoʻolawe" ~ "KOOLAU KAH",
              moku == "Kona" & island == "Lānaʻi" ~ "KONA LAN",
              moku == "Koʻolau" & island == "Lānaʻi" ~ "KOOLAU LAN",
              moku == "Kualuakoʻi" & island == "Molokaʻi" ~ "KALUAKOI",
              moku == "Pālāʻau" & island == "Molokaʻi" ~ "PALAAU",
              moku == "Kona" & island == "Molokaʻi" ~ "KONA MOL",
              moku == "Hālawa" & island == "Molokaʻi" ~ "HALAWA",
              moku == "Koʻolau" & island == "Molokaʻi" ~ "KOOLAU MOL",
              moku == "Lāhainā" & island == "Maui" ~ "LAHAINA",
              moku == "Kealaloloa" & island == "Maui" ~ "KEALALOLOA",
              moku == "Kula" & island == "Maui" ~ "KULA",
              moku == "Honuaʻula" & island == "Maui" ~ "HONUAULA",
              moku == "Kahikinui" & island == "Maui" ~ "KAHIKINUI",
              moku == "Kaupo" & island == "Maui" ~ "KAUPO",
              moku == "Kīpahulu" & island == "Maui" ~ "KIPAHULU",
              moku == "Hāna" & island == "Maui" ~ "HANA",
              moku == "Koʻolau" & island == "Maui" ~ "KOOLAU MAU",
              moku == "Hāmākualoa" & island == "Maui" ~ "HAMAKUALOA",
              moku == "Hāmākuapoko" & island == "Maui" ~ "HAMAKUAPOKO",
              moku == "Wailuku" & island == "Maui" ~ "WAILUKU",
              moku == "Kāʻanapali" & island == "Maui" ~ "KAANAPALI"
            )
          ) %>% 
          relocate(moku_name2, .after = moku)
        
        return(df)
      })
      combined_dataset <- do.call(rbind, datasets)
      readr::write_excel_csv(combined_dataset, file)
      
      showNotification("Data successfully exported as CSV file!", type = "message", duration = 10)
      
      gs4_deauth()
    }
  )
}

shinyApp(ui = ui, server = server)
