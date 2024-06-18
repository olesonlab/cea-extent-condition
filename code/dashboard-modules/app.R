library(shiny)
library(googlesheets4)
library(tidyverse)

# Function to import and process Google Sheets data
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

ui <- fluidPage(
  titlePanel("Google Sheets Authentication"),
  sidebarLayout(
    sidebarPanel(
      # Step 1: Authenticate Google Service Account
      h4("Step 1: Authenticate Google Service Account"),
      textInput("email", "Service Account Email:"),
      fileInput("key", "Upload JSON Key File:"),
      actionButton("authenticate", "Authenticate"),
      
      # Divider
      div(style = "border-top: 1px solid #ccc; margin-top: 20px;"),
      
      # Step 2: Enter Conditions URL
      h4("Step 2: Enter Conditions Google Sheets URL", style = "margin-top: 10px;"),
      textInput("conditions_url", "Conditions Google Sheets URL"),
      actionButton("load_data", "Load Data")
    ),
    mainPanel(
      # This main panel can be used to display any results or messages
    )
  )
)

server <- function(input, output, session) {
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
          HTML(paste("<center>Authentication failed</center>:", e$message)),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      })
    })
  })
  
  observeEvent(input$load_data, {
    req(input$conditions_url)
    
    showModal(modalDialog(
      title = HTML("<strong>Loading Data</strong>"),
      "Please wait while data is being loaded...",
      easyClose = FALSE,
      footer = NULL
    ))
    
    tryCatch({
      mhi_data <- import_mhi_extents_and_conditions_gs(input$conditions_url)
      removeModal()
      showNotification("Data successfully loaded!", type = "message")
    }, error = function(e) {
      removeModal()
      showNotification(paste("Failed to load data:", e$message), type = "error")
    })
  })
}

shinyApp(ui = ui, server = server)