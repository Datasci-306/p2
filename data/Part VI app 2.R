library(shiny)
library(tidyverse)

# --- Load Data ---
title_principals <- read_rds("title_principals.rda")
name_basics <- read_rds("name_basics.rda")
title_basics <- read_rds("title_basics.rda")

# --- Preprocessing ---
categories <- sort(unique(title_principals$category))

# Pre-join for faster filtering
title_principals_people <- title_principals %>%
  inner_join(name_basics, by = "nconst")

# --- UI ---
ui <- fluidPage(
  
  titlePanel("IMDb Principals Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      # Step 1: Select Category
      selectInput(
        "category",
        "Select a Category:",
        choices = categories
      ),
      
      # Step 2: Select Specific Job (dynamic)
      uiOutput("job_ui")
    ),
    
    mainPanel(
      h4("People Info (Filtered)"),
      dataTableOutput("filtered_results")
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  # Reactive: jobs available for selected category
  available_jobs <- reactive({
    req(input$category)
    
    title_principals %>%
      filter(category == input$category) %>%
      pull(job) %>%
      unique() %>%
      na.omit() %>%
      sort()
  })
  
  # Step 2: Show job choices
  output$job_ui <- renderUI({
    req(input$category)
    
    if (length(available_jobs()) == 0) {
      return(NULL)  # If no jobs, skip
    } else {
      selectInput(
        "job",
        "Select a Specific Job:",
        choices = available_jobs()
      )
    }
  })
  
  # Final filtered data
  filtered_data <- reactive({
    req(input$category)
    
    filtered_people <- title_principals_people %>%
      filter(category == input$category)
    
    if (length(available_jobs()) > 0) {
      req(input$job)
      filtered_people <- filtered_people %>%
        filter(job == input$job)
    }
    
    filtered_people %>%
      select(primaryName, birthYear, deathYear, primaryProfession, knownForTitles) %>%
      distinct()
  })
  
  # Render table
  output$filtered_results <- renderDataTable({
    filtered_data()
  })
}

# --- Run the App ---
shinyApp(ui = ui, server = server)