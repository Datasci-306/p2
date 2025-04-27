library(shiny)
library(tidyverse)

# Load data
title_principals <- read_rds("title_principals.rda")
name_basics <- read_rds("name_basics.rda")
title_basics <- read_rds("title_basics.rda")

# Group the categories based on your real data
job_groups <- list(
  "Acting" = c("actor", "actress", "self", "archive_footage", "archive_sound"),
  "Directing/Production" = c("director", "producer", "casting_director", "production_designer"),
  "Writing" = c("writer"),
  "Music" = c("composer"),
  "Technical" = c("cinematographer", "editor")
)

ui <- fluidPage(
  
  titlePanel("IMDb Principals Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "job_group", 
        "Select Job Group:",
        choices = names(job_groups)
      ),
      
      uiOutput("category_ui"),
      uiOutput("person_ui")
    ),
    
    mainPanel(
      h4("Titles for Selected Person"),
      tableOutput("person_titles")
    )
  )
)

server <- function(input, output, session) {
  
  # 1. After selecting a job group, pick specific category
  output$category_ui <- renderUI({
    req(input$job_group)
    selectInput(
      "category", 
      "Select Specific Job Category:",
      choices = job_groups[[input$job_group]],
      selected = NULL
    )
  })
  
  # 2. After selecting category, pick person
  output$person_ui <- renderUI({
    req(input$category)
    people <- title_principals %>%
      filter(category == input$category) %>%
      distinct(nconst) %>%
      inner_join(name_basics, by = "nconst") %>%
      arrange(primaryName)
    
    selectizeInput(
      "person",
      "Select a Person:",
      choices = people$primaryName,
      options = list(maxOptions = 5000)
    )
  })
  
  # 3. After selecting a person, show titles
  output$person_titles <- renderTable({
    req(input$person)
    
    selected_nconst <- title_principals %>%
      filter(category == input$category) %>%
      distinct(nconst) %>%
      inner_join(name_basics, by = "nconst") %>%
      filter(primaryName == input$person) %>%
      pull(nconst)
    
    title_principals %>%
      filter(nconst == selected_nconst) %>%
      inner_join(title_basics, by = "tconst") %>%
      select(primaryTitle, startYear, titleType) %>%
      arrange(desc(startYear)) %>%
      head(10)
  })
  
}

shinyApp(ui, server)