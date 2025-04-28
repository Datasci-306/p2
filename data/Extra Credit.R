library(shiny)
library(tidyverse)

# Load your datasets
title_basics <- read_rds("title_basics.rda")
title_principals <- read_rds("title_principals.rda")
name_basics <- read_rds("name_basics.rda")

# Kevin Bacon's nconst
kevin_bacon_nconst <- "nm0000102"

ui <- fluidPage(
  
  titlePanel("Six Degrees of Kevin Bacon"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("start_movie", "Enter Part of a Movie/TV Title:", value = ""),
      actionButton("start_game", "Start Game"),
      hr(),
      uiOutput("next_selection"),
      textOutput("game_status")
    ),
    
    mainPanel(
      tableOutput("current_info")
    )
  )
)

server <- function(input, output, session) {
  
  # Initialize game variables
  game_state <- reactiveValues(
    current_tconst = NULL,
    current_nconst = NULL,
    move_count = 0,
    found_kevin = FALSE,
    candidates = NULL,
    message = ""
  )
  
  # Start the game
  observeEvent(input$start_game, {
    req(input$start_movie)
    
    matching_titles <- title_basics %>%
      filter(str_detect(tolower(primaryTitle), tolower(input$start_movie)))
    
    if (nrow(matching_titles) == 0) {
      game_state$message <- "No matching titles found. Try again."
      return(NULL)
    }
    
    selected_tconst <- matching_titles$tconst[1]
    game_state$current_tconst <- selected_tconst
    game_state$current_nconst <- NULL
    game_state$move_count <- 0
    game_state$found_kevin <- FALSE
    game_state$message <- ""
    
    # Load candidates: people in this title
    game_state$candidates <- title_principals %>%
      filter(tconst == selected_tconst) %>%
      inner_join(name_basics, by = "nconst") %>%
      distinct(primaryName, nconst)
  })
  
  # Let user pick a next step
  output$next_selection <- renderUI({
    req(game_state$candidates)
    
    selectInput("next_choice", "Pick a Person:", choices = game_state$candidates$primaryName)
  })
  
  # After picking a person
  observeEvent(input$next_choice, {
    req(input$next_choice)
    
    selected_nconst <- game_state$candidates %>%
      filter(primaryName == input$next_choice) %>%
      pull(nconst)
    
    game_state$current_nconst <- selected_nconst
    game_state$move_count <- game_state$move_count + 1
    
    if (selected_nconst == kevin_bacon_nconst) {
      game_state$found_kevin <- TRUE
      game_state$message <- "🎉 You found Kevin Bacon! You win!"
      return(NULL)
    }
    
    if (game_state$move_count >= 6) {
      game_state$message <- "❌ You reached 6 moves without finding Kevin Bacon. You lose."
      return(NULL)
    }
    
    # Otherwise, load new titles this person is in
    titles_for_person <- title_principals %>%
      filter(nconst == selected_nconst) %>%
      inner_join(title_basics, by = "tconst") %>%
      distinct(primaryTitle, tconst)
    
    # Let user pick a title
    updateSelectInput(session, "next_choice", label = "Pick a Title:", choices = titles_for_person$primaryTitle)
    
    # Now after choosing a title, load people again
    observeEvent(input$next_choice, {
      selected_tconst <- titles_for_person %>%
        filter(primaryTitle == input$next_choice) %>%
        pull(tconst)
      
      game_state$current_tconst <- selected_tconst
      game_state$current_nconst <- NULL
      
      # Load new candidates
      game_state$candidates <- title_principals %>%
        filter(tconst == selected_tconst) %>%
        inner_join(name_basics, by = "nconst") %>%
        distinct(primaryName, nconst)
      
      updateSelectInput(session, "next_choice", label = "Pick a Person:", choices = game_state$candidates$primaryName)
    }, ignoreInit = TRUE, once = TRUE)
  })
  
  # Show status
  output$game_status <- renderText({
    if (game_state$message != "") {
      game_state$message
    } else {
      paste("Move:", game_state$move_count)
    }
  })
  
  # Show current list (for info)
  output$current_info <- renderTable({
    req(game_state$candidates)
    head(game_state$candidates, 10)
  })
}

shinyApp(ui, server)