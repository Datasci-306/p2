library(shiny)
library(tidyverse)

# Load IMDb data
title_basics <- read_rds("title_basics.rda")
title_principals <- read_rds("title_principals.rda")
name_basics <- read_rds("name_basics.rda")

kevin_bacon_nconst <- "nm0000102"

ui <- fluidPage(
  titlePanel("Six Degrees of Kevin Bacon"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("movie_search", "Enter Part of a Movie Title:", value = ""),
      actionButton("start_game", "Start Game"),
      hr(),
      uiOutput("choice_ui"),
      actionButton("make_move", "Confirm Move"),
      hr(),
      textOutput("status_message")
    ),
    
    mainPanel(
      tableOutput("history_table")
    )
  )
)

server <- function(input, output, session) {
  
  # Setup game state
  game <- reactiveValues(
    move_count = 0,
    mode = "choose_person",  # or "choose_title"
    candidates = NULL,
    selected = NULL,
    history = tibble(move = integer(), type = character(), name = character()),
    found = FALSE
  )
  
  # Start the game
  observeEvent(input$start_game, {
    req(input$movie_search)
    
    movie_found <- title_basics %>%
      filter(str_detect(tolower(primaryTitle), tolower(input$movie_search)))
    
    if (nrow(movie_found) == 0) {
      game$candidates <- NULL
      game$history <- tibble()
      game$move_count <- 0
      game$found <- FALSE
      return()
    }
    
    selected_tconst <- movie_found$tconst[1]
    
    # Find people in that movie
    people <- title_principals %>%
      filter(tconst == selected_tconst) %>%
      inner_join(name_basics, by = "nconst") %>%
      select(primaryName, nconst) %>%
      distinct()
    
    game$candidates <- people
    game$mode <- "choose_person"
    game$move_count <- 0
    game$found <- FALSE
    game$history <- tibble(move = 0, type = "Movie", name = movie_found$primaryTitle[1])
    game$selected <- NULL
  })
  
  # UI for user choice
  output$choice_ui <- renderUI({
    req(game$candidates)
    
    choices <- game$candidates$primaryName
    selectInput("selected_choice", "Select:", choices = choices)
  })
  
  # When user clicks "Confirm Move"
  observeEvent(input$make_move, {
    req(game$candidates)
    req(input$selected_choice)
    
    selected_row <- game$candidates %>%
      filter(primaryName == input$selected_choice)
    
    if (nrow(selected_row) == 0) return()
    
    game$move_count <- game$move_count + 1
    
    if (game$mode == "choose_person") {
      selected_nconst <- selected_row$nconst[1]
      
      # Check if it's Kevin Bacon
      if (selected_nconst == kevin_bacon_nconst) {
        game$found <- TRUE
        game$history <- bind_rows(game$history, tibble(move = game$move_count, type = "Person", name = "Kevin Bacon"))
        return()
      }
      
      # Find titles for this person
      titles <- title_principals %>%
        filter(nconst == selected_nconst) %>%
        inner_join(title_basics, by = "tconst") %>%
        select(primaryTitle, tconst) %>%
        distinct()
      
      game$candidates <- titles %>%
        rename(primaryName = primaryTitle, nconst = tconst)
      game$mode <- "choose_title"
      game$history <- bind_rows(game$history, tibble(move = game$move_count, type = "Person", name = selected_row$primaryName[1]))
      
    } else if (game$mode == "choose_title") {
      selected_tconst <- selected_row$nconst[1]
      
      # Find people in this title
      people <- title_principals %>%
        filter(tconst == selected_tconst) %>%
        inner_join(name_basics, by = "nconst") %>%
        select(primaryName, nconst) %>%
        distinct()
      
      game$candidates <- people
      game$mode <- "choose_person"
      game$history <- bind_rows(game$history, tibble(move = game$move_count, type = "Title", name = selected_row$primaryName[1]))
    }
    
    if (game$move_count >= 6 && !game$found) {
      game$mode <- "done"
    }
  })
  
  # Status message
  output$status_message <- renderText({
    if (game$found) {
      return("🎉 You found Kevin Bacon! You win!")
    } else if (game$move_count >= 6) {
      return("❌ You used 6 moves without finding Kevin Bacon. You lose.")
    } else {
      paste("Moves used:", game$move_count)
    }
  })
  
  # History table
  output$history_table <- renderTable({
    game$history
  })
}

shinyApp(ui, server)