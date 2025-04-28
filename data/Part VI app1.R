library(shiny)
library(tidyverse)

# Load IMDb data
title_basics <- read_rds("title_basics.rda")
title_ratings <- read_rds("title_ratings.rda")

# Join tables
imdb_data <- title_basics %>%
  select(tconst, primaryTitle, runtimeMinutes, startYear) %>%
  mutate(runtimeMinutes = as.numeric(runtimeMinutes)) %>%
  inner_join(title_ratings, by = "tconst") %>%
  filter(!is.na(runtimeMinutes), !is.na(averageRating))

ui <- fluidPage(
  
  titlePanel("IMDb Rating vs. Runtime Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("title_search", "Search Title (Key Words):", value = ""),
      
      sliderInput("rating_slider", "Rating Range:",
                  min = 0, max = 10, value = c(0, 10), step = 0.1),
      
      checkboxGroupInput("rating_checkbox", "Quick Minimum Rating:",
                         choices = c("9+" = 9, "8+" = 8, "7+" = 7, "6+" = 6),
                         selected = NULL),
      
      checkboxInput("filter_runtime", "Only show movies under 300 min?", value = TRUE),
      
      textOutput("warning_text")  # Show warning if both selected
    ),
    
    mainPanel(
      plotOutput("scatter_plot", click = "plot_click"),
      verbatimTextOutput("info"),
      tableOutput("click_info")
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    data <- imdb_data
    
    if (input$filter_runtime) {
      data <- data %>% filter(runtimeMinutes <= 300)
    }
    
    if (input$title_search != "") {
      data <- data %>% filter(str_detect(tolower(primaryTitle), tolower(input$title_search)))
    }
    
    # Determine whether to use checkbox or slider
    if (!is.null(input$rating_checkbox) && length(input$rating_checkbox) > 0) {
      # If user selects a checkbox
      min_rating <- min(as.numeric(input$rating_checkbox))
      data <- data %>% filter(averageRating >= min_rating)
    } else {
      # If no checkbox selected, use slider
      data <- data %>%
        filter(
          averageRating >= input$rating_slider[1],
          averageRating <= input$rating_slider[2]
        )
    }
    data
  })
  
  # Warning text: if both are used
  output$warning_text <- renderText({
    if (!is.null(input$rating_checkbox) && length(input$rating_checkbox) > 0 &&
        (input$rating_slider[1] > 0 || input$rating_slider[2] < 10)) {
      "⚠️ Warning: You selected both a quick rating and a slider range! Only the quick rating will apply."
    } else {
      ""
    }
  })
  
  output$scatter_plot <- renderPlot({
    filtered_data() %>%
      ggplot(aes(x = averageRating, y = runtimeMinutes)) +
      geom_point(alpha = 0.6, color = "dodgerblue") +
      labs(
        x = "IMDb Average Rating",
        y = "Runtime (minutes)",
        title = "Scatterplot of Runtime vs. Rating"
      ) +
      theme_minimal()
  })
  
  output$info <- renderPrint({
    req(input$plot_click)
    x <- round(input$plot_click$x, 2)
    y <- round(input$plot_click$y, 2)
    cat("[", x, ", ", y, "]", sep = "")
  })
  
  output$click_info <- renderTable({
    req(input$plot_click)
    nearPoints(
      filtered_data(),
      input$plot_click,
      xvar = "averageRating",
      yvar = "runtimeMinutes",
      threshold = 10,
      maxpoints = 1
    ) %>%
      select(primaryTitle, averageRating, runtimeMinutes, startYear)
  })
}

shinyApp(ui, server = server)
