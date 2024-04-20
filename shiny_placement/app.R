#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library("shiny")
source(here::here("scripts", "00_libs.R"))

mod_lt_placement <- readRDS("../models/mod_lt_placement.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Placement stuff"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput(
              "score_lt",
              "LexTALE score:",
              min = 1,
              max = 100,
              value = 50
            ), 
            sliderInput(
              "score_exam", 
              "Placement exam score:", 
              min = 1, 
              max = 46, 
              value = 25
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot <- renderPlot({

        post <- predictions(
          model = mod_lt_placement, 
          newdata = datagrid(
            score_lt = input$score_lt, 
            score_exam = input$score_exam
          ), 
          type = "response"
        ) |> 
          posterior_draws() |> 
          mutate(
            group = case_when(
              group == 1 ~ "1", 
              group == 2 ~ "1.5", 
              group == 3 ~ "2", 
              group == 4 ~ "3", 
              group == 5 ~ "4", 
              group == 6 ~ "Master"
            )
          ) 
        
        post |> 
          ggplot() + 
          aes(x = group, y = draw) + 
          stat_pointinterval(pch = 21, point_fill = "white") + 
          geom_text(
            data = tibble(
              group = "1", 
              draw = 1, 
              label = glue::glue("LT = {input$score_lt}\nPlacement = {input$score_exam}")
            ), 
            mapping = aes(label = label), 
            hjust = 0, vjust = 1, family = "Palatino", size = 6
          ) + 
          labs(x = NULL, y = "p(level)") + 
          ds4ling::ds4ling_bw_theme(base_size = 24, base_family = "Palatino")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
