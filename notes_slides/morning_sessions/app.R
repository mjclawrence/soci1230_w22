library(shiny)
library(tidyverse)

# From diamonds, show carat vs price plot

ui <- fluidPage(
  # display the plot
  selectInput("colorChoice",
              "Choose a color!",
              choices = c("red", "green", "blue")),
  textInput("titleChoice",
            "Title",
            placeholder = "Enter a title for your plot"),
  actionButton("button",
               "Click to make graph"),
  plotOutput("graph1"),
)

server <- function(input, output, session) {
  # make the plot
  
  output$graph1 <- renderPlot({
    
    input$button
    
    diamonds |> 
    ggplot() + geom_point(aes(x = carat, 
                              y = price),
                              color = isolate(input$colorChoice)) +
      labs(title = isolate(input$titleChoice))
  })
  
}

shinyApp(ui, server)