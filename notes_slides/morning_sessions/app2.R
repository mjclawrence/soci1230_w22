library(shiny)
library(tidyverse)
# using the okCupid profiles data set
# Allow the user to enter a word or phrase
# and display two things:

#1) A random essay response containing that word or phrase
#2) The number of times that word or phrase appears in each essay


profiles <- read_csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/13FuGzlObJQd3GyLHDpIC9n0IIVVGrPWk/SOCI 1230A: DataScience Across Disciplines - Winter 2022/Morning Activities/profiles.csv")

profiles_long <- profiles |> 
  select(essay0:essay9) |> 
  pivot_longer(names_to = "essay",
               values_to = "text",
               essay0:essay9)

ui <- fluidPage(
  
  textInput("wordChoice",
            "What word would you like to search for in the profiles?",
            placeholder = "Type your word here"),
  actionButton("button",
               "Click to count words by essay"),
  dataTableOutput("wordEssay")
)
  
  server <- function(input, output, session) {
  
    output$wordEssay <- renderDataTable({
      
      input$button
      
      random_essay <- profiles_long |> 
        filter(str_detect(text, isolate(input$wordChoice))) |> 
        group_by(essay) |> 
        sample_n(1)
      
      n_words <- profiles_long |> 
        filter(str_detect(text, isolate(input$wordChoice))) |> 
        mutate(word_count = str_count(text, isolate(input$wordChoice))) |> 
        group_by(essay) |> 
        summarise(n = sum(word_count, na.rm = TRUE))
      
      joined_df <- left_join(n_words, random_essay)
      
      joined_df
      
      
    })
    
}

shinyApp(ui, server)