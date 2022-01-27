# Load Packages

library(shiny)
library(tidyverse)
library(ggthemes)
library(scales)
library(weights)
library(RColorBrewer)
library(plotly)

# Load Data 
## For now the linked files are ok. 
## To publish the app, local files are better.

tfs_df <- read_csv("tfs_question_summary_labels.csv")
css_df <- read_csv("css_question_summary_labels.csv")

tfs_mobility <- read_csv("https://raw.githubusercontent.com/mjclawrence/soci1230_w22/main/data/tfs_means_mobility_withno.csv")
css_mobility <- read_csv("https://raw.githubusercontent.com/mjclawrence/soci1230_w22/main/data/css_means_mobility_withno.csv")

tfs_correlations_labeled <- read_csv("tfs_correlations_join.csv")
css_correlations_labeled <- read_csv("css_correlations_join.csv")

### We can change the datasets before moving on.
### We'll construct a couple new mobility variables to include.
### Keep edits simple here if you are doing them at all.

tfs_mobility <- tfs_mobility |> 
  mutate(kq345_cond_parq1 = kq3_cond_parq1 + kq4_cond_parq1 + kq5_cond_parq1,
         k_rank_cond_parq123 = k_rank_cond_parq1*par_q1 + k_rank_cond_parq2*par_q1 + 
           k_rank_cond_parq3*par_q3)

css_mobility <- css_mobility |> 
  mutate(kq345_cond_parq1 = kq3_cond_parq1 + 
           kq4_cond_parq1 + 
           kq5_cond_parq1,
         k_rank_cond_parq123 = k_rank_cond_parq1*par_q1 + 
           k_rank_cond_parq2*par_q1 + 
           k_rank_cond_parq3*par_q3)

chetty_fouryr <- read_csv("https://raw.githubusercontent.com/mjclawrence/soci1230_w22/main/data/chetty_fouryr.csv")

chetty_fouryr <- chetty_fouryr |> 
  mutate(kq345_cond_parq1 = kq3_cond_parq1 + 
           kq4_cond_parq1 + 
           kq5_cond_parq1,
         k_rank_cond_parq123 = k_rank_cond_parq1*par_q1 + 
           k_rank_cond_parq2*par_q1 + 
           k_rank_cond_parq3*par_q3)

# Set Up User Interface (UI)

if (interactive()) { # need this for shinyfullscreen 
ui <- fluidPage( # Big wrap for this page layout to fill browser window
    titlePanel("SOCI 1230 Working Example", # title for browser tab
               title = "Scratchpad: College Experiences and Income Mobility"), # Title in window
  sidebarPanel( # To set up the sidebar
  selectInput("df_id", # We'll use this variable name as an input later
              "Choose a survey:", # What the user sees
              choices = c("The Freshman Survey", # Choices in list
                          "College Senior Survey"),
              selected = NULL # Default
              ), # Each input is separated by a comma
  selectInput("group_id",
              "Choose a question group:",
              choices = NULL,
              selected = NULL),
  selectInput("question_id",
              "Choose a question:",
              choices = NULL,
              selected = NULL
              ),
  selectInput("mobility_variable_id",
              "Choose a mobility variable",
              selected = NULL,
              # In the next few lines we group choices with muted headings
              # Note we can also change how the variable names appear
              list(`OI Summaries` = list("Access Rate (Proportion of students from bottom 20%)" = "par_q1",
                                         "Success Rate (Proportion of students from bottom 20% who end up in top 20%)" = "kq5_cond_parq1",
                                         "Mobility Rate (Proportion of all students who come from bottom 20% and end up in top 20%)" = "mr_kq5_pq1"),
                   `K Rank` = list("k_rank", "k_rank_cond_parq5",
                                   "k_rank_cond_parq123",
                                   "kq345_cond_parq1")))
  ), # Close the sidebarPanel before moving on to the main panel...
  mainPanel( # Set up for mainPanel
    tabsetPanel( # We'll use tabs here to collect output
      tabPanel("Introduction", # Tab title
               htmlOutput("intro_box")), # What output from the server side should populate
      # Most text output just uses textOutput or verbatimTextOutput but htmlOutput allows formatting
      tabPanel("Question Summary", 
               plotOutput("summary_plot")), # Click for full screen of plotoutput
      tabPanel("Mobility Correlations",
               checkboxGroupInput("response_id", # Checkboxes allowing multiple selections
                                  "Choose responses to your selected question (but leave at least one unchecked):",
                                  choices = NULL),
               plotOutput("corr_plot")),
      tabPanel("Summary By Mobility",
               plotOutput("quantiles")),
      tabPanel("Correlations Summary Plot",
               plotlyOutput("corr_summary_plot"))
    ) # Close the tabsetPanel
  ) # Close the mainPanel
) # Close the UI

# Set Up Server Side Functions

server <- function(input, output, session) {
  
  # Dummy df to test quick facts in introduction tab
  
  output$intro_box <- renderText({ # Creates the object that will output in the UI
    names_column <- c("A", "B", "C", "D", "E")
    facts_column <- c("I learned this...",
                      "I discovered this...",
                      "Did you know...",
                      "Have you ever wondered...",
                      "For more information...")
    
    quick_facts <- as.data.frame(cbind(names_column,
                                       facts_column))
    
    quick_facts <- quick_facts |> 
      mutate(rowid = row_number()) |> 
      filter(rowid == sample(1:nrow(quick_facts), 1))
    
    paste(
      "<br> Here's a fast fact from student ",
          as.character(quick_facts$names_column), 
          ": <br><br>", 
          "<b>", 
          as.character(quick_facts$facts_column), 
          "</b> <br><br>",
          "<em>For more fast facts reload this page!</em>"
      )
  })
  
  # Switch the summary data 
  summary_data <- reactive({ # Creates a function as an object we will use later. This one is reactive to the choice of df_id in the UI 
    switch(input$df_id,
           "The Freshman Survey" = tfs_df,
           "College Senior Survey" = css_df)
  })

  # adding a delay of 1 sec can be helpful if there are lags when switching
  selected_summary <- summary_data |>  debounce(1000)
  
  # Switch the mobility data
  mobility_data <- reactive({ # Also need to switch the mobility data based on df_id
    switch(input$df_id,
           "The Freshman Survey" = tfs_mobility,
           "College Senior Survey" = css_mobility)
  })
  
  # add a delay of 1 sec
  selected_mobility <- mobility_data |>  debounce(1000)
  
  # Switch the correlations data
  
  selected_correlations_all <- reactive({
    switch(input$df_id,
           "The Freshman Survey" = tfs_correlations_labeled,
           "College Senior Survey" = css_correlations_labeled)
  })
  
  # add a delay of 1 sec
  selected_correlations <- selected_correlations_all |>  debounce(1000)
  
  
  # Update question group based on which survey is selected
  observeEvent(summary_data(), { # "Watch what happens in the summary_data() function we set up earlier. Use those changes here too."
    available_groups <- unique(summary_data()$Group_rev) # Notice the dataframe is a function so needs ()
    updateSelectInput(inputId = "group_id", #The choices for inputId are based on the dataset we observe being selected above.
                      choices = available_groups)
  })
  
  selected_group <- reactive({
    filter(summary_data(), Group_rev == input$group_id)
  })
  
  # Update questions based on which question group is selected
  observeEvent(selected_group(), {
    available_questions <- unique(selected_group()$Description)
    updateSelectInput(inputId = "question_id", choices = available_questions)
  })
  
  selected_question <- reactive({
    filter(summary_data(), Description == input$question_id)
  })
  
  ## Make a summary plot of the selected question
  
  output$summary_plot <- renderPlot({ # Saving the plot as output that will feed into the Question Summary tab in UI
    
    ggplot(data = selected_question(), aes(x = reorder(Label, -wtdprop), 
                                   y = wtdprop*100,
                                   fill = Label)) +
      geom_col() +
      geom_text(aes(label = sprintf("%1.0f%%",round(wtdprop, 2)*100)),
                position = position_dodge(width = .9),
                vjust = -.25) +
      labs(x = "", y = "Proportion", fill = "",
           title = paste(selected_question()$Survey_Name, ": ", selected_question()$Name),
           subtitle = selected_question()$Description,
           caption = paste("This question comes from the", selected_question()$Survey)) +
      theme_economist() +
      theme(axis.ticks.x = element_blank(),
            legend.position = "none") +
      scale_fill_brewer(palette = "Set1") +
      scale_x_discrete(labels = label_wrap(10)
                       )
  })
  
  # In correlations tab change the response levels based on which question is selected
  observeEvent(selected_question(), {
    choices <- unique(selected_question()$Label)
    updateCheckboxGroupInput(inputId = "response_id", 
                             choices = choices,
                             selected = choices[1]
    )
  })
  
  # Filter the survey dataset to only include the selected response level(s)
  corr_data <- reactive({
    selected_question() |> 
      filter(Label %in% input$response_id)
  })
  
  # Set up mobility data
  mobility_df2 <- reactive({
    mobility_data() |> 
      select(1:2, 
             type, tier_name,
             paste(corr_data()$Name,".propna", sep = ""),
             input$mobility_variable_id,
             starts_with(corr_data()$Summary)) |> # Pulls selected response levels
      rename(propna_nona = 5) |>  # Selecting columns in order allows you to rename by column number
           #  ) |> # Rename input variables when possible to use consistent names
      mutate(n_responses_nona = n_responses * (1 - propna_nona)) |>
      relocate(n_responses_nona, .after = propna_nona)
  })
  
  mobility_df3 <- reactive({
      mobility_df2() |> 
      mutate(propsum = rowSums(across(8:ncol(mobility_df2()))), # Add props across selected response levels
             weighted_correlation = wtd.cor(.data[[input$mobility_variable_id]],
                                            propsum,
                                            weight = n_responses_nona)[1],
             total_colleges = sum(!is.na(propsum)),
             total_responses = sum(n_responses_nona[!is.na(propsum)]))
  })
  
  # When building an app, it can be helpful to include tables as you go 
  # to make sure your dfs are setting up correctly. But hashtag them out
  # for the published version
  
  #output$corr_table <- renderTable(mobility_df2())

  # Summarise correlations by institution type
  correlations <- reactive({
    mobility_df3() |>
      group_by(type) |>
      summarise(wtd_correlation = wtd.cor(.data[[input$mobility_variable_id]],
                                          propsum,
                                          weight = n_responses_nona)[1])
  })

  
  # Create scatterplot with piped title, subtitle, and caption
  output$corr_plot <- renderPlot({
    mobility_df3() |>
      ggplot(aes(x = propsum, y = .data[[input$mobility_variable_id]],
                 size = n_responses_nona, color = type)) +
      geom_point() +
      labs(title = paste("The overall correlation is:",
                         round(mean(mobility_df3()$weighted_correlation),3)),
           subtitle = paste("The correlation at public institutions is:",
                            round(mean(correlations()$wtd_correlation[2]),3),
                            ". \nThe correlation at private institutions is:",
                            round(mean(correlations()$wtd_correlation[1]),3)),
           caption = paste("Data are available for this survey question from",
                           mobility_df3()$total_colleges,
                           "colleges representing",
                           mobility_df3()$total_responses,
                           "students")) +
      scale_color_brewer(palette = "Dark2")
  })
  
# Set up quantiles for selected mobility variable
  quantiles <- reactive({

    chetty_fouryr |> 
      select(input$mobility_variable_id, count) |> 
      #rename(mobility_variable = 1) |> 
      summarise(quant1 = wtd.quantile(.data[[input$mobility_variable_id]], 
                                       weights = count,
                                       probs = c(.5)), 
                quant2 = wtd.quantile(.data[[input$mobility_variable_id]], 
                                       weights = count,
                                       probs = c(.75)))
  })
  
  
  # Pull variables levels, labels, and description
  # Set up data
  # Make the plot
  
  ## This is probably too long for one function
  mobility_df_test2 <- reactive({
    
    variable_levels_df <- selected_question() |>
      mutate(response_level = str_extract(Summary, "[^.]*$"))
    
    variable_levels <- variable_levels_df$response_level
    
    variable_labels <- selected_question()$Label
    
    variable_description <- selected_question()$Description[1]
    
    mobility_data() |> 
      select(campus_id, n_responses,
             input$mobility_variable_id,
             paste(selected_question()$Name,".propna", sep = ""),
             starts_with(selected_question()$Summary)) |> 
      rename(#mobility_variable = 3,
             propna = 4) |> 
      mutate(n_responses_nona = n_responses * (1-propna),
             qtle = ifelse(.data[[input$mobility_variable_id]] < as.numeric(quantiles()[1]), "Bottom 50%",
                           ifelse(.data[[input$mobility_variable_id]] > as.numeric(quantiles()[2]), "Top 25%", 
                                  "Third Quartile"))) |> 
      pivot_longer(names_to = "response_level",
                   values_to = "proportion",
                   starts_with(selected_question()$Summary)) |> 
      mutate(response_level = str_remove(response_level, "_mean"),
             response_level = str_extract(response_level, "[^.]*$")) |> 
      mutate(response_level = factor(response_level,
                                     levels = variable_levels,
                                     labels = variable_labels)) |> 
      group_by(qtle, response_level) |> 
      summarise(wtd.mean = wtd.mean(proportion, 
                                    weights = n_responses_nona, 
                                    na.rm = TRUE)) |> 
      ggplot(aes(x = response_level, y = wtd.mean, fill = response_level)) +
      geom_col() +
      labs(title = variable_description,
           fill = "") +
      theme_tufte() +
      theme(legend.position = "none") + facet_grid(qtle~.) +
      scale_x_discrete(labels = label_wrap(10)) +
      geom_text(aes(label = round(wtd.mean, 3)),
                position = position_dodge(width = .9),
                vjust = 0)
  })

  output$quantiles <- renderPlot({
    return(mobility_df_test2())
  })
  
  
  output$corr_summary_plot <- renderPlotly({
    
    positive <- selected_correlations_all() |> 
      filter(corr_type == "Pooled", 
             !str_detect(Summary, "cant")) |> 
      select(input$mobility_variable_id, Group, Group_rev, 
             Description, Summary, Label) |> 
      group_by(Group_rev) |>
      slice_max(.data[[input$mobility_variable_id]], n = 1) |>
      mutate(corr_sign = "Positive") |> 
      arrange(-.data[[input$mobility_variable_id]])
    
    negative <- selected_correlations_all() |> 
      filter(corr_type == "Pooled", 
             !str_detect(Summary, "cant")) |> 
      select(input$mobility_variable_id, Group, Group_rev, 
             Description, Summary, Label) |> 
      group_by(Group_rev) |>
      slice_min(.data[[input$mobility_variable_id]], n = 1) |>
      mutate(corr_sign = "Negative") |> 
      arrange(-.data[[input$mobility_variable_id]])
    
    correlations_sign <- bind_rows(positive, negative)
    
    group_levels <- correlations_sign |> 
      filter(corr_sign=="Positive") |> 
      arrange(.data[[input$mobility_variable_id]]) |> 
      pull(Group_rev) 
    
    font <- list(
      family = "Arial",
      size = 14,
      color = "white"
    )
    label <- list(
      #bgcolor = size,
      bordercolor = "transparent",
      font = font
    )
    
    correlations_summary_plot <- correlations_sign |> 
      mutate(Group_rev = factor(Group_rev,
                                levels = group_levels)) |> 
      ggplot(aes(x = Group_rev, y = .data[[input$mobility_variable_id]], 
                 color = corr_sign,
                 text = paste("Group: ", Group_rev, "\n", # text for custom tooltip
                              "Question: ", Description,"\n",
                              "Response: ", Label, "\n",
                              "Correlation: ", round(.data[[input$mobility_variable_id]],3)))) +
      geom_point(size = 3) + 
      coord_flip() + theme(legend.position = "bottom") +
      scale_color_manual(values = c("red", "forest green"))
    
    ggplotly(correlations_summary_plot, tooltip = "text") |> 
      layout(legend = list(orientation = "h", x = .25, y = -0.2)) |> 
      style(hoverlabel = label) |> 
      layout(font = font)
    
  })
  
  
} # Close the server side functions

# Run The Application 
shinyApp(ui = ui, server = server, 
         options = list(launch.browser = TRUE)) # needed for fullscreen
} # closes if interactive