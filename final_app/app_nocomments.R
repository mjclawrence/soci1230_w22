# Load Packages

library(shiny)
library(tidyverse)
library(ggthemes)
library(scales)
library(weights)
library(RColorBrewer)

## vscode test

#shinyWidgetsGallery()

# Discussion of hierarchical select boxes on pp. 156-158 of *Mastering Shiny*

# Load Data

#tfs <- read_csv("https://raw.githubusercontent.com/mjclawrence/soci1230_w22/main/data/tfs_question_summary_withno.csv")
# tfs_labels <- read_csv("https://raw.githubusercontent.com/mjclawrence/soci1230_w22/main/data/tfs_question_summary_labels.csv")
# tfs_df <- left_join(tfs_labels, tfs)
# 
# css <- read_csv("https://raw.githubusercontent.com/mjclawrence/soci1230_w22/main/data/css_question_summary_withno.csv")
# css_labels <- read_csv("https://raw.githubusercontent.com/mjclawrence/soci1230_w22/main/data/css_question_summary_labels.csv")
# css_df <- left_join(css_labels, css)
# 
# tfs_mobility <- read_csv("https://raw.githubusercontent.com/mjclawrence/soci1230_w22/main/data/tfs_means_mobility_withno.csv")
# css_mobility <- read_csv("https://raw.githubusercontent.com/mjclawrence/soci1230_w22/main/data/css_means_mobility_withno.csv")


## Local Paths

#setwd("/Users/lawrence/desktop/shiny_prep/shiny_prep")
#tfs <- read_csv("tfs_question_summary_withno.csv")
tfs_df <- read_csv("tfs_question_summary_labels.csv")
#tfs_df <- left_join(tfs_labels, tfs)

#css <- read_csv("css_question_summary_withno.csv")
css_df <- read_csv("css_question_summary_labels.csv")
#css_df <- left_join(css_labels, css)

tfs_mobility <- read_csv("tfs_means_mobility_withno.csv")
css_mobility <- read_csv("css_means_mobility_withno.csv")

tfs_mobility <- tfs_mobility |> 
  mutate(kq345_cond_parq1 = kq3_cond_parq1 + kq4_cond_parq1 + kq5_cond_parq1,
         k_rank_cond_parq123 = k_rank_cond_parq1*par_q1 + k_rank_cond_parq2*par_q1 + 
           k_rank_cond_parq3*par_q3)

css_mobility <- css_mobility |> 
  mutate(kq345_cond_parq1 = kq3_cond_parq1 + kq4_cond_parq1 + kq5_cond_parq1,
         k_rank_cond_parq123 = k_rank_cond_parq1*par_q1 + 
           k_rank_cond_parq2*par_q1 + k_rank_cond_parq3*par_q3)

chetty_fouryr <- read_csv("chetty_fouryr.csv")

chetty_fouryr <- chetty_fouryr |> 
  mutate(kq345_cond_parq1 = kq3_cond_parq1 + kq4_cond_parq1 + kq5_cond_parq1,
         k_rank_cond_parq123 = k_rank_cond_parq1*par_q1 + k_rank_cond_parq2*par_q1 + 
           k_rank_cond_parq3*par_q3)

chetty_fouryr <- chetty_fouryr |> 
  mutate(kq345_cond_parq1 = kq3_cond_parq1 + kq4_cond_parq1 + kq5_cond_parq1,
         k_rank_cond_parq123 = k_rank_cond_parq1*par_q1 + 
           k_rank_cond_parq2*par_q1 + k_rank_cond_parq3*par_q3)

# Set Up User Interface

ui <- fluidPage(
  sidebarPanel(
  selectInput("df_id", 
              "Choose a survey:",
              choices = c("The Freshman Survey", 
                          "College Senior Survey"),
              selected = NULL
              ),
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
              #choices = names(tfs_mobility |> 
              #select(444:ncol(tfs_mobility))),
              #                 select(mr_kq5_pq1, par_q1, kq5_cond_parq1,
              #                       k_rank, k_rank_cond_parq5)),
              selected = NULL,
              list(`OI Summaries` = list("mr_kq5_pq1", 
                                         "par_q1",
                                         "kq5_cond_parq1"),
                   `K Rank` = list("k_rank", "k_rank_cond_parq5",
                                   "k_rank_cond_parq123",
                                   "kq345_cond_parq1"))
  )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Introduction",
               htmlOutput("intro_box")),
      tabPanel("Question Summary", 
               plotOutput("summary_plot")
               ),
      tabPanel("Mobility Correlations",
               checkboxGroupInput("response_id",
                                  "Choose responses to your selected question (but leave at least one unchecked):",
                                  choices = NULL),
               plotOutput("corr_plot")),
      tabPanel("Summary By Mobility",
               plotOutput("quantiles"))
    )
  )
)

# Set Up Server Side Functions

server <- function(input, output, session) {
  
  output$intro_box <- renderText({
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
  
  summary_data <- reactive({
    switch(input$df_id,
           "The Freshman Survey" = tfs_df,
           "College Senior Survey" = css_df)
  })
  
  # summary_data <- reactive({
  #   if (input$df_id == "The Freshman Survey"){
  #     summary_df <- tfs_df
  #   }
  #   else if (input$df_id == "College Senior Survey"){
  #     summary_df <- css_df
  #   }
  #   return(summary_df)
  # })
  
  # add a delay of 1 sec
  selected_summary <- summary_data |>  debounce(1000)
  
  mobility_data <- reactive({
    switch(input$df_id,
           "The Freshman Survey" = tfs_mobility,
           "College Senior Survey" = css_mobility)
  })
  
  # mobility_data <- reactive({
  #   if (input$df_id == "The Freshman Survey"){
  #     mobility_df <- tfs_mobility 
  #   }
  #   else if (input$df_id == "College Senior Survey"){
  #     mobility_df <- css_mobility
  #   }
  #   return(mobility_df) 
  # })
  
  # add a delay of 1 sec
  selected_mobility <- mobility_data |>  debounce(1000)

  observeEvent(summary_data(), {
    available_groups <- unique(summary_data()$Group)
    updateSelectInput(inputId = "group_id", 
                      choices = available_groups)
  })
  
  selected_group <- reactive({
    filter(summary_data(), Group == input$group_id)
  })
  
  observeEvent(selected_group(), {
    available_questions <- unique(selected_group()$Description)
    updateSelectInput(inputId = "question_id", choices = available_questions)
  })
  
  selected_question <- reactive({
    filter(summary_data(), Description == input$question_id)
  })
  
  output$summary_plot <- renderPlot({
    
    ggplot(data = selected_question(), aes(x = reorder(Label, -wtdprop), 
                                   y = wtdprop*100,
                                   fill = Label)) +
      geom_col() +
      geom_text(aes(label = sprintf("%1.0f%%",round(wtdprop, 2)*100)),
                position = position_dodge(width = .9),
                vjust = -.25) +
      labs(x = "", y = "Proportion", fill = "",
           title = paste(selected_question()$Survey_Name, ": ", selected_question()$Name), # I like the survey and variable names in the title
           subtitle = selected_question()$Description,
           caption = paste("This question comes from the", selected_question()$Survey)) + # And the description in the subtitle
      theme_economist() +
      theme(axis.ticks.x = element_blank(),
            legend.position = "none") +
      scale_fill_brewer(palette = "Set1") +
      scale_x_discrete(labels = label_wrap(10)
                       )
  })
  
  observeEvent(selected_question(), {
    choices <- unique(selected_question()$Label)
    updateCheckboxGroupInput(inputId = "response_id", 
                             choices = choices,
                             selected = choices[1]
    )
  })
  
  corr_data <- reactive({
    selected_question() |> 
      filter(Label %in% input$response_id)
  })
  
  mobility_df2 <- reactive({
    mobility_data() |> 
      select(1:2, 
             type, tier_name,
             paste(corr_data()$Name,".propna", sep = ""),
             input$mobility_variable_id,
             starts_with(corr_data()$Summary)) |>
      rename(propna_nona = 5,
             mobility_variable = 6) |>
      mutate(n_responses_nona = n_responses * (1 - propna_nona)) |>
      relocate(n_responses_nona, .after = propna_nona)
  })
  
  mobility_df3 <- reactive({
      mobility_df2() |> 
      mutate(propsum = rowSums(across(8:ncol(mobility_df2()))),
             weighted_correlation = wtd.cor(mobility_variable,
                                            propsum,
                                            weight = n_responses_nona)[1],
             total_colleges = sum(!is.na(propsum)),
             total_responses = sum(n_responses_nona[!is.na(propsum)]))
  })
  
  # output$table <- renderTable({
  #   return(mobility_df2())
  # })
  
  
  
  
  correlations <- reactive({
    mobility_df3() |>
      group_by(type) |>
      summarise(wtd_correlation = wtd.cor(mobility_variable,
                                          propsum,
                                          weight = n_responses_nona)[1])
  })
  
  output$corr_plot <- renderPlot({
    mobility_df3() |>
      ggplot(aes(x = propsum, y = mobility_variable,
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
  

  quantiles <- reactive({

    chetty_fouryr |> 
      select(input$mobility_variable_id, count) |> 
      rename(mobility_variable = 1) |> 
      summarise(quant25 = wtd.quantile(mobility_variable, 
                                       weights = count,
                                       probs = c(.5)), 
                quant75 = wtd.quantile(mobility_variable, 
                                       weights = count,
                                       probs = c(.75)))
  })
  

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
      rename(mobility_variable = 3,
             propna = 4) |> 
      mutate(n_responses_nona = n_responses * (1-propna),
             qtle = ifelse(mobility_variable < as.numeric(quantiles()[1]), "Bottom 50%",
                           ifelse(mobility_variable > as.numeric(quantiles()[2]), "Top 25%", 
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
  
  
}

# Run The Application 
shinyApp(ui = ui, server = server)
