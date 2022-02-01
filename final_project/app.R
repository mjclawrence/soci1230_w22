# Load Packages

#install.packages("shiny") # This is new; hashtag after installing.
library(shiny)
library(tidyverse)
library(ggthemes)
library(scales)
library(weights)
library(RColorBrewer)
library(plotly)
library(shinythemes)
library(DT) 
library(shinyfullscreen) 
library(googlesheets4) # Need for fast facts


# Load Data (use local files)

tfs_df <- read_csv("tfs_question_summary_labels.csv") 

css_df <- read_csv("css_question_summary_labels.csv")

tfs_mobility <- read_csv("tfs_means_mobility_withno.csv")
css_mobility <- read_csv("css_means_mobility_withno.csv")

tfs_correlations_labeled <- read_csv("tfs_correlations_join.csv") 
css_correlations_labeled <- read_csv("css_correlations_join.csv") 

css_correlations_labeled <- css_correlations_labeled |> 
  filter(!str_detect(Group_rev, "Scales|Family"))

chetty_fouryr <- read_csv("chetty_fouryr.csv")

## Add constructed mobility measures to chetty_fouryr
chetty_fouryr <- chetty_fouryr |> 
  mutate(kq345_cond_parq1 = kq3_cond_parq1 + 
           kq4_cond_parq1 + 
           kq5_cond_parq1,
         k_rank_cond_parq123 = k_rank_cond_parq1*par_q1 + 
           k_rank_cond_parq2*par_q1 + 
           k_rank_cond_parq3*par_q3)

## The RACE_ETHNICITY variable in tfs_df is not in tfs_mobility or tfs_correlations_labeled. 
## Fix it.

### Editing race in tfs_df

race_variables <- tfs_df |> 
  filter(str_detect(Name, regex("race", ignore_case = TRUE)), # to ignore case
         str_detect(Summary, "yes"))

race_variables <- race_variables |> 
  mutate(Name = "RACE_ETHNICITY",
         Description = "Racial and ethnic composition") |> 
  separate(Summary, c("race", "level"), sep = "[.]") |> 
  mutate(race = "RACE_ETHNICITY") |> 
  unite("Summary", race:level, sep = ".")

tfs_df <- tfs_df |> 
  filter(!str_detect(Name, "RACE"))

tfs_df <- bind_rows(tfs_df, race_variables)

tfs_df <- tfs_df |> 
  mutate(Description = str_trim(Description, side = c("left")))

### Editing race in tfs_mobility

tfs_mobility <- tfs_mobility |> 
  pivot_longer(starts_with("RACE") & !contains("propna")) |> 
  filter(!str_detect(name, "_no_")) |> 
  separate(name, c("race", "level"), sep = "[.]") |> 
  mutate(race = "RACE_ETHNICITY") |> 
  unite("race", race:level, sep = ".") |> 
  pivot_wider(names_from = race, 
              values_from = value) |> 
  pivot_longer(starts_with("RACE") & contains("propna")) |> 
  separate(name, c("race", "propna"), sep = "[.]") |> 
  mutate(race = "RACE_ETHNICITY") |> 
  unite("race", race:propna, sep = ".") |> 
  distinct() |> 
  pivot_wider(names_from = race,
              values_from = value) |> 
  relocate(starts_with("RACE_ETHNICITY"), .before = type)

race_variables <- tfs_correlations_labeled |> 
  filter(str_detect(Name, regex("race", ignore_case = TRUE)), # to ignore case
         str_detect(Summary, "yes"))

race_variables <- race_variables |> 
  mutate(Name = "RACE_ETHNICITY",
         Description = "Racial and ethnic composition") |> 
  separate(Summary, c("race", "level"), sep = "[.]") |> 
  mutate(race = "RACE_ETHNICITY") |> 
  unite("Summary", race:level, sep = ".")


### Editing race in tfs_correlations_labeled
race_variables <- tfs_correlations_labeled |> 
  filter(str_detect(Name, regex("race", ignore_case = TRUE)), # to ignore case
         str_detect(Summary, "yes"))

race_variables <- race_variables |> 
  mutate(Name = "RACE_ETHNICITY",
         Description = "Racial and ethnic composition") |> 
  separate(Summary, c("race", "level"), sep = "[.]") |> 
  mutate(race = "RACE_ETHNICITY") |> 
  unite("Summary", race:level, sep = ".")

tfs_correlations_labeled <- tfs_correlations_labeled |> 
  filter(!str_detect(Name, "RACE"))

tfs_correlations_labeled <- bind_rows(tfs_correlations_labeled, race_variables)

tfs_correlations_labeled <- tfs_correlations_labeled |> 
  mutate(Description = str_trim(Description, side = c("left")))

### The Puerto Rican numbers do not look right. Drop them from all the dfs for now.
tfs_df <- tfs_df |> 
  filter(!str_detect(Summary, regex("Puerto", ignore_case = TRUE)))

tfs_correlations_labeled <- tfs_correlations_labeled |> 
  filter(!str_detect(Summary, regex("Puerto", ignore_case = TRUE)))

tfs_mobility <- tfs_mobility |> 
  select(-contains(regex("Puerto", ignore_case = TRUE)))
  

## Build data frame for searchable data table

all_questions <- bind_rows(tfs_df, css_df) |>
  select(Survey_Name, Group_rev, Description) |> 
  rename(Survey = Survey_Name,
         `Question Group` = Group_rev,
         Question = Description) |> 
  distinct()


## Fast Facts

gs4_deauth()
fastfacts_gs <- "https://docs.google.com/spreadsheets/d/1KstDLAg_B3SdZpJDsABlr_UxunAjQ_UBUQ3EG6akQy4/edit#gid=0"
fast_facts <- read_sheet(fastfacts_gs)

fast_facts <- fast_facts |> 
  filter(Name != "Example") |> 
  mutate(rowid = row_number())


## Need this for popup window to close in Chrome
modalActionButton <- function(inputId, label, icon = NULL, width = NULL, ...) {
  
  value <- restoreInput(id = inputId, default = NULL)
  tags$button(id = inputId, type = "button", style = if (!is.null(width)) 
    paste0("width: ", validateCssUnit(width), ";"), type = "button", 
    class = "btn btn-default action-button", `data-dismiss` = "modal", `data-val` = value, 
    list(shiny:::validateIcon(icon), label), ...)
  
}


# Set Up User Interface (UI)

#if (interactive()) { # need this for shinyfullscreen 
ui <- fluidPage(#shinythemes::themeSelector(), ### New to test themes
  theme = shinytheme("united"), ### New to use chosen theme
  titlePanel("SOCI 1230 App", # title for browser tab
             title = "How Does College Matter For Income Mobility?"), # Title in window
  tags$head(tags$style(  ### New for sidebar background color
    HTML('
         #sidebar {
            background-color: #fff;
        }

        body, label, input, button, select { 
          font-family: "Arial";
        }')
  )), # End for sidebar background color
    sidebarPanel(# To set up the sidebar
      id="sidebar", # To id sidebar color code above
      conditionalPanel(  ### NEW
      condition="input.tabselected>=2", ### NEW
      selectInput("mobility_variable_id",
                  "Choose a mobility variable",
                  selected = NULL,
                  # The next lines we group choices with muted headings
                  # Note we can also change how the variable names appear
                  list(
                    
                    `Ranks in Young Adult Income Distribution (when young adult is 32)` = list(
                      "Average rank" = "k_rank",
                      "Average rank if grew up in bottom 60%" = "k_rank_cond_parq123",
                      "Average rank if grew up in bottom quintile" = "k_rank_cond_parq1",
                      "Average rank if grew up in second lowest quintile" = "k_rank_cond_parq2",
                      "Average rank if grew up in middle quintile" = "k_rank_cond_parq3",
                      "Average rank if grew up in second highest quintile" = "k_rank_cond_parq4",
                      "Average rank if grew up in highest quintile" = "k_rank_cond_parq5"
                    ),
                    
                    `Measures Of Income Mobility` = list(
                      "'Great Working Class Colleges' success rate (bottom quintile of parent distribution and highest 60% of young adult distribution)" = "kq345_cond_parq1",
                      "Opportunity Insights success rate (bottom quintile of parent distribution and top quintile of young adult distribution)" = "kq5_cond_parq1",
                      "Opportunity Insights mobility rate (Proportion of all students at an institution moving from bottom quintile of parent distribution to highest quintile of young adult distribution)" = "mr_kq5_pq1",
                      "Opportunity Insights upper tail mobility rate (bottom quintile of parent distribution and top 1% of young adult distribution)" = "mr_ktop1_pq1",
                      "Stickiness at the bottom (bottom quintile of parent distribution and bottom quintile of young adult distribution)" = "kq1_cond_parq1",
                      "Stickiness at the top (top quintile of parent distribution and top quintile of young adult distribution)" = "kq5_cond_parq5",
                      "Downward mobility from the top (top quintile of parent distribution and bottom quintile of young adult distribution)" = "kq1_cond_parq5"
                    ),
                    
                    `Young Adult Income Distribution (when young adult is 32)` = list(
                      "Proportion ending up in bottom quintile" = "k_q1",
                      "Proportion ending up in second lowest quintile" = "k_q2",
                      "Proportion ending up in middle quintile" = "k_q3",
                      "Proportion ending up in second highest quintile" = "k_q4",
                      "Proportion ending up in highest quintile" = "k_q5"
                    ),
                    
                    `Parent Income Distribution (when young adult is 16)` = list(
                      "Proportion from bottom quintile (Opportunity Insights access rate)" = "par_q1",
                      "Proportion from second lowest quintile" = "par_q2",
                      "Proportion from middle quintile" = "par_q3",
                      "Proportion from second highest quintile" = "par_q4",
                      "Proportion from highest quintile" = "par_q5"
                    ))
                  )),
      conditionalPanel(
      condition="input.tabselected>=2",
      selectInput("df_id", # We'll use this variable name as an input later
                  "Choose a survey:", # What the user sees
                  choices = c("The Freshman Survey", # Choices in list
                              "College Senior Survey"),
                  #selected = NULL # Default
      )), # Each input is separated by a comma
      conditionalPanel(
      condition="input.tabselected>=3",
      selectInput("group_id",
                  "Choose a question group:",
                  choices = NULL)),
                  #selected = NULL),
      conditionalPanel(
        condition="input.tabselected>=3",
      selectInput("question_id",
                  "Choose a question:",
                  choices = NULL))
                  #selected = NULL,
    ), # Close the sidebarPanel before moving on to the main panel...
  mainPanel( # Set up for mainPanel
    tabsetPanel( # We'll use tabs here to collect output
      
      tags$head(tags$style( # This centers the equalizer image in its tab
        type="text/css",
        "#equalizer img {
            width: 65%;
            display: block;
            margin-left: auto;
            margin-right: auto;
        }")),
        
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        
        ),
        
      tabPanel("Introduction", value = 1, # Tab title, 
               htmlOutput("text"),
               imageOutput("equalizer"),
               htmlOutput("intro_box")),
        tabPanel("Correlations Summary Plot",value = 2,  
                 fullscreen_this(plotlyOutput("corr_summary_plot")),
                 textOutput("text1")),
        tabPanel("Summary By Mobility", value = 3, 
                 fullscreen_this(plotOutput("quantiles"))),
        tabPanel("Mobility Correlations", value = 4, 
                 checkboxGroupInput("response_id", # Checkboxes allowing multiple selections
                                    "Choose responses to your selected question (but leave at least one unchecked):",
                                    choices = NULL), # Is this working in Chrome?
                  fullscreen_this(plotOutput("corr_plot"))),
        tabPanel("Search All Questions", value = 5,
                 dataTableOutput("question_df")),
        #tabPanel("Question Summary", value = 5, ### NEW
         #        plotOutput("summary_plot")), # Click for full screen of plotoutput
        id = "tabselected") # Close the tabsetPanel, ### NEW add id for conditionalpanels
    ) # Close the mainPanel
    ) # Close the UI
  
  # Set Up Server Side Functions
  
  server <- function(input, output, session) {

    observeEvent(once = TRUE,ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = summary_data, { 
      # event will be called when summary_data initial default is selected, which only happens once, when app is first launched
      showModal(div(id="ModalDiv", modalDialog( # Use this for Chrome to work
        #showModal(modalDialog(
        title = "Welcome!",
        footer = modalActionButton("continue", "Continue"),
        easyclose = FALSE,
        h2("Obtaining a college degree used to guarantee social and economic mobility. But today, it is not so simple. In the face of a massive student debt crisis and an increasingly competitive job market, 
        young Americans have begun questioning whether higher education lives up to its promise as the great equalizer."),
        p("This app explores how college experiences matter for income mobility by combining `big data` with survey research.
          We link data on outcomes after college (collected by Opportunity Insights) to surveys of students at the beginning and end of college.
          These surveys, administered by the Higher Education Research Institute at UCLA, asked various questions regarding academic life, goals, 
        lifestyle choices, as well as social and political views. By combining these survey results with data on outcomes after college, we can examine 
        how college experiences influence success later in life.",
        style = "font-size:16px"),
        p(em("Which college experiences do you expect will be most influential in shaping student outcomes? The answers might surprise you!",
             style = "font-size:16px")),
        p('This app was created at Middlebury College for SOCI 1230 - Data Science Across The Disciplines - in Winter 2022.')
      )))
    })
    
    observeEvent(input$continue, {  # do something after user confirmation
      removeModal()
    })
    
    
    output$equalizer <- renderImage({
      list(
        src = file.path("equalizer.png"),
        contentType = "image/png"
        #width = 875,
        #height = 540.5
      )
    }, delete = FALSE)
    
    output$text <- renderText(paste("<center><big><br><br>Calling higher education `the great equalizer` means that students who attend similar colleges have similar life outcomes regardless of their family origins. 
                                    This figure, adapted from Opportunity Insights, confirms that average economic outcomes for young adults who attended the same types of college are very close even if they have different economic backgrounds. But why? 
                                     This app explores how different aspects of college influence future success. </big><center/><br>"
    ))
    
    
    # Dummy df to test quick facts in introduction tab
    
    output$intro_box <- renderText({ # Creates the object that will output in the UI
      
      fast_facts <- fast_facts |> 
        filter(rowid == sample(1:nrow(fast_facts), 1))
      
      paste(
        "<center><br><br><br><big>Here are some of our favorite findings from this project.</big><br>",
        as.character(fast_facts$Name), 
        ": <br>", 
        "<b>", 
        as.character(fast_facts$Fact), 
        "</b> <br><br>",
        "<em>For more fast facts reload this page.<br>When you are ready to make your own discoveries, click the next tab.</em><br><br><center/>"
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
    
    # Switch the correlations_all data
    selected_correlations_all <- reactive({ ### NEW
      switch(input$df_id,
             "The Freshman Survey" = tfs_correlations_labeled,
             "College Senior Survey" = css_correlations_labeled)
    })
    
    selected_correlations <- selected_correlations_all |>  debounce(1000) ### New
    
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

    
    output$question_df <- renderDataTable({
      
      datatable(all_questions, 
                filter = "top",
                caption = "All Survey Questions",
                rownames = FALSE)
    })
    
    
    
    ## Make a summary plot of the selected question (dropped from final version)
    
    # output$summary_plot <- renderPlot({ # Saving the plot as output that will feed into the Question Summary tab in UI
    #   
    #   ggplot(data = selected_question(), aes(x = reorder(Label, -wtdprop), 
    #                                          y = wtdprop*100,
    #                                          fill = Label)) +
    #     geom_col() +
    #     geom_text(aes(label = sprintf("%1.0f%%",round(wtdprop, 2)*100)),
    #               position = position_dodge(width = .9),
    #               vjust = -.25) +
    #     labs(x = "", y = "Proportion", fill = "",
    #          title = paste(selected_question()$Survey_Name, ": ", selected_question()$Name),
    #          subtitle = selected_question()$Description,
    #          caption = paste("This question comes from the", selected_question()$Survey)) +
    #     theme_economist() +
    #     theme(axis.ticks.x = element_blank(),
    #           legend.position = "none") +
    #     scale_fill_brewer(palette = "Set1") +
    #     scale_x_discrete(labels = label_wrap(10)
    #     )
    # })
    
    # In correlations tab change the response levels based on which question is selected
    observeEvent(selected_question(), {
      choices <- unique(selected_question()$Label)
      
      req(input$mobility_variable_id, input$df_id, 
          input$group_id, input$question_id)
      
      updateCheckboxGroupInput(inputId = "response_id", 
                               choices = choices,
                               selected = choices[1])
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
    
    # output$corr_plot <- renderPlot({
    #   mobility_df3() |>
    #     ggplot(aes(x = propsum, y = .data[[input$mobility_variable_id]],
    #                size = n_responses_nona, color = type)) +
    #     geom_point() +
    #     labs(title = paste("The overall correlation is:",
    #                        round(mean(mobility_df3()$weighted_correlation),3)),
    #          subtitle = paste("The correlation at public institutions is:",
    #                           round(mean(correlations()$wtd_correlation[2]),3),
    #                           ". \nThe correlation at private institutions is:",
    #                           round(mean(correlations()$wtd_correlation[1]),3)),
    #          caption = paste("Data are available for this survey question from",
    #                          mobility_df3()$total_colleges,
    #                          "colleges representing",
    #                          mobility_df3()$total_responses,
    #                          "students")) +
    #     scale_color_brewer(palette = "Dark2")
    # })
    
    mobility_labels <- tibble("Average rank" = "k_rank",
                              "Average rank if grew up in bottom 60%" = "k_rank_cond_parq123",
                              "Average rank if grew up in bottom quintile" = "k_rank_cond_parq1",
                              "Average rank if grew up in second lowest quintile" = "k_rank_cond_parq2",
                              "Average rank if grew up in middle quintile" = "k_rank_cond_parq3",
                              "Average rank if grew up in second highest quintile" = "k_rank_cond_parq4",
                              "Average rank if grew up in highest quintile" = "k_rank_cond_parq5",
                              "'Great Working Class Colleges' success rate (bottom quintile of parent distribution and highest 60% of young adult distribution)" = "kq345_cond_parq1",
                              "Opportunity Insights success rate (bottom quintile of parent distribution and top quintile of young adult distribution)" = "kq5_cond_parq1",
                              "Opportunity Insights mobility rate (Proportion of all students at an institution moving from bottom quintile of parent distribution to highest quintile of young adult distribution)" = "mr_kq5_pq1",
                              "Opportunity Insights upper tail mobility rate (bottom quintile of parent distribution and top 1% of young adult distribution)" = "mr_ktop1_pq1",
                              "Stickiness at the bottom (bottom quintile of parent distribution and bottom quintile of young adult distribution)" = "kq1_cond_parq1",
                              "Stickiness at the top (top quintile of parent distribution and top quintile of young adult distribution)" = "kq5_cond_parq5",
                              "Downward mobility from the top (top quintile of parent distribution and bottom quintile of young adult distribution)" = "kq1_cond_parq5",
                              "Proportion ending up in bottom quintile" = "k_q1",
                              "Proportion ending up in second lowest quintile" = "k_q2",
                              "Proportion ending up in middle quintile" = "k_q3",
                              "Proportion ending up in second highest quintile" = "k_q4",
                              "Proportion ending up in highest quintile" = "k_q5",
                              "Proportion from bottom quintile (Opportunity Insights access rate)" = "par_q1",
                              "Proportion from second lowest quintile" = "par_q2",
                              "Proportion from middle quintile" = "par_q3",
                              "Proportion from second highest quintile" = "par_q4",
                              "Proportion from highest quintile" = "par_q5") |>
      pivot_longer(cols = everything(), names_to = "Label", values_to = "Variable")
    
    output$corr_plot <- renderPlot({
      
      req(input$mobility_variable_id, input$df_id, 
          input$group_id, input$question_id)
      
      mobility_df3() |>
        ggplot(aes(x = propsum, y = .data[[input$mobility_variable_id]],
                   size = n_responses_nona, color = type)) +
        geom_point() +
        labs(title = str_wrap(paste("Correlation between the response(s) and",
                           mobility_labels$Label[mobility_labels$Variable == input$mobility_variable_id], ": ",
                           round(mean(mobility_df3()$weighted_correlation),3)), 80),
             subtitle = paste("The correlation is",
                              round(mean(correlations()$wtd_correlation[2]),3), "at public institutions and",
                              round(mean(correlations()$wtd_correlation[1]),3), "at private institutions.",
                              "\nEach dot represents one institution scaled to the number of survey responses."),
             caption = paste("Data are available for this survey question from",
                             mobility_df3()$total_colleges,
                             "colleges representing",
                             mobility_df3()$total_responses,
                             "students"),
             y = str_wrap(mobility_labels$Label[mobility_labels$Variable == input$mobility_variable_id], 30),
             x = "Proportion of students",
             size = "Number of responses",
             color = "Institution type") +
        theme_few(base_size = 15) +
        theme(legend.position = "bottom") + guides(size = "none")
      #scale_color_brewer(palette = "Spectral")
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
      
      req(input$mobility_variable_id, input$df_id, 
          input$group_id, input$question_id)
      
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
               qtle = ifelse(.data[[input$mobility_variable_id]] < as.numeric(quantiles()[1]), "Bottom Half",
                             ifelse(.data[[input$mobility_variable_id]] > as.numeric(quantiles()[2]), "Top Quartile", 
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
        mutate(qtle = factor(qtle,
                             levels = c("Bottom Half",
                                        "Third Quartile",
                                        "Top Quartile"))) |> 
        ggplot(aes(x = response_level, y = wtd.mean, fill = response_level)) +
        geom_col() +
        labs(x = "", y = "Weighted Mean",
             fill = "",
             title = paste(selected_question()$Group_rev, ": ", variable_description, sep = ""))+
        theme_economist_white() +
        theme(legend.position = "bottom",
              plot.background = element_rect(fill = "white"),
              legend.background = element_rect(fill = "white"),
              axis.ticks.x = element_blank(),
              axis.line.x = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_text(margin = margin(0,0,0,10)),
              plot.title = element_text(size = 16, face = "bold", margin=margin(0,0,30,0)),
              strip.text = element_text(face = "bold", margin=margin(0,0,10,0))) +
        facet_wrap(qtle~.) +
        scale_x_discrete(labels = label_wrap(10)) +
        geom_text(aes(label = round(wtd.mean, 3)),
                  position = position_dodge(width = 1.3),
                  vjust = -0.1,
                  size = 5)
    })
    
    output$quantiles <- renderPlot({
      return(mobility_df_test2())
    })
    
    
    ## For Summary Correlations Plots  ### New
    
    output$corr_summary_plot <- renderPlotly({
      
      req(input$mobility_variable_id, input$df_id)
      
      positive <- selected_correlations_all() |> 
        filter(corr_type == "Pooled", 
               #Group!="Constructed Scales", # Messy because of TFS
               !str_detect(Summary, "cant")) |> 
        select(input$mobility_variable_id, Group, Group_rev,
               Description, Summary, Label) |> 
        group_by(Group_rev) |>
        slice_max(.data[[input$mobility_variable_id]], n = 1) |>
        mutate(corr_sign = "Positive Correlation") |> 
        arrange(-.data[[input$mobility_variable_id]])
      
      negative <- selected_correlations_all() |> 
        filter(corr_type == "Pooled", 
               !str_detect(Summary, "cant")) |> 
        select(input$mobility_variable_id, Group, Group_rev,
               Description, Summary, Label) |> 
        group_by(Group_rev) |> 
        slice_min(.data[[input$mobility_variable_id]], n = 1) |> 
        mutate(corr_sign = "Negative Correlation") |> 
        arrange(-.data[[input$mobility_variable_id]])
      
      correlations_sign <- bind_rows(positive, negative)
      
      group_levels <- correlations_sign |> 
        filter(corr_sign=="Positive Correlation") |> 
        arrange(.data[[input$mobility_variable_id]]) |> 
        pull(Group_rev) 
      
      font <- list(
        family = "Arial",
        size = 16,
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
        labs(x = "", y = "",
             title = "Strongest Correlations by Question Group",
             color = "") +
        scale_color_manual(values = c("red", "forest green")) +
        theme_economist_white() +
        theme(legend.position = "bottom",
              plot.background = element_rect(fill = "white"),
              legend.background = element_rect(fill = "white"),
              axis.ticks.x = element_blank(),
              axis.line.x = element_blank(),
              #axis.text.x = element_blank(),
              axis.text.y = element_text(size = 14),
              plot.title = element_text(size = 16, face = "bold")) 
      
      
      output$text1 <- renderText(paste("This figure displays the strongest positive and negative correlations in each group of survey questions and your mobility variable.
                                      You can change the selected survey in the drop down menu. Search the table in the last tab to see all questions in each survey and group."))
      
      ggplotly(correlations_summary_plot, tooltip = "text") |> 
        layout(legend = list(orientation = "h", x = .25, y = -0.25)) |> 
        style(hoverlabel = label) |> 
        layout(font = font)
      
    })
  }
  
  #} # Close the server side functions
  
  # Run The Application 
  shinyApp(ui = ui, server = server, 
           options = list(launch.browser = TRUE)) # needed for fullscreen
#} # closes if interactive