# Load necessary libraries
library(shinythemes)
library(tidyverse)
library(shiny)
library(DT)
library(bslib)
library(showtext)
library(thematic)
library(data.table)
library(igraph)
library(ggplot2)
library(circlize)
library(chorddiag)
library(dplyr)
library(tidyr)
library(tibble)
library(rlang)
library(visNetwork)
source("global.R")



# Creating ShinyApp theme
my_theme <- bs_theme(
  bootswatch = "darkly",
  primary = "#1BE99E",
  base_font = font_google("Ubuntu")
)


ui <- fluidPage(
  theme = my_theme,
  titlePanel("Football Players Transfers"),
  tabsetPanel(
    navbarMenu("Home",
               tabPanel("About the App",
                        titlePanel("About the Football Players Transfers App"),
                        textOutput("about_the_app")
               ),
               tabPanel("Metadata",
                        titlePanel("Metadata"),
                        textOutput("metadata"),
                        dataTableOutput("metadata_table")
               )
    ),
    navbarMenu("Descriptive Statistics",
               tabPanel("General",
                        titlePanel("General Descriptive Statistics"),
                        textOutput("general_descriptive_statistics"),
                        dataTableOutput("general_statistics_table")
               ),
               tabPanel("League",
                        titlePanel("League Statistics"),
                        textOutput("league_descriptive_statistics"),
                        fluidRow(
                          column(2, selectInput("season", "Select Season", choices = c("All Seasons", unique(dt.player.transfers$season)))),
                          column(2, sliderInput("age", "Age", min = min(dt.player.transfers$age, na.rm = TRUE), max = max(dt.player.transfers$age, na.rm = TRUE), value = c(min(dt.player.transfers$age, na.rm = TRUE), max(dt.player.transfers$age, na.rm = TRUE)), step = 1)),
                          column(2, selectInput("position", "Position", choices = unique(dt.player.transfers$position), multiple = TRUE)),
                          column(2, selectInput("nationality", "Nationality", choices = unique(dt.player.transfers$nationality), multiple = TRUE))),
                        fluidRow(
                          column(2, sliderInput("market_value", "Market Value", min = min(dt.player.transfers$market_value, na.rm = TRUE), max = max(dt.player.transfers$market_value, na.rm = TRUE), value = c(min(dt.player.transfers$market_value, na.rm = TRUE), max(dt.player.transfers$market_value, na.rm = TRUE)), step = 1000)),
                          column(2, sliderInput("transfer_fee", "Transfer Fee", min = min(dt.player.transfers$fee, na.rm = TRUE), max = max(dt.player.transfers$fee, na.rm = TRUE), value = c(min(dt.player.transfers$fee, na.rm = TRUE), max(dt.player.transfers$fee, na.rm = TRUE)), step = 1000)),
                          column(2, checkboxGroupInput("window", "Transfer Window", choices = unique(dt.player.transfers$window))),
                          column(2, checkboxGroupInput("loan", "Loan", choices = unique(dt.player.transfers$is_loan)))
                        ),
                        mainPanel(
                          DT::dataTableOutput("filtered_league_table")
                        )
               ),
               tabPanel("Clubs",
                        titlePanel("Club Statistics"),
                        textOutput("club_descriptive_statistics"),
                        fluidRow(
                          column(2, selectInput("season2", "Select Season", choices = c("All Seasons", unique(dt.player.transfers$season)))),
                          column(2, sliderInput("age2", "Age", min = min(dt.player.transfers$age, na.rm = TRUE), max = max(dt.player.transfers$age, na.rm = TRUE), value = c(min(dt.player.transfers$age, na.rm = TRUE), max(dt.player.transfers$age, na.rm = TRUE)), step = 1)),
                          column(2, selectInput("position2", "Position", choices = unique(dt.player.transfers$position), multiple = TRUE)),
                          column(2, selectInput("nationality2", "Nationality", choices = unique(dt.player.transfers$nationality), multiple = TRUE)),
                          column(2, selectInput("club_buyer", "Club Buyer", choices = unique(dt.player.transfers$club_buyer), multiple = TRUE))
                        ),
                        fluidRow(
                          column(2, sliderInput("market_value2", "Market Value", min = min(dt.player.transfers$market_value, na.rm = TRUE), max = max(dt.player.transfers$market_value, na.rm = TRUE), value = c(min(dt.player.transfers$market_value, na.rm = TRUE), max(dt.player.transfers$market_value, na.rm = TRUE)), step = 1000)),
                          column(2, sliderInput("transfer_fee2", "Transfer Fee", min = min(dt.player.transfers$fee, na.rm = TRUE), max = max(dt.player.transfers$fee, na.rm = TRUE), value = c(min(dt.player.transfers$fee, na.rm = TRUE), max(dt.player.transfers$fee, na.rm = TRUE)), step = 1000)),
                          column(2, checkboxGroupInput("window2", "Transfer Window", choices = unique(dt.player.transfers$window))),
                          column(2, checkboxGroupInput("loan2", "Loan", choices = unique(dt.player.transfers$is_loan)))
                        ),
                        mainPanel(
                          DT::dataTableOutput("filtered_clubs_table")
                        )
               ),
               tabPanel("Nationality",
                        titlePanel("Nationality Statistics"),
                        textOutput("nationality_descriptive_statistics"),
                        fluidRow(
                          column(2, selectInput("season3", "Select Season", choices = c("All Seasons", unique(dt.player.transfers$season)))),
                          column(2, sliderInput("age3", "Age", min = min(dt.player.transfers$age, na.rm = TRUE), max = max(dt.player.transfers$age, na.rm = TRUE), value = c(min(dt.player.transfers$age, na.rm = TRUE), max(dt.player.transfers$age, na.rm = TRUE)), step = 1)),
                          column(2, selectInput("position3", "Position", choices = unique(dt.player.transfers$position), multiple = TRUE)),
                          column(2, selectInput("nationality3", "Nationality", choices = unique(dt.player.transfers$nationality), multiple = TRUE)),
                          column(2, selectInput("club_buyer2", "Club Buyer", choices = unique(dt.player.transfers$club_buyer), multiple = TRUE))
                        ),
                        fluidRow(
                          column(2, sliderInput("market_value3", "Market Value", min = min(dt.player.transfers$market_value, na.rm = TRUE), max = max(dt.player.transfers$market_value, na.rm = TRUE), value = c(min(dt.player.transfers$market_value, na.rm = TRUE), max(dt.player.transfers$market_value, na.rm = TRUE)), step = 1000)),
                          column(2, sliderInput("transfer_fee3", "Transfer Fee", min = min(dt.player.transfers$fee, na.rm = TRUE), max = max(dt.player.transfers$fee, na.rm = TRUE), value = c(min(dt.player.transfers$fee, na.rm = TRUE), max(dt.player.transfers$fee, na.rm = TRUE)), step = 1000)),
                          column(2, checkboxGroupInput("window3", "Transfer Window", choices = unique(dt.player.transfers$window))),
                          column(2, checkboxGroupInput("loan3", "Loan", choices = unique(dt.player.transfers$is_loan)))
                        ),
                        mainPanel(
                          DT::dataTableOutput("filtered_nationality_table")
                        )
               ),
               tabPanel("Player",
                        titlePanel("Player Statistics"),
                        textOutput("player_descriptive_statistics"),
                        fluidRow(
                          column(2, selectInput("season4", "Select Season", choices = c("All Seasons", unique(dt.player.transfers$season)))),
                          column(2, sliderInput("age4", "Age", min = min(dt.player.transfers$age, na.rm = TRUE), max = max(dt.player.transfers$age, na.rm = TRUE), value = c(min(dt.player.transfers$age, na.rm = TRUE), max(dt.player.transfers$age, na.rm = TRUE)), step = 1)),
                          column(2, selectInput("position4", "Position", choices = unique(dt.player.transfers$position), multiple = TRUE)),
                          column(2, selectInput("nationality4", "Nationality", choices = unique(dt.player.transfers$nationality), multiple = TRUE)),
                          column(2, selectInput("club_buyer3", "Club Buyer", choices = unique(dt.player.transfers$club_buyer), multiple = TRUE))
                        ),
                        fluidRow(
                          column(2, sliderInput("market_value4", "Market Value", min = min(dt.player.transfers$market_value, na.rm = TRUE), max = max(dt.player.transfers$market_value, na.rm = TRUE), value = c(min(dt.player.transfers$market_value, na.rm = TRUE), max(dt.player.transfers$market_value, na.rm = TRUE)), step = 1000)),
                          column(2, sliderInput("transfer_fee4", "Transfer Fee", min = min(dt.player.transfers$fee, na.rm = TRUE), max = max(dt.player.transfers$fee, na.rm = TRUE), value = c(min(dt.player.transfers$fee, na.rm = TRUE), max(dt.player.transfers$fee, na.rm = TRUE)), step = 1000)),
                          column(2, checkboxGroupInput("window4", "Transfer Window", choices = unique(dt.player.transfers$window))),
                          column(2, checkboxGroupInput("loan4", "Loan", choices = unique(dt.player.transfers$is_loan)))
                        ),
                        mainPanel(
                          DT::dataTableOutput("filtered_player_table")
                        ))
    ),
    navbarMenu("Network Exploration",
               tabPanel("Network Statistics",
                        titlePanel("Network Exploratory Statistics"),
                        br(),
                        textOutput("network_exploratory_statistics"),
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            verticalLayout(
                              selectInput(inputId = "filter_network_stats_1", label = "Choose the Network to analyse", choices = c("Bipartite Graph: Clubs and Players", "Bipartite Projection: Clubs"), width = "350px"),
                              selectInput(inputId = "centrality_filter", label = "Choose which Centrality Measure to Analyse", choices = c("Degree Centrality", "Closeness Centrality", "Betweenness Centrality", "Eigenvector Centrality"))
                            )
                          ),
                          mainPanel(
                            selectInput(inputId = "network_exploratory_1_1", label = "Select Leagues", choices = selected.leagues, multiple = TRUE, selected = c("Eredivisie", "Laliga", "1 Bundesliga"), width = "500px"))
                          
                        ),
                        
                        uiOutput("content_network_stats_1")
                        
               ),
               
               tabPanel("Transfers at club level",
                        titlePanel("Transfers at club level"),
                        br(),
                        textOutput("text_network_exploratory_page2"),
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            verticalLayout(
                              selectInput(inputId = "filter_position", label = "Choose the Position to analyse", choices = c("Attack", "Midfield", "Defense"), width = "350px"),
                              selectInput(inputId = "filter_clubs", label = "Select Clubs", choices = selected.clubs.advanced2, multiple = TRUE, selected = c("Real Madrid", "Chelsea FC", "Juventus FC", "SL Benfica"), width = "350px"),
                              sliderInput(inputId = "filter_age", label = "Choose Minimum and Maximum Age", min = min(unique(dt.player.transfers$age), na.rm = TRUE), max = max(unique(dt.player.transfers$age), na.rm = TRUE), value = c(min(unique(dt.player.transfers$age), na.rm = TRUE), max(unique(dt.player.transfers$age), na.rm = TRUE)), step = 1, width = "1000px"),
                              sliderInput(inputId = "filter_fee", label = "Choose Minimum and Maximum Fee", min = min(unique(dt.player.transfers$fee), na.rm = TRUE), max = max(unique(dt.player.transfers$fee), na.rm = TRUE), value = c(min(unique(dt.player.transfers$fee), na.rm = TRUE), max(unique(dt.player.transfers$fee), na.rm = TRUE)), step = 1)
                            )
                          ),
                          mainPanel(
                            visNetworkOutput("graph_network_exploratory_page2"))
                        )
               ),
               
               tabPanel("A look at transfers between leagues",
                        titlePanel("A look at transfers between leagues"),
                        textOutput("network_exploratory_statistics_3"),
                        fluidRow(
                          column(width = 12, align = "center",
                                 selectInput("filter_league_chord", "Select League", choices = selected_leagues, multiple = TRUE, selected = c("Premier League", "Laliga", "1 Bundesliga"))
                          )
                        ),
                        fluidRow(
                          column(width = 12, align = "center",
                                 plotOutput("my_plot", height = "400px", width = "400px")
                          )
                        )
               ),
               tabPanel("Bipartite Network",
                        titlePanel("Bipartite Network"),
                        br(),
                        textOutput("network_exploratory_statistics_4"),
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            verticalLayout(
                              selectInput("league_filter", "Select LeagueS", choices = unique(dt.player.transfers$league), multiple = TRUE, selected = c("Premier League", "Laliga", "1 Bundesliga"), width = "500px"),
                              sliderInput(inputId = "degree_filter", label = "Select the Degree", min = 0, max = 5, value = 2),
                              sliderInput(inputId = "filter_season", label = "Choose the Season Range", min = min(unique(dt.player.transfers$season), na.rm = TRUE), max = max(unique(dt.player.transfers$season), na.rm = TRUE), value = c(min(unique(dt.player.transfers$season), na.rm = TRUE), max(unique(dt.player.transfers$season), na.rm = TRUE)), step = 1, width = "1000px"),
                            )
                          ),
                          mainPanel(
                            visNetworkOutput("network_exploratory_4")
                          )
                        )
               )
    ),
    navbarMenu("Advanced Analysis",
               tabPanel("Club Similarity",
                        titlePanel("Club Similarity"),
                        br(),
                        textOutput("network_advanced_page1"),
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "filter_advanced_clubs", label = "Select a Club", choices = selected.clubs.advanced2, multiple = FALSE, width = "350px"),
                          ),
                          mainPanel(
                            tableOutput("club_similarity_table")
                          )
                        )
               ),
               tabPanel("Link Predictions: Future Transfers between Clubs",
                        titlePanel("Football Players Transfers"),
                        br(),
                        textOutput("advanced_page2"),
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "filter_clubs_advanced2", label = "Select Clubs", choices = selected.clubs.advanced2, multiple = TRUE, selected = c("Real Madrid", "Chelsea FC", "Juventus FC", "SL Benfica"), width = "500px"),
                            selectInput(inputId = "threshold", label = "Select the desired Threshold", choices = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
                          ),
                          mainPanel(
                            visNetworkOutput("graph_advanced_page2")
                          )
                        )
               ),
               tabPanel("Similarity",
                        titlePanel("Similarity Analysis"),
                        br(),
                        textOutput("text_advanced_page3"),
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            verticalLayout(
                              textInput("name_filter", "Enter your name", value = ""),
                              selectInput(inputId = "filter_clubs_advanced2", label = "Select Clubs", choices = unique(dt.player.transfers$club_buyer), multiple = TRUE, selected = c("FC Molenbeek Brüssel", "Villarreal CF", "Stade Rennais FC", "CS Marítimo", "Ascoli Calcio 1898", "Burnley FC", "Eintracht Frankfurt", "Parma FC", "SV Zulte Waregem"), width = "500px"),
                              sliderInput(inputId = "age_player_3", label = "Choose the Age of the Player", min = min(unique(dt.player.transfers$age), na.rm = TRUE), max = max(unique(dt.player.transfers$age), na.rm = TRUE), value = 20, step = 1, width = "1000px"),
                              selectInput(inputId = "position_filter_3", label = "Select the Player's Position", choices = unique(dt.player.transfers$position), multiple = TRUE, selected = "Goalkeeper"),
                              sliderInput(inputId = "filter_market_value", label = "Choose the Market Value Range", min = min(unique(dt.player.transfers$market_value), na.rm = TRUE), max = max(unique(dt.player.transfers$market_value), na.rm = TRUE), value = c(0, 13000), step = 1000, width = "1000px"),
                              sliderInput(inputId = "filter_fee", label = "Choose the Fee's Range", min = min(unique(dt.player.transfers$fee), na.rm = TRUE), max = max(unique(dt.player.transfers$fee), na.rm = TRUE), value = c(min(unique(dt.player.transfers$fee), na.rm = TRUE), max(unique(dt.player.transfers$fee), na.rm = TRUE)), step = 1000, width = "1000px"),
                              sliderInput(inputId = "filter_diff", label = "Choose the Maximum Difference Between the Market Value and Fee", min = min(unique(dt.player.transfers$diff), na.rm = TRUE), max = max(unique(dt.player.transfers$diff), na.rm = TRUE), value = 100000, step = 1, width = "1000px")
                            )
                          ),
                          mainPanel(
                            visNetworkOutput("plot_advanced_page3"),
                            tableOutput("table_advanced_page3")
                          )
                        )
               )
    )
  )
)
# Server code
server <- function(input, output) {
  
  
  #----------------------------------------------------------------------------------------------    
  #about the APP page
  output$about_the_app <- renderText({
    "Welcome to our network analysis Shiny app! Here, you can explore the intricate web of player transfers between clubs in several leagues over the 2005-2021 period. Our app provides a unique perspective on the movements of football players, and allows you to visualize and analyze the network of transfers through a variety of tools and visualizations.
With our interactive filters, you can easily choose the leagues and time frame you want to analyze, and view detailed network statistics such as the number of nodes, edges, and degree centrality. There is also a section with extensive exploratory network analysis, and another one which answers questions regarding similarity and link prediction.
We hope our app provides you with a fascinating glimpse into the world of football player transfers. Enjoy exploring the network!"
  })
  
  # Metadata page
  output$metadata <- renderText({
    "The metadata table below provides a comprehensive overview of the variables included in the app. We hope this metadata helps you navigate through the app and provides you with useful insights into the world of Football! Enjoy your exploration!"
  })
  output$metadata_table <- DT::renderDataTable({
    DT::datatable(metadata,
                  options = list(
                    dom = 'Bfrtip',
                    pageLength = 10,
                    language = list(
                      search = '_INPUT_',
                      searchPlaceholder = 'Search...'
                    )
                  )
    ) %>%
      formatStyle(names(metadata), backgroundColor = '#F0F0F0', color = '#000000')
    
  })  
  
  
  #----------------------------------------------------------------------------------------------    
  #CHORD DIAGRAM
  output$my_plot <- renderPlot({
    par(bg = "transparent")
    # chord reactive
    diagram <- reactive({
      club_leagues <- V(g2.league)$name
      
      # Filter the graph based on the selected leagues
      g.filtered <- g2.league %>%
        delete.vertices(which(!club_leagues %in% input$filter_league_chord))
      
      # Convert the edge list of g.filtered into a data frame
      edges_df <- get.data.frame(g.filtered, what = "edges")
      
      # Create a data frame with all possible node combinations from the selected leagues
      all_combinations <- tidyr::crossing(from = input$filter_league_chord, to = input$filter_league_chord)
      
      # Left join the all_combinations data frame with the edges_df data frame, filling missing values with zeros
      m <- all_combinations %>%
        left_join(edges_df, by = c("from", "to")) %>%
        replace_na(list(weight = 0)) %>%
        pivot_longer(cols = -c(from, to)) %>%
        pivot_wider(names_from = to, values_from = value, values_fill = 0) %>%
        column_to_rownames(var = "from")
      
      # Reorder the matrix according to the selected_leagues
      m_reordered <- m[match(input$filter_league_chord, rownames(m)), match(input$filter_league_chord, colnames(m))]
      
      # Return the reordered matrix
      return(m_reordered)
    })
    
    # Your plot code here
    # Convert the reordered matrix to a chord diagram
    chordDiagram(as.matrix(diagram()))
  })
  
  #----------------------------------------------------------------------------------------------
  
  
  # general stats page
  output$general_descriptive_statistics <- renderText({
    "In this page of the app, you can find very general statistics about our data set that allows the user to have a broad sense of what we are dealing with. "
  })
  
  #GENERAL STATS TABLE
  output$general_statistics_table <- DT::renderDataTable({
    DT::datatable(stats_df_t,
                  options = list(
                    dom = 'Bfrtip',
                    pageLength = 10,
                    language = list(
                      search = '_INPUT_',
                      searchPlaceholder = 'Search...'
                    )
                  )
    ) %>%
      formatStyle(names(stats_df_t), backgroundColor = '#F0F0F0', color = '#000000')
    
  })
  
  # league stats page
  output$league_descriptive_statistics <- renderText({
    "In this page of the app, you can find relevant descriptive statistics about each one of the leagues in a data table. To get this, we have grouped our original dataset by leagues and calculated the statistics we think will add value to the user.  
In addition to this, it is also possible to apply filters to the data table such as season the transfer occurred, age of the players, position of the players, nationality of the players, market value of the players, transfer fee of the transfers, transfer window in which the transfers occurred and whether the transfer was a loan or not. This way, the relevant stats for each league will be calculated according to the selected filters. "
  })
  
  
  
  #VASCO LEAGUE TABLE
  filtered_data <- reactive({
    if (input$season == "All Seasons") {
      dt.player.transfers_filt <- dt.player.transfers
    } else {
      # Filter dt.player.transfers based on the selected season
      dt.player.transfers_filt <- dt.player.transfers[season == input$season]
    }
    # Apply additional filters
    dt.player.transfers_filt <- dt.player.transfers_filt[age >= input$age[1] & age <= input$age[2]]
    if (length(input$position) > 0) {
      dt.player.transfers_filt <- dt.player.transfers_filt[position %in% input$position]
    }
    if (length(input$nationality) > 0) {
      dt.player.transfers_filt <- dt.player.transfers_filt[nationality %in% input$nationality]
    }
    dt.player.transfers_filt <- dt.player.transfers_filt[market_value >= input$market_value[1] & market_value <= input$market_value[2]]
    dt.player.transfers_filt <- dt.player.transfers_filt[fee >= input$transfer_fee[1] & fee <= input$transfer_fee[2]]
    if (length(input$window) > 0) {
      dt.player.transfers_filt <- dt.player.transfers_filt[window %in% input$window]
    }
    if (length(input$loan) > 0) {
      dt.player.transfers_filt <- dt.player.transfers_filt[is_loan %in% input$loan]
    }
    # Compute aggregated statistics for the filtered data
    dt.player.transfers.by.league_filt <- dt.player.transfers_filt[, list(Number_Total_Tranfers = .N, Number_Loans = sum(str_count(is_loan, "True")), Number_Players = length(unique(name)), Number_Clubs = length(unique(club_buyer)), Number_Selling_Clubs = length(unique(club_seller)), Number_Nationalities = length(unique(nationality)), Number_Selling_Countries = length(unique(dealing_country)), Average_Age = mean(age, na.rm = TRUE), Maximum_Age = max(age, na.rm = TRUE), Minimum_Age = min(age, na.rm = TRUE), Standard_Deviation_Age = sd(age, na.rm = TRUE), Average_Market_Value = mean(market_value, na.rm = TRUE), Maximum_Market_Value = max(market_value, na.rm = TRUE), Minimum_Market_Value = min(market_value, na.rm = TRUE), Standard_Deviation_Market_Value = sd(market_value, na.rm = TRUE), Average_Transfer_Fee = mean(fee, na.rm = TRUE), Maximum_Transfer_Fee = max(fee, na.rm = TRUE), Minimum_Transfer_Fee = min(fee, na.rm = TRUE), Standard_Deviation_Transfer_Fee = sd(fee, na.rm = TRUE)), by = league]
    
    setnames(dt.player.transfers.by.league_filt, "league", "League")
    
    return(dt.player.transfers.by.league_filt)
  })
  
  # Render league table
  output$filtered_league_table <- DT::renderDataTable({
    filtered_data()
  }, width = "100%", height = "auto", options = list(scrollX = TRUE))
  
  #----------------------------------------------------------------------------------------------
  # clubs stats page
  output$club_descriptive_statistics <- renderText({
    "In this page of the app, you can find relevant descriptive statistics about each one of the clubs in a data table. To get this, we have grouped our original dataset by clubs and calculated the statistics we think will add value to the user.
In addition to this, it is also possible to apply filters to the data table such as season the transfer occurred, age of the players, position of the players, nationality of the players, market value of the players, transfer fee of the transfers, transfer window in which the transfers occurred and whether the transfer was a loan or not. This way, the relevant stats for each club will be calculated according to the selected filters."
  })
  
  #VASCO CLUBS TABLE
  filtered_data_clubs <- reactive({
    dt.player.transfers_filt <- dt.player.transfers %>%
      filter((season == input$season2) | (input$season2 == "All Seasons")) %>%
      filter(age >= input$age2[1] & age <= input$age2[2]) %>%
      filter(if (length(input$position2) > 0) position %in% input$position2 else TRUE) %>%
      filter(if (length(input$nationality2) > 0) nationality %in% input$nationality2 else TRUE)%>%
      filter(if (length(input$club_buyer) > 0) club_buyer %in% input$club_buyer else TRUE) %>%
      filter(market_value >= input$market_value2[1] & market_value <= input$market_value2[2]) %>%
      filter(fee >= input$transfer_fee2[1] & fee <= input$transfer_fee2[2]) %>%
      filter(if (length(input$window2) > 0) window %in% input$window2 else TRUE) %>%
      filter(if (length(input$loan2) > 0) is_loan %in% input$loan2 else TRUE)
    
    dt.player.transfers.by.club_filt <- dt.player.transfers_filt[, list(League=unique(league), Number_Total_Tranfers = .N, Number_Loans = sum(str_count(is_loan, "True")), Number_Players = length(unique(name)), Number_Selling_Clubs = length(unique(club_seller)), Number_Nationalities = length(unique(nationality)), Number_Selling_Countries = length(unique(dealing_country)), Average_Age = mean(age, na.rm = TRUE), Maximum_Age = max(age, na.rm = TRUE), Minimum_Age = min(age, na.rm = TRUE), Standard_Deviation_Age = sd(age, na.rm = TRUE), Average_Market_Value = mean(market_value, na.rm = TRUE), Maximum_Market_Value = max(market_value, na.rm = TRUE), Minimum_Market_Value = min(market_value, na.rm = TRUE), Standard_Deviation_Market_Value = sd(market_value, na.rm = TRUE), Average_Transfer_Fee = mean(fee, na.rm = TRUE), Maximum_Transfer_Fee = max(fee, na.rm = TRUE), Minimum_Transfer_Fee = min(fee, na.rm = TRUE), Standard_Deviation_Transfer_Fee = sd(fee, na.rm = TRUE)), by = club_buyer]
    
    setnames(dt.player.transfers.by.club_filt, "club_buyer", "Club")
    
    return(dt.player.transfers.by.club_filt)
  })
  # Render clubs table
  output$filtered_clubs_table <- DT::renderDataTable({
    filtered_data_clubs()
  }, width = "100%", height = "auto", options = list(scrollX = TRUE, searching = FALSE))
  
  #----------------------------------------------------------------------------------------------
  # nationality stats page
  output$nationality_descriptive_statistics <- renderText({
    "In this page of the app, you can find relevant descriptive statistics about each one of the nationalities in a data table. To get this, we have grouped our original dataset by nationality and calculated the statistics we think will add value to the user.
In addition to this, it is also possible to apply filters to the data table such as season the transfer occurred, age of the players, position of the players, nationality of the players, market value of the players, transfer fee of the transfers, transfer window in which the transfers occurred and whether the transfer was a loan or not. This way, the relevant stats for each nationality will be calculated according to the selected filters."
  })  
  
  #VASCO Nationality TABLE
  filtered_data_nationality <- reactive({
    dt.player.transfers_filt <- dt.player.transfers %>%
      filter((season == input$season3) | (input$season3 == "All Seasons")) %>%
      filter(age >= input$age3[1] & age <= input$age3[2]) %>%
      filter(if (length(input$position3) > 0) position %in% input$position3 else TRUE) %>%
      filter(if (length(input$nationality3) > 0) nationality %in% input$nationality3 else TRUE) %>%
      filter(if (length(input$club_buyer2) > 0) club_buyer %in% input$club_buyer2 else TRUE) %>%
      filter(market_value >= input$market_value3[1] & market_value <= input$market_value3[2]) %>%
      filter(fee >= input$transfer_fee3[1] & fee <= input$transfer_fee3[2]) %>%
      filter(if (length(input$window3) > 0) window %in% input$window3 else TRUE) %>%
      filter(if (length(input$loan3) > 0) is_loan %in% input$loan3 else TRUE)
    
    dt.player.transfers.by.nation_filt <- dt.player.transfers_filt[, list(Number_Leagues = length(unique(league)), Number_Total_Tranfers = .N, Number_Loans = sum(str_count(is_loan, "True")), Number_Players = length(unique(name)), Number_Clubs = length(unique(club_buyer)), Number_Selling_Clubs = length(unique(club_seller)), Number_Selling_Countries = length(unique(dealing_country)), Average_Age = mean(age, na.rm = TRUE), Maximum_Age = max(age, na.rm = TRUE), Minimum_Age = min(age, na.rm = TRUE), Standard_Deviation_Age = sd(age, na.rm = TRUE), Average_Market_Value = mean(market_value, na.rm = TRUE), Maximum_Market_Value = max(market_value, na.rm = TRUE), Minimum_Market_Value = min(market_value, na.rm = TRUE), Standard_Deviation_Market_Value = sd(market_value, na.rm = TRUE), Average_Transfer_Fee = mean(fee, na.rm = TRUE), Maximum_Transfer_Fee = max(fee, na.rm = TRUE), Minimum_Transfer_Fee = min(fee, na.rm = TRUE), Standard_Deviation_Transfer_Fee = sd(fee, na.rm = TRUE)), by = nationality]
    
    setnames(dt.player.transfers.by.nation_filt, "nationality", "Nationality")
    
    return(dt.player.transfers.by.nation_filt)
  })
  
  output$filtered_nationality_table <- DT::renderDataTable({
    filtered_data_nationality()
  }, width = "100%", height = "auto", options = list(scrollX = TRUE, searching = FALSE))
  
  
  
  #---------------------------------------------------------------------------------------------- 
  
  # players stats page
  output$player_descriptive_statistics <- renderText({
    "In this page of the app, you can find relevant descriptive statistics about each one of the players in a data table. To get this, we have grouped our original dataset by players and calculated the statistics we think will add value to the user. Although a lot of players have only had one transfer, keep in mind that a lot of them have also had more than one, hence the relevance of grouping our data by player.
In addition to this, it is also possible to apply filters to the data table such as season the transfer occurred, age of the players, position of the players, nationality of the players, market value of the players, transfer fee of the transfers, transfer window in which the transfers occurred and whether the transfer was a loan or not. This way, the relevant stats for each player will be calculated according to the selected filters."
  }) 
  
  #VASCO Players TABLE
  filtered_data_player <- reactive({
    dt.player.transfers_filt <- dt.player.transfers %>%
      filter((season == input$season4) | (input$season4 == "All Seasons")) %>%
      filter(age >= input$age4[1] & age <= input$age4[2]) %>%
      filter(if (length(input$position4) > 0) position %in% input$position4 else TRUE) %>%
      filter(if (length(input$nationality4) > 0) nationality %in% input$nationality4 else TRUE) %>%
      filter(if (length(input$club_buyer3) > 0) club_buyer %in% input$club_buyer3 else TRUE) %>%
      filter(market_value >= input$market_value4[1] & market_value <= input$market_value4[2]) %>%
      filter(fee >= input$transfer_fee4[1] & fee <= input$transfer_fee4[2]) %>%
      filter(if (length(input$window4) > 0) window %in% input$window4 else TRUE) %>%
      filter(if (length(input$loan4) > 0) is_loan %in% input$loan4 else TRUE)
    
    dt.player.transfers.by.player_filt <- dt.player.transfers_filt[, list(Number_Positions_Played = length(unique(position)), Nationality = unique(nationality), Number_Leagues = length(unique(league)), Number_Total_Tranfers = .N, Number_Loans = sum(str_count(is_loan, "True")), Number_Clubs = length(unique(club_buyer)), Number_Selling_Clubs = length(unique(club_seller)), Number_Selling_Countries = length(unique(dealing_country)), Maximum_Age = max(age, na.rm = TRUE), Minimum_Age = min(age, na.rm = TRUE), Maximum_Market_Value = max(market_value, na.rm = TRUE), Minimum_Market_Value = min(market_value, na.rm = TRUE), Average_Transfer_Fee = mean(fee, na.rm = TRUE), Maximum_Transfer_Fee = max(fee, na.rm = TRUE), Minimum_Transfer_Fee = min(fee, na.rm = TRUE)), by = name]
    
    setnames(dt.player.transfers.by.player_filt, "name", "Player")
    
    return(dt.player.transfers.by.player_filt)
  })
  
  output$filtered_player_table <- DT::renderDataTable({
    filtered_data_player()
  }, width = "100%", height = "auto", options = list(scrollX = TRUE, searching = FALSE))
  
  
  #--------------------------------------------------------------------------------------------------  
  # Network Exploratory Statistics 1
  output$network_exploratory_statistics <- renderText({
    "This tab provides detailed information on the Club network, which consists of players who were transferred to different clubs. You can choose to analyze the network at the Bipartite Network level, which includes both players and clubs, or at the clubs level based on players. The Network Statistics table shows basic statistics such as the number of nodes, edges, average degree centrality, average path length, and diameter of the network. We examine degree, closeness, betweenness and eigen vector as measures of centrality. In order to do this, a table of the top 5 ascending and descending nodes with the highest centrality is displayed for each measure, in each network, along with a histogram showing the degree distribution of the entire network. There is also a drop-down menu to filter data by league."
  })
  
  output$network.statistics.bipartite <- renderTable({
    # Create selected vertices
    selected.clubs <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(club_buyer)]
    selected.players <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(name)]
    all.selected <- rbind(selected.clubs, selected.players)
    
    # Filter vertices by selected leagues
    sub.vertices <- V(g)[name %in% all.selected]
    
    # Create subgraph
    g.filtered <- induced_subgraph(g, sub.vertices)
    
    # Calculating centrality measures of bipartite network
    num.nodes <- vcount(g.filtered)
    num.edges <- ecount(g.filtered)
    avg.degree <- mean(degree(g.filtered))
    cc <- transitivity(g.filtered, type="average")
    avg.path.len <- mean_distance(g.filtered)
    diam <- diameter(g.filtered)
    
    network.statistics.table <- data.frame(Measure= c("Number of Nodes", "Number of Edges", "Average Degree", "Clustering Coefficient", "Average Path Lenght", "Diameter"), Value= c(num.nodes , num.edges , avg.degree , cc , avg.path.len , diam))
    
  })
  
  output$top.5.degree.A.input.out <- renderTable({
    # Create selected vertices
    selected.clubs <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(club_buyer)]
    selected.players <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(name)]
    all.selected <- rbind(selected.clubs, selected.players)
    
    # Filter vertices by selected leagues
    sub.vertices <- V(g)[name %in% all.selected]
    
    # Create subgraph
    g.filtered <- induced_subgraph(g, sub.vertices)
    
    #Calculating the degree
    V(g.filtered)$degree <- degree(g.filtered)
    deg <- V(g.filtered)$degree
    g.filtered.name <- V(g.filtered)$name
    degree <- data.frame(g.filtered.name, deg)
    
    # Top 5 ascending degree club and player
    top.5.degree.A <- head(degree[order(degree$deg, decreasing = FALSE), ], 5)
    top.5.degree.A.table <- data.frame("Club or Player" = top.5.degree.A$g.filtered.name, "Degree" = top.5.degree.A$deg)
  })
  
  output$top.5.degree.D.input.out <- renderTable({
    
    # Create selected vertices
    selected.clubs <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(club_buyer)]
    selected.players <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(name)]
    all.selected <- rbind(selected.clubs, selected.players)
    
    # Filter vertices by selected leagues
    sub.vertices <- V(g)[name %in% all.selected]
    
    # Create subgraph
    g.filtered <- induced_subgraph(g, sub.vertices)
    
    #Calculating the degree
    V(g.filtered)$degree <- degree(g.filtered)
    deg <- V(g.filtered)$degree
    g.filtered.name <- V(g.filtered)$name
    degree <- data.frame(g.filtered.name, deg)
    
    
    # Top 5 descending degree club and player
    top.5.degree.D <- head(degree[order(degree$deg, decreasing = TRUE), ], 5)
    
    top.5.degree.D.table <- data.frame("Club or Player" = top.5.degree.D$g.filtered.name, "Degree" = top.5.degree.D$deg)
  })
  
  # Closeness Centrality------------------------------
  output$top.5.closeness.A. <- renderTable({
    
    # Create selected vertices
    selected.clubs <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(club_buyer)]
    selected.players <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(name)]
    all.selected <- rbind(selected.clubs, selected.players)
    
    # Filter vertices by selected leagues
    sub.vertices <- V(g)[name %in% all.selected]
    
    # Create subgraph
    g.filtered <- induced_subgraph(g, sub.vertices)
    
    club <- V(g.filtered)$name
    clos <- closeness(g.filtered)
    closeness <- data.frame(club, clos)
    rownames(closeness) <- NULL
    # Top 5 ascending closeness
    top.5.A.closeness <- head(closeness[order(closeness$clos, decreasing = FALSE), ], 5)
    top.5.A.closeness.table <- data.frame("Club/Player" = top.5.A.closeness$club, "Degree" = top.5.A.closeness$clos)
  })
  
  output$top.5.closeness.D. <- renderTable({
    # Create selected vertices
    selected.clubs <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(club_buyer)]
    selected.players <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(name)]
    all.selected <- rbind(selected.clubs, selected.players)
    
    # Filter vertices by selected leagues
    sub.vertices <- V(g)[name %in% all.selected]
    
    # Create subgraph
    g.filtered <- induced_subgraph(g, sub.vertices)
    
    
    club <- V(g.filtered)$name
    clos <- closeness(g.filtered)
    closeness <- data.frame(club, clos)
    rownames(closeness) <- NULL
    # Top 5 descending Closeness
    top.5.D.closeness <- head(closeness[order(closeness$clos, decreasing = TRUE), ], 5)
    top.5.D.closeness.table <- data.frame("Club/Player" = top.5.D.closeness$club, "Degree" = top.5.D.closeness$clos)
  })  
  
  
  # Betweenness Centrality------------------------------------
  
  output$top.5.betweenness.A. <- renderTable({
    
    # Create selected vertices
    selected.clubs <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(club_buyer)]
    selected.players <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(name)]
    all.selected <- rbind(selected.clubs, selected.players)
    
    # Filter vertices by selected leagues
    sub.vertices <- V(g)[name %in% all.selected]
    
    # Create subgraph
    g.filtered <- induced_subgraph(g, sub.vertices)
    
    club <- V(g.filtered)$name
    bet <- betweenness(g.filtered)
    betweenness <- data.frame(club, bet)
    rownames(betweenness) <- NULL
    
    
    # Top 5 ascending betweness
    top.5.A.betweenness <- head(betweenness[order(betweenness$bet, decreasing = FALSE), ], 5)
    top.5.A.betweenness.table <- data.frame("Club/Player" = top.5.A.betweenness$club, "Degree" = top.5.A.betweenness$bet)
  })
  
  
  output$top.5.betweenness.D. <- renderTable({
    # Create selected vertices
    selected.clubs <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(club_buyer)]
    selected.players <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(name)]
    all.selected <- rbind(selected.clubs, selected.players)
    
    # Filter vertices by selected leagues
    sub.vertices <- V(g)[name %in% all.selected]
    
    # Create subgraph
    g.filtered <- induced_subgraph(g, sub.vertices)
    
    
    club <- V(g.filtered)$name
    bet <- betweenness(g.filtered)
    betweenness <- data.frame(club, bet)
    rownames(betweenness) <- NULL
    
    # Top 5 descending betweness
    top.5.D.betweenness <- head(betweenness[order(betweenness$bet, decreasing = TRUE), ], 5)
    top.5.D.betweenness.table <- data.frame("Club/Player" = top.5.D.betweenness$club, "Degree" = top.5.D.betweenness$bet)
    
  })
  # Eigenvector Centrality
  
  output$top.5.eigenvector.A. <- renderTable({
    
    # Create selected vertices
    selected.clubs <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(club_buyer)]
    selected.players <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(name)]
    all.selected <- rbind(selected.clubs, selected.players)
    
    # Filter vertices by selected leagues
    sub.vertices <- V(g)[name %in% all.selected]
    
    # Create subgraph
    g.filtered <- induced_subgraph(g, sub.vertices)
    
    club <- V(g.filtered)$name
    evcent <- evcent(g.filtered)$vector
    ev <- data.frame(club, evcent)
    rownames(ev) <- NULL
    
    # Top 5 ascending eigenvector
    top.5.A.eigenvector <- head(ev[order(ev$evcent, decreasing = FALSE), ], 5)
    top.5.A.eigenvector.table <- data.frame("Club/Player" = top.5.A.eigenvector$club, "Degree" = top.5.A.eigenvector$evcent)
  })
  
  output$top.5.eigenvector.D. <- renderTable({
    # Create selected vertices
    selected.clubs <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(club_buyer)]
    selected.players <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(name)]
    all.selected <- rbind(selected.clubs, selected.players)
    
    # Filter vertices by selected leagues
    sub.vertices <- V(g)[name %in% all.selected]
    
    # Create subgraph
    g.filtered <- induced_subgraph(g, sub.vertices)
    
    club <- V(g.filtered)$name
    evcent <- evcent(g.filtered)$vector
    ev <- data.frame(club, evcent)
    rownames(ev) <- NULL
    
    # Top 5 descending betweness
    top.5.D.eigenvector <- head(ev[order(ev$evcent, decreasing = TRUE), ], 5)
    top.5.D.eigenvector.table <- data.frame("Club/Player" = top.5.D.eigenvector$club, "Degree" = top.5.D.eigenvector$evcent)
  })  
  
  
  output$bipartite.graph <- renderPlot({
    
    # Create selected vertices
    selected.clubs <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(club_buyer)]
    selected.players <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(name)]
    all.selected <- rbind(selected.clubs, selected.players)
    
    # Filter vertices by selected leagues
    sub.vertices <- V(g)[name %in% all.selected]
    
    # Create subgraph
    g.filtered <- induced_subgraph(g, sub.vertices)
    
    #Calculating the degree
    V(g.filtered)$degree <- degree(g.filtered)
    deg <- V(g.filtered)$degree
    g.filtered.name <- V(g.filtered)$name
    degree <- data.frame(g.filtered.name, deg)
    
    # Get degree distribution
    degree_df <- data.frame(degree = degree(g.filtered))
    
    # Plot bar chart
    return(ggplot(degree_df, aes(x=degree)) + scale_y_log10() + geom_histogram(binwidth=3, fill = "lightgreen", color = "black") + labs(x = "Degree", y ="Count degree")) 
    
  })
  
  output$network.statistics.proj <- renderTable({
    # Create selected vertices
    selected.clubs <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(club_buyer)]
    
    # Filter vertices by selected leagues
    sub.club.vertices <- V(g.clubs)[name %in% selected.clubs]
    
    # Create subgraph
    g.club.filtered <- induced_subgraph(g.clubs, sub.club.vertices)
    
    # Calculating centrality measures of bipartite projection (clubs based on players)
    num.nodes <- vcount(g.club.filtered)
    num.edges <- ecount(g.club.filtered)
    avg.degree <- mean(degree(g.club.filtered))
    cc <- transitivity(g.club.filtered, type="average")
    avg.path.len <- mean_distance(g.club.filtered)
    diam <- diameter(g.club.filtered)
    
    B.network.statistics.table <- data.frame(Measure= c("Number of Nodes", "Number of Edges", "Average Degree", "Clustering Coefficient", "Average Path Lenght", "Diameter"), Value= c(num.nodes , num.edges , avg.degree , cc , avg.path.len , diam))
    
  })
  
  output$B.top.5.degree.A.input.out <- renderTable({
    
    # Create selected vertices
    selected.clubs <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(club_buyer)]
    
    # Filter vertices by selected leagues
    sub.club.vertices <- V(g.clubs)[name %in% selected.clubs]
    
    # Create subgraph
    g.club.filtered <- induced_subgraph(g.clubs, sub.club.vertices)
    
    #Calculating the degree
    V(g.club.filtered)$degree <- degree(g.club.filtered)
    degB <- V(g.club.filtered)$degree
    g.nameB <- V(g.club.filtered)$name
    degree <- data.frame(g.nameB, degB)
    
    # Top 5 ascending degree club
    B.top.5.degree.A <- head(degree[order(degree$degB, decreasing = FALSE), ], 5)
    B.top.5.degree.A.table <- data.frame("Club" = B.top.5.degree.A$g.nameB, "Degree" = B.top.5.degree.A$degB)
  })
  
  output$B.top.5.degree.D.input.out <- renderTable({
    
    # Create selected vertices
    selected.clubs <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(club_buyer)]
    
    # Filter vertices by selected leagues
    sub.club.vertices <- V(g.clubs)[name %in% selected.clubs]
    
    # Create subgraph
    g.club.filtered <- induced_subgraph(g.clubs, sub.club.vertices)
    
    #Calculating the degree
    V(g.club.filtered)$degree <- degree(g.club.filtered)
    degB <- V(g.club.filtered)$degree
    g.nameB <- V(g.club.filtered)$name
    degree <- data.frame(g.nameB, degB)
    
    # Top 5 ascending degree club
    B.top.5.degree.A <- head(degree[order(degree$degB, decreasing = TRUE), ], 5)
    B.top.5.degree.A.table <- data.frame("Club" = B.top.5.degree.A$g.nameB, "Degree" = B.top.5.degree.A$degB)
  })
  
  # Closeness Centrality-----------------------------------------------------------------------
  output$B.top.5.closeness.A. <- renderTable({
    
    # Create selected vertices
    selected.clubs <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(club_buyer)]
    
    # Filter vertices by selected leagues
    sub.club.vertices <- V(g.clubs)[name %in% selected.clubs]
    
    # Create subgraph
    g.club.filtered <- induced_subgraph(g.clubs, sub.club.vertices)
    
    
    B.club <- V(g.club.filtered)$name
    B.clos <- closeness(g.club.filtered)
    B.closeness <- data.frame(B.club, B.clos)
    rownames(B.closeness) <- NULL
    # Top 5 ascending closeness
    B.top.5.A.closeness <- head(B.closeness[order(B.closeness$B.clos, decreasing = FALSE), ], 5)
    B.top.5.A.closeness.table <- data.frame("Club/Player" = B.top.5.A.closeness$B.club, "Degree" = B.top.5.A.closeness$B.clos)
  })
  
  
  output$B.top.5.closeness.D. <- renderTable({
    # Create selected vertices
    selected.clubs <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(club_buyer)]
    
    # Filter vertices by selected leagues
    sub.club.vertices <- V(g.clubs)[name %in% selected.clubs]
    
    # Create subgraph
    g.club.filtered <- induced_subgraph(g.clubs, sub.club.vertices)
    
    
    
    B.club <- V(g.club.filtered)$name
    B.clos <- closeness(g.club.filtered)
    B.closeness <- data.frame(B.club, B.clos)
    rownames(B.closeness) <- NULL
    # Top 5 descending Closeness
    B.top.5.D.closeness <- head(B.closeness[order(B.closeness$B.clos, decreasing = TRUE), ], 5)
    B.top.5.D.closeness.table <- data.frame("Club/Player" = B.top.5.D.closeness$B.club, "Degree" = B.top.5.D.closeness$B.clos)
  })  
  
  
  
  # Betweenness Centrality-------------------------------------------------------------------
  
  output$B.top.5.betweenness.A. <- renderTable({
    
    # Create selected vertices
    selected.clubs <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(club_buyer)]
    
    # Filter vertices by selected leagues
    sub.club.vertices <- V(g.clubs)[name %in% selected.clubs]
    
    # Create subgraph
    g.club.filtered <- induced_subgraph(g.clubs, sub.club.vertices)
    
    B.club <- V(g.club.filtered)$name
    B.bet <- betweenness(g.club.filtered)
    B.betweenness <- data.frame(B.club, B.bet)
    rownames(B.betweenness) <- NULL
    
    
    # Top 5 ascending betweness
    B.top.5.A.betweenness <- head(B.betweenness[order(B.betweenness$B.bet, decreasing = FALSE), ], 5)
    B.top.5.A.betweenness.table <- data.frame("Club/Player" = B.top.5.A.betweenness$B.club, "Degree" = B.top.5.A.betweenness$B.bet)
  })
  
  
  output$B.top.5.betweenness.D. <- renderTable({
    
    # Create selected vertices
    selected.clubs <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(club_buyer)]
    
    # Filter vertices by selected leagues
    sub.club.vertices <- V(g.clubs)[name %in% selected.clubs]
    
    # Create subgraph
    g.club.filtered <- induced_subgraph(g.clubs, sub.club.vertices)
    
    B.club <- V(g.club.filtered)$name
    B.bet <- betweenness(g.club.filtered)
    B.betweenness <- data.frame(B.club, B.bet)
    rownames(B.betweenness) <- NULL
    
    # Top 5 descending betweness
    B.top.5.D.betweenness <- head(B.betweenness[order(B.betweenness$B.bet, decreasing = TRUE), ], 5)
    B.top.5.D.betweenness.table <- data.frame("Club/Player" = B.top.5.D.betweenness$B.club, "Degree" = B.top.5.D.betweenness$B.bet)
    
  })  
  
  
  
  # Eigenvector Centrality------------------------------------------------------------------
  output$B.top.5.eigenvector.A. <- renderTable({
    
    # Create selected vertices
    selected.clubs <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(club_buyer)]
    
    # Filter vertices by selected leagues
    sub.club.vertices <- V(g.clubs)[name %in% selected.clubs]
    
    # Create subgraph
    g.club.filtered <- induced_subgraph(g.clubs, sub.club.vertices)
    
    B.club <- V(g.club.filtered)$name
    B.evcent <- evcent(g.club.filtered)$vector
    B.ev <- data.frame(B.club, B.evcent)
    rownames(B.ev) <- NULL
    
    # Top 5 ascending eigenvector
    B.top.5.A.eigenvector <- head(B.ev[order(B.ev$B.evcent, decreasing = FALSE), ], 5)
    B.top.5.A.eigenvector.table <- data.frame("Club/Player" = B.top.5.A.eigenvector$B.club, "Degree" = B.top.5.A.eigenvector$B.evcent)
  })
  
  output$B.top.5.eigenvector.D. <- renderTable({
    # Create selected vertices
    selected.clubs <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(club_buyer)]
    
    # Filter vertices by selected leagues
    sub.club.vertices <- V(g.clubs)[name %in% selected.clubs]
    
    # Create subgraph
    g.club.filtered <- induced_subgraph(g.clubs, sub.club.vertices)
    
    B.club <- V(g.club.filtered)$name
    B.evcent <- evcent(g.club.filtered)$vector
    B.ev <- data.frame(B.club, B.evcent)
    rownames(B.ev) <- NULL
    
    # Top 5 descending eigenvector
    B.top.5.D.eigenvector <- head(B.ev[order(B.ev$B.evcent, decreasing = TRUE), ], 5)
    B.top.5.D.eigenvector.table <- data.frame("Club/Player" = B.top.5.D.eigenvector$B.club, "Degree" = B.top.5.D.eigenvector$B.evcent)
  })
  
  
  output$proj.graph <- renderPlot({
    
    # Create selected vertices
    selected.clubs <- dt.player.transfers[league %in% input$network_exploratory_1_1, unique(club_buyer)]
    
    # Filter vertices by selected leagues
    sub.club.vertices <- V(g.clubs)[name %in% selected.clubs]
    
    # Create subgraph
    g.club.filtered <- induced_subgraph(g.clubs, sub.club.vertices)
    
    #Calculating the degree
    V(g.club.filtered)$degree <- degree(g.club.filtered)
    degB <- V(g.club.filtered)$degree
    g.nameB <- V(g.club.filtered)$name
    degree <- data.frame(g.nameB, degB)
    
    # Get degree distribution
    degree_df_B <- data.frame(degree = degree(g.club.filtered))
    
    # Plot bar chart
    return(ggplot(degree_df_B, aes(x=degree)) + scale_y_log10() + geom_histogram(binwidth=3, fill = "lightgreen", color = "black") + labs(title = "Degree distribution", x = "Degree", y ="Count degree")) 
    
  })  
  
  
  
  output$content_network_stats_1 <- renderUI({
    if (input$filter_network_stats_1 == "Bipartite Graph: Clubs and Players") {
      list(
        verticalLayout(
          splitLayout(
            div(
              span(style = "font-weight: bold; font-size: 24px; display: inline-block; text-align: center;","Statistics"),
              tableOutput("network.statistics.bipartite")),
            div(
              span(style = "font-weight: bold; font-size: 24px; display: inline-block; text-align: center;","Degree Distribution"),
              plotOutput("bipartite.graph", height = "250px"))
          ),
          if(input$centrality_filter == "Degree Centrality"){
            list(
              flowLayout(
                div(
                  span(style = "font-weight: bold; font-size: 24px; display: inline-block; text-align: center;","Lowest Degree Centrality"),
                  tableOutput("top.5.degree.A.input.out")),
                div(
                  span(style = "font-weight: bold; font-size: 24px; display: inline-block; text-align: center;","Highest Degree Centrality"),
                  tableOutput("top.5.degree.D.input.out"))
              ))
          } else if(input$centrality_filter == "Closeness Centrality"){
            list(
              flowLayout(
                div(
                  span(style = "font-weight: bold; font-size: 24px; display: inline-block; text-align: center;","Lowest Closeness Centrality"),
                  tableOutput("top.5.closeness.A.")),
                div(
                  span(style = "font-weight: bold; font-size: 24px; display: inline-block; text-align: center;","Highest Closeness Centrality"),
                  tableOutput("top.5.closeness.D."))
              )) 
          } else if(input$centrality_filter == "Betweenness Centrality"){
            list(
              flowLayout(
                div(
                  span(style = "font-weight: bold; font-size: 24px; display: inline-block; text-align: center;","Lowest Betweenness Centrality"),
                  tableOutput("top.5.betweenness.A.")),
                div(
                  span(style = "font-weight: bold; font-size: 24px; display: inline-block; text-align: center;","Highest Betweenness Centrality"),
                  tableOutput("top.5.betweenness.D."))
              ))
          } else if(input$centrality_filter == "Eigenvector Centrality"){
            list(
              flowLayout(
                div(
                  span(style = "font-weight: bold; font-size: 24px; display: inline-block; text-align: center;","Lowest Eigenvector Centrality"),
                  tableOutput("top.5.eigenvector.A.")),
                div(
                  span(style = "font-weight: bold; font-size: 24px; display: inline-block; text-align: center;","Highest Eigenvector Centrality"),
                  tableOutput("top.5.eigenvector.D."))
              ))}
          
        )
      )
    }
    else if (input$filter_network_stats_1 == "Bipartite Projection: Clubs") {
      list(
        verticalLayout(
          splitLayout(
            div(
              span(style = "font-weight: bold; font-size: 24px; display: inline-block; text-align: center;","Statistics"),
              tableOutput("network.statistics.proj")),
            div(
              span(style = "font-weight: bold; font-size: 24px; display: inline-block; text-align: center;","Degree Distribution"),
              plotOutput("proj.graph", height = "250px"))
          ),
          #-----------------------------------------
          if(input$centrality_filter == "Degree Centrality"){
            list(
              flowLayout(
                div(
                  span(style = "font-weight: bold; font-size: 24px; display: inline-block; text-align: center;","Lowest Degree Centrality"),
                  tableOutput("B.top.5.degree.A.input.out")),
                div(
                  span(style = "font-weight: bold; font-size: 24px; display: inline-block; text-align: center;","Highest Degree Centrality"),
                  tableOutput("B.top.5.degree.D.input.out"))
              ))
          } else if(input$centrality_filter == "Closeness Centrality"){
            list(
              flowLayout(
                div(
                  span(style = "font-weight: bold; font-size: 24px; display: inline-block; text-align: center;","Lowest Closeness Centrality"),
                  tableOutput("B.top.5.closeness.A.")),
                div(
                  span(style = "font-weight: bold; font-size: 24px; display: inline-block; text-align: center;","Highest Closeness Centrality"),
                  tableOutput("B.top.5.closeness.D."))
              )) 
          } else if(input$centrality_filter == "Betweenness Centrality"){
            list(
              flowLayout(
                div(
                  span(style = "font-weight: bold; font-size: 24px; display: inline-block; text-align: center;","Lowest Betweenness Centrality"),
                  tableOutput("B.top.5.betweenness.A.")),
                div(
                  span(style = "font-weight: bold; font-size: 24px; display: inline-block; text-align: center;","Highest Betweenness Centrality"),
                  tableOutput("B.top.5.betweenness.D."))
              ))
          } else if(input$centrality_filter == "Eigenvector Centrality"){
            list(
              flowLayout(
                div(
                  span(style = "font-weight: bold; font-size: 24px; display: inline-block; text-align: center;","Lowest Eigenvector Centrality"),
                  tableOutput("B.top.5.eigenvector.A.")),
                div(
                  span(style = "font-weight: bold; font-size: 24px; display: inline-block; text-align: center;","Highest Eigenvector Centrality"),
                  tableOutput("B.top.5.eigenvector.D."))
              ))}
        )
      )
    }
  })
  
  
  #----------------------------------------------------------------------------------------------------   
  # Network Exploratory Statistics 2
  
  output$text_network_exploratory_page2 <- renderText({
    "Here you can explore the web of player transfers between football clubs. The main visualization on this page displays a network of football clubs, with players represented as edge labels. You can explore the network using various filters on the left panel. Select specific clubs, intervals of age and transfer fees, as well as the player type. These player types are a combination of several positions used in the dataset:  Attack (CF, RW, SS, LW, attack); Midfield (AM, DM, CM, LM, RM, midfield); Defense (CB, LB, RB, GK, defense)."
  })
  
  output$graph_network_exploratory_page2 <- renderVisNetwork({
    
    if (input$filter_position == "Attack") {
      dt.filtered <- dt.player.transfers[(short_pos %in% c("CF", "RW", "SS", "LW", "attack")), ]
    } else if (input$filter_position == "Midfield") {
      dt.filtered <- dt.player.transfers[(short_pos %in% c("AM", "DM", "CM", "LM", "RM", "midfield")), ]
    } else if (input$filter_position == "Defense") {
      dt.filtered <- dt.player.transfers[(short_pos %in% c("CB", "LB", "RB", "GK", "defense")), ]
    } else {
      # Handle the case where selected.position is not "Attack", "Midfield", or "Defense"
      dt.filtered <- NULL
    }
    
    #dt.filtered <- dt.filtered[(age >= selected.age) & (fee >= selected.fee), ]
    dt.filtered<- dt.filtered[(age >= input$filter_age[1] & age <= input$filter_age[2]) & (fee >= input$filter_fee[1] & fee <= input$filter_fee[2])]
    
    
    # Combine unique values of club_seller and club_buyer to create vertices
    all.buyer <- dt.filtered[, list(name = unique(club_buyer), type = FALSE)]
    all.seller <- dt.filtered[, list(name = unique(club_seller), type =FALSE)]
    all.vertices.df <- rbind(all.seller, all.buyer)
    all.vertices.df <- distinct(all.vertices.df, name, .keep_all = TRUE)
    
    # Create edge data frames with different directions
    edge_data_seller_to_buyer <- dt.filtered[, c("club_seller", "club_buyer", "name")]
    colnames(edge_data_seller_to_buyer) <- c("from", "to", "weight")
    edge_data_seller_to_buyer$direction <- "club_seller_to_buyer"
    
    edge_data_buyer_to_seller <- dt.filtered[, c("club_buyer", "club_seller", "name")]
    colnames(edge_data_buyer_to_seller) <- c("from", "to", "weight")
    edge_data_buyer_to_seller$direction <- "club_buyer_to_seller"
    
    # Combine the two data frames
    edge_data_both <- c(edge_data_seller_to_buyer, edge_data_buyer_to_seller)
    
    # Create the graph
    test <- graph_from_data_frame(edge_data_both, directed = TRUE, vertices = all.vertices.df)
    
    # Set edge labels to player names
    E(test)$label <- E(test)$weight
    
    # Filter vertices by selected clubs
    sub_vertices <- V(test)[name %in% input$filter_clubs]
    
    # Create subgraph
    subgraph.1 <- induced_subgraph(test, sub_vertices)
    subgraph <-  induced_subgraph(subgraph.1, V(subgraph.1)[degree(subgraph.1) > 0])
    
    E(subgraph)$length <- 400
    
    visIgraph(subgraph) %>% 
      visEdges(smooth = TRUE , color = "yellow") %>% 
      visInteraction(navigationButtons = TRUE, hideEdgesOnDrag = TRUE, tooltipDelay = 100, multiselect = TRUE) %>%
      visNodes(font = list(color = "red")) %>%
      visOptions(highlightNearest = TRUE)
    
  })
  #--------------------------------------------------------------------------------------------------  
  # Network Exploratory Statistics 3
  
  output$network_exploratory_statistics_3 <- renderText({
    "This section displays the projection of the football transfer network into the league space. It provides an overview of player movements between the selected popular European football leagues available in the filter bellow. By doing so, it is easier to analyze certain connections. The graph illustrates the connections between leagues, excluding moves inside the league. The nodes represent the leagues, and the arcs connect them. The scale outside the circle helps on concluding approximately the number of players transferred."
  })
  #--------------------------------------------------------------------------------------------------  
  # Network Exploratory Statistics 4  
  
  output$network_exploratory_statistics_4 <- renderText({
    "Bellow the bipartite network consisting of clubs and players is displayed. The nodes marked as yellow circles are football players, and the nodes marked as green squares are football clubs. Select specific nodes to better see their connections. You can zoom in, zoom out, or move nodes around for better clarity. It is possible to filter data by Season (2005-2021), League and degree of each node (i.e. if the degree selected is 3, only nodes with degree of 3 or higher will appear).
"
  })
  
  output$network_exploratory_4 <- renderVisNetwork({
    
    selected.leagues <- input$league_filter 
    selected.degree <- input$degree_filter
    selected.seasons <- input$filter_season
    
    # Get all clubs from selected leagues
    selected.clubs <- dt.player.transfers[league %in% selected.leagues, unique(club_buyer)]
    
    dt.bipartite.graph <- dt.player.transfers[(league %in% selected.leagues) & (season %in% selected.seasons) & (club_buyer %in% selected.clubs),]
    
    # Create vertices
    all.clubs <- dt.bipartite.graph[, list(name = unique(club_buyer), type = TRUE)]
    all.players <- dt.bipartite.graph[, list(name = unique(name), type=FALSE)]
    all.vertices <- rbind(all.players, all.clubs)
    
    # Creating bipartite graph
    g.bipartite.graph <- graph.data.frame(dt.bipartite.graph[, list(club_buyer, name)],
                                          directed = FALSE,
                                          vertices = all.vertices)
    
    g.bipartite.graph.induced.1 <- induced_subgraph(g.bipartite.graph, V(g.bipartite.graph)[degree(g.bipartite.graph) >= selected.degree])
    g.bipartite.graph.induced <- induced_subgraph(g.bipartite.graph.induced.1, V(g.bipartite.graph.induced.1)[degree(g.bipartite.graph.induced.1) > 0])
    
    V(g.bipartite.graph.induced)$type <- bipartite_mapping(g.bipartite.graph.induced)$type
    V(g.bipartite.graph.induced)$color <- ifelse(V(g.bipartite.graph.induced)$type, "green", "yellow")
    V(g.bipartite.graph.induced)$shape <- ifelse(V(g.bipartite.graph.induced)$type, "square", "circle")
    V(g.bipartite.graph.induced)$size <- ifelse(V(g.bipartite.graph.induced)$type, 50, 5)
    V(g.bipartite.graph.induced)$label.cex <- 0.5
    E(g.bipartite.graph.induced)$color <- "lightgrey"
      E(g.bipartite.graph.induced)$length <- 400
      
      visIgraph(g.bipartite.graph.induced) %>% 
        visEdges(smooth = TRUE) %>% 
        visInteraction(navigationButtons = TRUE, hideEdgesOnDrag = TRUE, tooltipDelay = 100) %>%
        visNodes(font = list(color = "red")) %>%
        visOptions(highlightNearest = TRUE) 
  })
  
  
  
  #-------------------------------------------------------------------------------------------------------
  # Advanced Page 1
  
  output$network_advanced_page1 <- renderText({
    "In this page of the app, it is possible for the user to select a single club in order to understand what are the clubs that have the closest transfer patterns when accounting for market value and transfer fee.  

This way, in a hypothetical application scenario, a club could understand what other clubs tend to trade with similar financial amounts and players with similar market values, which would potentialy make a future deal more likely if negetiations were started.
    
It is important to note that the values displayed are rounded to 2 decimals, so although most of them are different from each other, a lot of them might be displayed as the same value. The clubs are displayed in a descending order by their real values of similarity, so for the same displayed value on the table, the clubs that come on top have in reality a slightly higher similarity that the ones displayed beneath them."
  })
  
  output$club_similarity_table <- renderTable({
    
    # Preprocess the data to create a feature matrix for each club
    club_features <- dt.player.transfers %>%
      group_by(club_buyer) %>%
      summarize(
        avg_market_value = mean(market_value, na.rm = TRUE),
        avg_fee = mean(fee, na.rm = TRUE),
        .groups = 'drop' # To avoid the warning message about groups
      )
    
    # Normalize the numeric features
    normalize <- function(x) {
      return((x - min(x)) / (max(x) - min(x)))
    }
    
    club_features$avg_market_value <- normalize(club_features$avg_market_value)
    club_features$avg_fee <- normalize(club_features$avg_fee)
    
    # Calculate cosine similarity between clubs based on the feature matrix
    similarity_matrix <- 1 - proxy::dist(as.matrix(club_features[, -1]), method = "cosine")
    
    # Convert the 'dist' object to a matrix
    similarity_matrix <- as.matrix(similarity_matrix)
    
    # Set row and column names
    rownames(similarity_matrix) <- colnames(similarity_matrix) <- club_features$club_buyer
    
    # Define your club name
    my_club <- input$filter_advanced_clubs #FILTER
    
    
    # Rank clubs according to their similarity scores
    similar_clubs <- data.frame(
      Club = rownames(similarity_matrix),
      Similarity = similarity_matrix[my_club, ]
    ) %>%
      filter(Club != my_club) %>%
      arrange(desc(Similarity))
    
    # Display the top N most similar clubs
    top_n_similar_clubs <- 500
    head(similar_clubs, top_n_similar_clubs)
    
  })
  #-------------------------------------------------------------------------------------------------------
  
  # Advanced Page 2
  output$advanced_page2 <- renderText({
    "In this page of the app, it is possible for the user to choose multiple clubs and obtain the prediction of future transfers between them. 

To do this, we used Jaccard distance on historical data to get the most likely links between clubs in the future. Depending on the minimum threshold that is selected we will obtain more lenient or conservative prediction. A higher threshold will be more conservative and hence yield less predictions and vice versa. "
  })
  
  # Advanced Page 2
  output$advanced_page2 <- renderText({
    "In this page of the app, it is possible for the user to choose multiple clubs and obtain the prediction of future transfers between them. 

To do this, we used Jaccard distance on historical data to get the most likely links between clubs in the future. Depending on the minimum threshold that is selected we will obtain more lenient or conservative prediction. A higher threshold will be more conservative and hence yield less predictions and vice versa. "
  })
  
  graph_data <- reactive({
    
    # Combine unique values of club_seller and club_buyer to create vertices
    all.buyer <- dt.player.transfers[, list(name = unique(club_buyer), type = FALSE)]
    all.seller <- dt.player.transfers[, list(name = unique(club_seller), type =FALSE)]
    all.vertices.df <- rbind(all.seller, all.buyer)
    all.vertices.df <- distinct(all.vertices.df, name, .keep_all = TRUE)
    
    # Create edge data frames with different directions
    edge_data_seller_to_buyer <- dt.player.transfers[, c("club_seller", "club_buyer", "name")]
    colnames(edge_data_seller_to_buyer) <- c("from", "to", "weight")
    edge_data_seller_to_buyer$direction <- "club_seller_to_buyer"
    
    edge_data_buyer_to_seller <- dt.player.transfers[, c("club_buyer", "club_seller", "name")]
    colnames(edge_data_buyer_to_seller) <- c("from", "to", "weight")
    edge_data_buyer_to_seller$direction <- "club_buyer_to_seller"
    
    # Combine the two data frames
    edge_data_both <- c(edge_data_seller_to_buyer, edge_data_buyer_to_seller)
    
    # Create the graph
    test <- graph_from_data_frame(edge_data_both, directed = TRUE, vertices = all.vertices.df)
    
    # Set edge labels to player names
    E(test)$label <- E(test)$weight
    
    # Filter vertices by selected clubs
    sub_vertices <- V(test)[name %in% input$filter_clubs_advanced2]
    
    # Create subgraph
    subgraph <- induced_subgraph(test, sub_vertices)
    
    m.predicted.edges <- similarity.jaccard(subgraph) * (1 - as_adjacency_matrix(subgraph, sparse = FALSE))
    m.predicted.edges[m.predicted.edges < as.numeric(input$threshold)] <- 0  # set a threshold to filter weak edges
    g.predicted.edges <- graph_from_adjacency_matrix(m.predicted.edges, 
                                                     mode = "directed", 
                                                     weighted = TRUE)
    E(g.predicted.edges)$width <- E(g.predicted.edges)$weight * 2
    #plot(g.predicted.edges)
    #--------------------------------------------
    #Create nodes and edges data frames for visNetwork
    nodes <- data.frame(id = V(g.predicted.edges)$name, label = V(g.predicted.edges)$name)
    edges <- get.data.frame(g.predicted.edges, what = "edges")
    
    list(nodes = nodes, edges = edges)
    
  })
  output$graph_advanced_page2 <- renderVisNetwork({
    graph_data_list <- graph_data()
    visNetwork(nodes = graph_data_list$nodes, edges = graph_data_list$edges) %>%
      visIgraphLayout(layout = "layout_with_fr") %>%
      visEdges(smooth = TRUE , color = "yellow") %>% 
      visNodes(font = list(color = "green")) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
  })
  
  output$text_advanced_page3 <- renderText({
    "NETWORK EXPLORATORY TEXT AQUI"
  })
  
  output$plot_advanced_page3 <- renderVisNetwork({
    
    # Enter player name here
    my_player <- input$name_filter
    age_range <- input$age_player_3
    diff_max <- input$filter_diff
    position_p <- input$position_filter_3
    club_p <- input$filter_clubs_advanced2
    market_value_range <- c(input$filter_market_value[1], input$filter_market_value[2]) 
    fee_range <- c(input$filter_fee[1],input$filter_fee[2]) 
    
    # Preprocess the data to create a feature matrix for each player
    player.features <- dt.player.transfers %>%
      select(name, nationality, club_buyer, position, age, market_value, fee, diff) %>%
      group_by(name, .drop = TRUE) %>%
      summarize(
        nationality = first(nationality),
        club = first(club_buyer),
        position = first(position),
        age = first(age),
        market_value = first(market_value),
        fee = first(fee),
        diff = first(diff)
      )
    
    # Apply the filters
    filtered.player.features <- player.features %>%
      filter(age == age_range,
             position == position_p,
             club %in% club_p,
             market_value >= market_value_range[1] & market_value <= market_value_range[2],
             fee >= fee_range[1] & fee <= fee_range[2],
             diff <= diff_max)
    
    # Normalize the numeric features (age and market_value)
    filtered.player.features$age <- normalize(filtered.player.features$age)
    
    filtered.player.features$market_value <- normalize(filtered.player.features$market_value)
    
    filtered.player.features$fee <- normalize(filtered.player.features$fee)
    
    filtered.player.features$diff <- normalize(filtered.player.features$diff)
    
    # Define the color palette for the nodes
    color_palette <- rainbow(length(unique(filtered.player.features$nationality)))
    
    # Create a color dictionary for the nationalities
    color_dict <- setNames(color_palette, unique(filtered.player.features$nationality))
    
    # Add the color column to the data frame
    filtered.player.features$color <- color_dict[filtered.player.features$nationality]
    
    # Define the node color for the user's player
    filtered.player.features <- rbind(filtered.player.features, data.frame(name = my_player, nationality = NA, club = NA, position = position_p, age = age_range, market_value = mean(market_value_range), fee = mean(fee_range), diff = mean(c(0, diff_max)), color = "white"))
    
    # Compute the cosine similarity matrix using the proxy package
    similarity_matrix <- proxy::dist(as.matrix(filtered.player.features[, -c(1,2,3,4)]), method = "cosine")
    
    # Convert the distance matrix to a similarity matrix
    similarity_matrix <- 1 - as.matrix(similarity_matrix)
    
    # Set row and column names
    rownames(similarity_matrix) <- filtered.player.features$name
    colnames(similarity_matrix) <- filtered.player.features$name
    
    #For not having disconnected graphs
    min_similarity <- 0.1 # Set a threshold for minimum similarity
    similarity_matrix[similarity_matrix < min_similarity] <- 0.1
    
    # Remove self-loops
    diag(similarity_matrix) <- 0
    
    # Create a graph object
    g <- graph_from_adjacency_matrix(similarity_matrix, mode = "undirected", weighted = TRUE)
    
    # Define the vertex attributes
    V(g)$name <- paste(V(g)$name, " (", filtered.player.features$nationality, ")")
    V(g)$color <- filtered.player.features$color
    
    #OUTPUT
    # Plot the graph
    
    visIgraph(g) %>% 
      visEdges(smooth = TRUE) %>% 
      visInteraction(navigationButtons = TRUE, hideEdgesOnDrag = TRUE, tooltipDelay = 100) %>%   visNodes(font = list(color = "white")) %>%
      visOptions(highlightNearest=TRUE)
  })
  
  output$table_advanced_page3 <- renderTable({
    
    # Enter player name here
    my_player <- input$name_filter
    age_range <- input$age_player_3
    diff_max <- input$filter_diff
    position_p <- input$position_filter_3
    club_p <- input$filter_clubs_advanced2
    #range/slider
    market_value_range <- c(input$filter_market_value[1], input$filter_market_value[2]) 
    fee_range <- c(input$filter_fee[1],input$filter_fee[2]) 
    
    # Preprocess the data to create a feature matrix for each player
    player.features <- dt.player.transfers %>%
      select(name, nationality, club_buyer, position, age, market_value, fee, diff) %>%
      group_by(name, .drop = TRUE) %>%
      summarize(
        nationality = first(nationality),
        club = first(club_buyer),
        position = first(position),
        age = first(age),
        market_value = first(market_value),
        fee = first(fee),
        diff = first(diff)
      )
    
    # Apply the filters
    filtered.player.features <- player.features %>%
      filter(age == age_range,
             position == position_p,
             club %in% club_p,
             market_value >= market_value_range[1] & market_value <= market_value_range[2],
             fee >= fee_range[1] & fee <= fee_range[2],
             diff <= diff_max)
    
    # Normalize the numeric features (age and market_value)
    filtered.player.features$age <- normalize(filtered.player.features$age)
    
    filtered.player.features$market_value <- normalize(filtered.player.features$market_value)
    
    filtered.player.features$fee <- normalize(filtered.player.features$fee)
    
    filtered.player.features$diff <- normalize(filtered.player.features$diff)
    
    # Define the color palette for the nodes
    color_palette <- rainbow(length(unique(filtered.player.features$nationality)))
    
    # Create a color dictionary for the nationalities
    color_dict <- setNames(color_palette, unique(filtered.player.features$nationality))
    
    # Add the color column to the data frame
    filtered.player.features$color <- color_dict[filtered.player.features$nationality]
    
    # Define the node color for the user's player
    filtered.player.features <- rbind(filtered.player.features, data.frame(name = my_player, nationality = NA, club = NA, position = position_p, age = age_range, market_value = mean(market_value_range), fee = mean(fee_range), diff = mean(c(0, diff_max)), color = "white"))
    
    # Compute the cosine similarity matrix using the proxy package
    similarity_matrix <- proxy::dist(as.matrix(filtered.player.features[, -c(1,2,3,4)]), method = "cosine")
    
    # Convert the distance matrix to a similarity matrix
    similarity_matrix <- 1 - as.matrix(similarity_matrix)
    
    # Set row and column names
    rownames(similarity_matrix) <- filtered.player.features$name
    colnames(similarity_matrix) <- filtered.player.features$name
    
    #For not having disconnected graphs
    min_similarity <- 0.1 # Set a threshold for minimum similarity
    similarity_matrix[similarity_matrix < min_similarity] <- 0.1
    
    # Remove self-loops
    diag(similarity_matrix) <- 0
    
    # Create a graph object
    g <- graph_from_adjacency_matrix(similarity_matrix, mode = "undirected", weighted = TRUE)
    
    # Define the vertex attributes
    V(g)$name <- paste(V(g)$name, " (", filtered.player.features$nationality, ")")
    V(g)$color <- filtered.player.features$color
    
    
    # Create a table filtered by the same characteristics of players and group by club and nationality
    filtered.table <- filtered.player.features %>%
      filter(name != my_player) %>%
      group_by(club, nationality) %>%
      summarize(n_players = n(), .groups = 'drop')
    
    # Sort the table by n_players in descending order
    filtered.table <- filtered.table %>%
      arrange(desc(n_players))
    
    # Remove rows with no players
    filtered.table <- filtered.table[filtered.table$n_players > 0,]
    colnames(filtered.table) <- c("Club", "Nationality", "Number of Players")
    filtered.table <- head(filtered.table, 5)
    
  }) 
  
}



# Run the app
shinyApp(ui, server)
