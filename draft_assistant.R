
library(shiny)
library(tidyverse)
library(httr)
library(rvest)
library(gt)
library(bslib)
source('R/helpers.R')
# Initial Load
teams <- nflreadr::load_teams()
adp <- read_csv('~/Downloads/FantasyPros_2023_Overall_ADP_Rankings (1).csv')
player_ids <- nflreadr::load_ff_playerids()
# URL <- "https://fantasyfootballcalculator.com/api/v1/adp/ppr?teams=12&year=2023"
# 
# r <- GET(url = URL)
# ffc_adp <- jsonlite::fromJSON( rawToChar(r$content) ) |> 
#   pluck("players") |> 
#   mutate(
#     team = ifelse(name == 'Donovan Peoples-Jones', 'CLE', team),
#     merge_name = ffscrapr::dp_clean_names(name, lowercase=TRUE)
#   ) |> 
#   as_tibble()

draft_picks <- get_draft_picks()

underdog_adp <-  read_csv('~/Downloads/Underdog 4for4 ADP.csv') |> 
  janitor::clean_names() |> 
  mutate(merge_name = ffscrapr::dp_clean_names(player, lowercase = TRUE))

ffc_adp <- readRDS('data/ffc_adp.rds')

df <- get_base_adp(adp, player_ids, ffc_adp, underdog_adp)

df_base <- df |> 
  get_df_base(draft_picks = get_draft_picks()) 


#PICK <- get_pick_number(draft_picks)
COUNT <- 0

# APP ------------------------------------

library(shinydashboard)

ui <- dashboardPage(skin = 'blue',
  dashboardHeader(title = "Draft Assistant"),
  dashboardSidebar(
    br(),
    fluidRow(valueBoxOutput("draft_round", width = 12)),
    fluidRow(valueBoxOutput("draft_pick", width = 12)),
    fluidRow(box(width = 8, actionButton("refresh", "Refresh")))
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #21283c;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #21283c;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #21283c;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #21283c;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #ff0000;
                              }
              .content-wrapper {
        background-color: #151a2a;
      }
                              '))
  ),
    tags$style(".small-box.bg-red { background-color: #ef74a1cc !important; color: #000000 !important; }"),
    tags$style(".small-box.bg-orange { background-color: #feae58cc !important; color: #000000 !important; }"),
    tags$style(".small-box.bg-green { background-color: #8ff2cacc !important; color: #000000 !important; }"),
    tags$style(".small-box.bg-blue { background-color: #56c9f8cc !important; color: #000000 !important; }"),
    fluidRow(
      # A static valueBox
      valueBoxOutput("qb", width = 3),
      valueBoxOutput("rb", width = 3),
      valueBoxOutput("wr", width = 3),
      valueBoxOutput("te", width = 3),
    ),
    fluidRow(
      # Clicking this will increment the progress amount
      #splitLayout(cellWidths = c("50%", "50%"), gt_output("distPlot"), gt_output("rbPlot"))
      gt_output("draft_table")
    )
  )
)


histogramServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(mtcars[[input$var]])
    output$hist <- renderPlot({
      hist(data(), breaks = input$bins, main = input$var)
    }, res = 96)
  })
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  PICK <- reactive({get_pick_number(draft_picks())})
  
  draft_picks <- reactive({
    get_draft_picks()
  })
  
  positions_drafted <- reactive({
    get_positions_drafted(draft_picks())
  })
  
  #output$draft_pick <- renderText(PICK)
  #output$draft_round <- renderText(get_round(PICK))
  
  output$draft_pick <- renderValueBox({
    valueBox("Draft Round",
             PICK(),
             color = 'purple'
    )
  })
  
  output$draft_round <- renderValueBox({
    valueBox("Draft Pick",
             get_round(PICK()),
             color = 'purple'
    )
  })
  
  output$qb <- renderValueBox({
    valueBox("QB",
    positions_drafted() |> filter(pos == 'QB') |> pull(n),
    color = 'red'
    )
  })
  output$te <- renderValueBox({
    valueBox("TE",
             positions_drafted() |> filter(pos == 'TE') |> pull(n),
             color = 'orange'
    )
  })
  output$rb <- renderValueBox({
    valueBox("RB",
             positions_drafted() |> filter(pos == 'RB') |> pull(n),
             color = 'green'
    )
  })
  output$wr <- renderValueBox({
    valueBox("WR",
             positions_drafted() |> filter(pos == 'WR') |> pull(n),
             color = 'blue'
    )
  })
  
  dt <- reactive({
    df |> 
      get_df_base(draft_picks()) 
  })
  
  output$draft_table <- render_gt({
    dt() |> 
      create_gt()
  })
  
  
  observeEvent(input$refresh, {
    PICK <- reactive({get_pick_number(draft_picks())})
    draft_picks <- reactive({
      get_draft_picks()
    })
    draft_picks <- reactive({
      get_draft_picks()
    })
    
    positions_drafted <- reactive({
      get_positions_drafted(draft_picks())
    })
    
    #output$draft_pick <- renderText(PICK)
    #output$draft_round <- renderText(get_round(PICK))
    
    output$draft_pick <- renderValueBox({
      valueBox("Draft Round",
               PICK(),
               color = 'purple'
      )
    })
    
    output$draft_round <- renderValueBox({
      valueBox("Draft Pick",
               get_round(PICK()),
               color = 'purple'
      )
    })
    
    output$qb <- renderValueBox({
      valueBox("QB",
               positions_drafted() |> filter(pos == 'QB') |> pull(n),
               color = 'red'
      )
    })
    output$te <- renderValueBox({
      valueBox("TE",
               positions_drafted() |> filter(pos == 'TE') |> pull(n),
               color = 'orange'
      )
    })
    output$rb <- renderValueBox({
      valueBox("RB",
               positions_drafted() |> filter(pos == 'RB') |> pull(n),
               color = 'green'
      )
    })
    output$wr <- renderValueBox({
      valueBox("WR",
               positions_drafted() |> filter(pos == 'WR') |> pull(n),
               color = 'blue'
      )
    })
    
    dt <- reactive({
      df |> 
        get_df_base(draft_picks()) 
    })
    
    output$draft_table <- render_gt({
      dt() |> 
        create_gt()
    })
    
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)
