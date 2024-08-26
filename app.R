library(tidyverse)
library(httr)
library(rvest)
library(gt)
library(reactable)
library(htmltools)
source('R/helpers2.R')
# Initial Load
options(reactable.theme = reactableTheme(
  color = "hsl(233, 9%, 87%)",
  backgroundColor = "hsl(233, 9%, 19%)",
  borderColor = "hsl(233, 9%, 22%)",
  stripedColor = "hsl(233, 12%, 22%)",
  highlightColor = "hsl(233, 12%, 24%)",
  inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
))
TEAMS <- nflreadr::load_teams()
TARGETS <- c('jaleel mclaughlin', 'garrett wilson', 'jonathan taylor')
AVOID <- c('javonte williams', 'sam laporta', 'dak prescott', 'josh jacobs',
           'courtland sutton', 'rhamondre stevenson', 'jerome ford')

# df_ffc <- get_ffc_adp('2qb', '2024') |> 
#   clean_ffc_adp(ff_ids)
# 
# #draft_picks <- get_draft_picks()
# df_fantasy_life_adp <- clean_fantasy_life_adp('~/Downloads/nfl_adp (1).csv', 2024)
# df_adp <- combine_ffc_fantasy_life_adp(df_ffc, df_fantasy_life_adp)
# 


# APP ------------------------------------

library(shinydashboard)

ui <- dashboardPage(skin = 'blue',
                    dashboardHeader(title = "Draft Assistant"),
                    dashboardSidebar(
                      br(),
                      fluidRow(valueBoxOutput("draft_round", width = 12)),
                      fluidRow(valueBoxOutput("draft_pick", width = 12)),
                      fluidRow(valueBoxOutput("players_taken", width = 12)),
                      fluidRow(
                        selectInput(
                          inputId = "adp_type",
                          label = "Choose ADP:",
                          choices = c("2qb", "ppr", "half-ppr"),
                          selected = "1qb"
                        )
                      ),
                      fluidRow(actionButton("refresh", "Refresh"), align = 'center')
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
                        reactableOutput("draft_table")
                      )
                    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  PICK <- reactive({get_pick_number(draft_picks())})
  PICK2 <- reactive({get_pick_number2(draft_picks())})
  
  draft_picks <- reactive({
    get_draft_picks()
  })
  
  positions_drafted <- reactive({
    get_positions_drafted(draft_picks())
  })
  
  output$draft_pick <- renderValueBox({
    valueBox("Draft Pick",
             tags$p(PICK2(), style = 'font-size: 200%'),
             color = 'light-blue'
    )
  })
  
  output$players_taken <- renderValueBox({
    valueBox("Players Taken",
             tags$p(PICK(), style = 'font-size: 200%'),
             color = 'purple'
    )
  })
  
  output$draft_round <- renderValueBox({
    valueBox("Draft Round",
             tags$p(get_round2(draft_picks()), style = 'font-size: 200%'),
             color = 'light-blue'
    )
  })
  
  output$qb <- renderValueBox({
    valueBox("QB",
             tags$p(positions_drafted() |> filter(pos == 'QB') |> pull(n), style = 'font-size: 200%'),
             color = 'red'
    )
  })
  output$te <- renderValueBox({
    valueBox("TE",
             tags$p(positions_drafted() |> filter(pos == 'TE') |> pull(n), style = 'font-size: 200%'),
             color = 'orange'
    )
  })
  output$rb <- renderValueBox({
    valueBox("RB",
             tags$p(positions_drafted() |> filter(pos == 'RB') |> pull(n), style = 'font-size: 200%'),
             color = 'green'
    )
  })
  output$wr <- renderValueBox({
    valueBox("WR",
             tags$p(positions_drafted() |> filter(pos == 'WR') |> pull(n), style = 'font-size: 200%'),
             color = 'blue'
    )
  })
  
  dat <- reactive({
    
    df_ffc <- get_ffc_adp(input$adp_type, '2024') |> 
      clean_ffc_adp(ff_ids)
    
    #draft_picks <- get_draft_picks()
    df_fantasy_life_adp <- clean_fantasy_life_adp('~/Downloads/nfl_adp (1).csv', 2024)
    df_adp <- combine_ffc_fantasy_life_adp(df_ffc, df_fantasy_life_adp)
    
    df_adp |> clean_base_table(draft_picks()) |> 
      mutate( across(c(rt, underdog, nffc, yahoo, adp), \(x) if_else(is.na(x), 999, x)))
  })
  
  output$draft_table <- renderReactable({
    
    tmp <- dat()
    tmp |> 
      create_reactable()
    
  })
  
  
  
  
  observeEvent(input$refresh, {
    
    PICK <- reactive({get_pick_number(draft_picks())})
    PICK2 <- reactive({get_pick_number2(draft_picks())})
    
    draft_picks <- reactive({
      get_draft_picks()
    })
    
    positions_drafted <- reactive({
      get_positions_drafted(draft_picks())
    })
    
    output$draft_pick <- renderValueBox({
      valueBox("Draft Pick",
               tags$p(PICK2(), style = 'font-size: 200%'),
               color = 'blue'
      )
    })
    
    output$players_taken <- renderValueBox({
      valueBox("Players Taken",
               tags$p(PICK(), style = 'font-size: 200%'),
               color = 'purple'
      )
    })
    
    output$draft_round <- renderValueBox({
      valueBox("Draft Round",
               tags$p(get_round2(draft_picks()), style = 'font-size: 200%'),
               color = 'light-blue'
      )
    })
    
    output$qb <- renderValueBox({
      valueBox("QB",
               tags$p(positions_drafted() |> filter(pos == 'QB') |> pull(n), style = 'font-size: 200%'),
               color = 'red'
      )
    })
    output$te <- renderValueBox({
      valueBox("TE",
               tags$p(positions_drafted() |> filter(pos == 'TE') |> pull(n), style = 'font-size: 200%'),
               color = 'orange'
      )
    })
    output$rb <- renderValueBox({
      valueBox("RB",
               tags$p(positions_drafted() |> filter(pos == 'RB') |> pull(n), style = 'font-size: 200%'),
               color = 'green'
      )
    })
    output$wr <- renderValueBox({
      valueBox("WR",
               tags$p(positions_drafted() |> filter(pos == 'WR') |> pull(n), style = 'font-size: 200%'),
               color = 'blue'
      )
    })
    
    dat <- reactive({
      
        df_ffc <- get_ffc_adp(input$adp_type, '2024') |> 
          clean_ffc_adp(ff_ids)
        
        #draft_picks <- get_draft_picks()
        df_fantasy_life_adp <- clean_fantasy_life_adp('~/Downloads/nfl_adp (1).csv', 2024)
        df_adp <- combine_ffc_fantasy_life_adp(df_ffc, df_fantasy_life_adp)
      
      df_adp |> clean_base_table(draft_picks()) |> 
        mutate( across(c(rt, underdog, nffc, yahoo, adp), \(x) if_else(is.na(x), 999, x)))
    })
    
    output$draft_table <- renderReactable({
      
      tmp <- dat()
      tmp |> 
        create_reactable()
      
    })
    
  })
  
  
    
  }
  
  


# Run the application 
shinyApp(ui = ui, server = server)
