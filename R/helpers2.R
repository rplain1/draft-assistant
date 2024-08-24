library(httr)
library(jsonlite)
library(tidyverse)

TEAMS <- nflreadr::load_teams()
TARGETS <- c('jaleel mclaughlin')
AVOID <- c('javonte williams')



# TODO: fix this to be dynamic to league and year
get_draft_picks <- function(league_id = '1045662785243389952', year = 2024) {
  
  my_league <- ffscrapr::ff_connect(platform = 'sleeper',
                                    season = year,
                                    league_id = league_id)


  draft_picks <- ffscrapr::ff_draft(my_league)
  return(draft_picks)

}

# TODO: Fix this to be dynamic to league size
get_pick_number <- function(draft_picks, team_size = 10) {
  if(nrow(draft_picks) <= team_size) 1
  else(nrow(draft_picks[team_size:nrow(draft_picks), ]))
}

get_round <- function(pick_number) ceiling(pick_number/12)

get_positions_drafted <- function(draft_picks) {

  draft_picks |>
    count(pos)

}

clean_fantasy_life_adp <- function(path, season = ffl::most_recent_season()) {

  adp <- read_csv(path) |>
  janitor::clean_names() |>
  mutate(
    position = dplyr::if_else(player == "Taysom Hill", "QB", position)
  ) |>
  mutate(
    player = case_when(
      player == "Gabe Davis" ~ "Gabriel Davis",
      player == "Hollywood Brown" ~ "Marquise Brown",
      TRUE ~ player
  ),
  merge_name = ffscrapr::dp_clean_names(player_name = player, lowercase = TRUE, convert_lastfirst = TRUE, use_name_database = TRUE)
  )

  players <- nflreadr::load_ff_playerids() |>
    dplyr::filter(db_season == season, !is.na(sleeper_id)) |>
    filter(sleeper_id != '748') # wrong mike williams

  joined_table <- adp |>
    left_join(
    players,
    by = c("merge_name", "position"),
   ) |>
    select(player,
      team = team.x, merge_name, position, underdog
      , rt, nffc, yahoo, adp, sleeper_id, sleeper, adp
    )

  empty_rows <- nrow(joined_table[ is.na(joined_table$sleeper_id) & joined_table$position %in% c('RB', 'WR','TE', 'QB'), ])

  if(empty_rows != 0) {
    message(glue::glue("{empty_rows} did not join"))
  }
  else {
    message("All players identified!")
  }
  

  return(joined_table)

}




clean_base_table <- function(df_base) {

  df_base |>
    rowwise() |>
    mutate(
      across(c(rt, underdog, nffc, yahoo, adp), \(x) if_else(is.na(x), 999, x)),
      upper = floor(min(underdog, nffc, rt, yahoo, adp)),
      lower = ceiling(max(underdog, nffc, rt, yahoo, adp))
    ) |>
    ungroup() |>
    group_by(position) |>
    arrange(adp) |>
    mutate(
      pos_rank = row_number(),

      )

}





get_ffc_adp <- function(format, year, teams = "12", position = "all") {
  url <- glue::glue("https://fantasyfootballcalculator.com/api/v1/adp/{format}?teams={teams}&year={year}&position={position}")
  # Fetch the data from the URL
  response <- GET(url)
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Parse the JSON content
    data <- content(response, as = "text")
    json_data <- fromJSON(data, flatten = TRUE)
  } else {
    cat("Error: ", status_code(response), "\n")
  }
  
  json_data$players |>
    as_tibble() |>
    mutate(season = year)
  
}

clean_ff_ids <- function(ff_ids) {
  
  ff_ids |> filter(is.na(
    case_when(
      merge_name == "mike williams" & gsis_id == "00-0033536" ~ TRUE,
      merge_name == "steve smith" & gsis_id == "00-0020337" ~ TRUE,
      merge_name == "zach miller" & gsis_id == "00-0027125" ~ TRUE,
      TRUE ~ NA)
  )
  )
}

ff_ids <- nflreadr::load_ff_playerids() |> 
  clean_ff_ids()

#player_stats <- nflreadr::load_player_stats(seasons = 2023, stat_type = c('offense'))


clean_ffc_adp <- function(ffc_adp, ff_ids) {

  ffc_adp$join_name <- nflreadr::clean_player_names(ffc_adp$name)
  ffc_adp |>
    mutate(
      join_name = case_when(
        join_name == "Mike Vick" ~ "Michael Vick",
        str_detect(join_name, "Jr") ~ str_remove(join_name, " Jr"),
        TRUE ~ join_name
      ),
      join_name = str_to_lower(join_name)
    ) |>
    inner_join(ff_ids |>
                 select(gsis_id, merge_name, sleeper_id, position, pfr_id) |>
                 filter(!is.na(gsis_id)), by = c("join_name" = "merge_name", "position" = "position")) |>
    arrange(adp) |> 
    mutate(
      rank = row_number()
    ) |> 
    group_by(season, position) |>
    arrange(adp) |>
    mutate(pos_rank = row_number()) |>
    ungroup()

}


clean_base_table <- function(adp, draft_picks) {
  
  adp |>
    left_join(
      draft_picks |>
        #mutate(draft_position = (round * max(pick)) + pick) |>
        mutate(draft_position = row_number()) |>
        select(player_id, draft_position),
      by = c("sleeper_id" = "player_id")
    ) |>
    mutate(
      drafted = !is.na(draft_position),
      value = draft_position - adp,
      # TODO look at using sleeper ids
      target = case_when(
        join_name %in% AVOID ~ "Avoid",
        join_name %in% TARGETS ~ "Target",
        TRUE ~ ""
      )
    ) |>
  relocate(position, .before = team) |>
    mutate(
      color_ = case_when(
        drafted == TRUE ~ "grey",
        position == "TE" ~ "#feae58cc",
        position == "QB" ~ "#ef74a1cc",
        position == "RB" ~ "#8ff2cacc",
        position == "WR" ~ "#56c9f8cc",
        TRUE ~ "black"
      )
    ) |>
    left_join(
      TEAMS |>
        mutate(team_abbr = ifelse(team_abbr == "LA", "LAR", team_abbr)) |>
        select(team_abbr, ends_with("color")),
      by = c("team" = "team_abbr")
    ) |>
    mutate(
      url = glue::glue(
        "https://sleepercdn.com/content/nfl/players/thumb/{sleeper_id}.jpg"
      ),
      round = map_dbl(.x = draft_position, get_round)
    ) |>
    left_join(
      TEAMS |> select(team = team_abbr, team_logo_wikipedia),
      by = "team"
    )
  
  
}

combine_ffc_fantasy_life_adp <- function(df_ffc, df_fantasy_life) {
  df_ffc |> 
    left_join(
      df_fantasy_life |> 
        select(-player, -team, -merge_name, -position, -adp),
      by = 'sleeper_id'
    )
}

df_ffc <- get_ffc_adp('2qb', '2024') |> 
  clean_ffc_adp(ff_ids)

draft_picks <- get_draft_picks()
df_fantasy_life_adp <- clean_fantasy_life_adp('~/Downloads/nfl_adp (1).csv', 2024)
df_adp <- combine_ffc_fantasy_life_adp(df_ffc, df_fantasy_life_adp)

df_adp

df_base <- df_adp |> clean_base_table(draft_picks)
df_base <- df_base |> 
  mutate( across(c(rt, underdog, nffc, yahoo, adp), \(x) if_else(is.na(x), 999, x)),
)
library(reactable)


orange_pal <- function(x) rgb(colorRamp(c("#ffe4aa", "#ff9500"))(x), maxColorValue = 255)
blue_pal <- function(x) rgb(colorRamp(c("#aae4ff", "#0095ff"))(x), maxColorValue = 255)
red_pal <- function(x) rgb(colorRamp(c("#ffaae4", "#ff1100"))(x), maxColorValue = 255)
blue_pal <- function(x) rgb(colorRamp(c("#aae4ff", "#0095ff"))(x), maxColorValue = 255)
BuYlRd <- function(x) rgb(colorRamp(c("#7fb7d7", "#ffffbf", "#fc8d59", bias = 3))(x), maxColorValue = 255)
BuYlRd <- function(x) rgb(colorRamp(c("#000000","#004949","#009292","#ff6db6","#ffb6db",
                                      "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
                                      "#920000","#924900","#db6d00","#24ff24","#ffff6d"))(x), maxColorValue = 255)



reactable(
  df_base
)





df_base |>
  rename(player = name) |> 
  relocate(url, .after = position) |>
  relocate(team_logo_wikipedia, .after = team) |>
  relocate(target, .after = adp_formatted) |>
  relocate(rank, .before = player) |>
  select(-c(player_id, season, join_name, ends_with('id'))) |> 
  reactable(
    style = list(fontSize = "17px"),
    defaultPageSize = 10,
    striped = TRUE,
    highlight = TRUE,
    searchable = TRUE,
    columns = list(
      # avg_adp = colDef(
      #   name = "Value",
      #   format = colFormat(digits = 1),
      #   style = function(x) {
      #     if(is.na(x)) return()
      #     if (x > 0) {
      #       color <- make_color_pallete(viridisLite::viridis(20))(1/x)
      #     } else if (x <= 0) {
      #       color <- 'red'
      #     } else {
      #       color <- 'blue'
      #     }
      #     list(background = color)
      #   }
      # ),
      # ud_adp_diff = colDef(
      #   name = "UD ADP Diff",
      #   format = colFormat(digits = 1),
      #   style = list(position = 'sticky', left = 0),
      #   headerStyle = list(position = 'sticky', left = 0)
      # ),
      # stdev = colDef(
      #   name = "Variability",
      #   # Render the bar charts using a custom cell render function
      #   cell = function(value) {
      #     width <- value
      #     bar_chart(value, width = width, fill = "#3fc1c9")
      #   },
      #   # And left-align the columns
      #   align = "left",
      #   style = list(position = 'sticky', left = 0),
      #   headerStyle = list(position = 'sticky', left = 0)
      # ),
      rank = colDef(
        name = "ADP",
        style = function(index) {
          color <- "white"  # Default color
          if (df_base$drafted[index] == TRUE) {
            color <- "grey"
          } else if (df_base$position[index] == "RB") {
            color <- "#8ff2cacc"
          } else if (df_base$position[index] == "WR") {
            color <- "#56c9f8cc"
          } else if (df_base$position[index] == "QB") {
            color <- "#ef74a1cc"
          } else if (df_base$position[index] == "TE") {
            color <- "#feae58cc"
          }
          list(background = color, color = 'black')
        },

      ),
      position = colDef(
        name = "Position",
        align = 'center',
        style = function(value, index) {
          color <- "white"  # Default color
          if (df_base$drafted[index] == TRUE) {
            color <- "grey"
          } else if (df_base$position[index] == "RB") {
            color <- "#8ff2cacc"
          } else if (df_base$position[index] == "WR") {
            color <- "#56c9f8cc"
          } else if (df_base$position[index] == "QB") {
            color <- "#ef74a1cc"
          } else if (df_base$position[index] == "TE") {
            color <- "#feae58cc"
          }
          list(background = color, color = 'black')
        }
      ),
      player = colDef(
        name = "Player",
        style = function(value, index) {
          color <- "white"  # Default color
          if (df_base$drafted[index] == TRUE) {
            color <- "grey"
          } else if (df_base$position[index] == "WR") {
            color <- "#56c9f8cc"
          } else if (df_base$position[index] == "QB") {
            color <- "#ef74a1cc"
          } else if (df_base$position[index] == "TE") {
            color <- "#feae58cc"
          } else if (df_base$position[index] == "RB") {
            color <- "#8ff2cacc"
          }
          list(background = color, color = 'black')
        }
      ),
      url = colDef(
        name = 'Headshot',
        cell = reactablefmtr::embed_img(height = 166/2.8, width = 250/3),
        style = function(value, index) {
          if(df_base$drafted[index] == TRUE) color <- 'grey'
          else color <- NULL
          list(background = color)
        }
      ),
      team_logo_wikipedia   = colDef(
        name = 'Team',
        cell = reactablefmtr::embed_img(height = 50, width = 75),
        style = function(value, index) {
          if(df_base$drafted[index] == TRUE) color <- 'grey'
          else color <- df_base$team_color[index]
          list(background = color)
        }
      ),
      target = colDef(
        name = 'Target',
        align = 'center',
        cell = function(value) {
          if (value  == 'Avoid' ) {
            shiny::icon("warning", class = "fas fa-3x", style = "color: red")
          } else if (value == 'Target') {
            shiny::icon("thumbs-up", class = 'fas fa-3x', style = "color: cyan")
          } else value
        }
      ),
      team = colDef(show = FALSE),
      times_drafted = colDef(show = FALSE),
      team_color = colDef(show = FALSE),
      color_ = colDef(show = FALSE),
      drafted = colDef(show = FALSE)
      # round = colDef(
      #   name = 'Round',
      #   align = 'center',
      #   style = function(value, index) {
      #     #if(!is.numeric(value)) return()
      #     normalized <- (df_base$round[index] - min(df_base$round)) / (max(df_base$round) - min(df_base$round))
      #     color <- BuYlRd(normalized)
      #     list(background = color)
      #   }
      # )


      )


    )










