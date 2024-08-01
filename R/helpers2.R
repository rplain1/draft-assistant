# TODO: fix this to be dynamic to league and year
get_draft_picks <- function() {
  league_id <- '996102938634387456'

  my_league <- ffscrapr::ff_connect(platform = 'sleeper',
                                    season = 2023,
                                    league_id = league_id)


  draft_picks <- ffscrapr::ff_draft(my_league)
  return(draft_picks)

}

# TODO: Fix this to be dynamic to league size
get_pick_number <- function(draft_picks) {
  if(nrow(draft_picks) <= 12) 1
  else(nrow(draft_picks[12:nrow(draft_picks), ]))
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

  players <- ffscrapr::dp_playerids() |>
    dplyr::filter(db_season == season, !is.na(sleeper_id)) |>
    filter(sleeper_id != '748') # wrong mike williams

  joined_table <- adp |>
    left_join(
    players,
    by = c("merge_name", "position"),
   ) |>
    select(player,
      team = team.x, merge_name, position, underdog
      , rt, nffc, yahoo, adp, sleeper_id
    )

  empty_rows <- nrow(joined_table[ is.na(joined_table$sleeper_id), ])

  if(empty_rows != 0) {
    message(glue::glue("{empty_rows} did not join"))
  }
  message("All players identified!")

  return(joined_table)

}

get_base_table <- function(adp, draft_picks) {

  teams <- nflreadr::load_teams()

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
    target = case_when(
      merge_name %in% c(
        "jerry jeudy",
        "tyreek hill"
      ) ~ "Avoid",
      merge_name %in% c(
        "tank bigsby",
        'ceedee lamb'
      ) ~ "Target",
      TRUE ~ ""
    )
  ) |>
  #mutate(position = ifelse(is.na(position), substr(pos, 1, 2), position))
  arrange(adp) |>
  mutate(rank = row_number()) |>
  group_by(position) |>
  mutate(
    tier = round(
      lead(ifelse(drafted == FALSE & target != "Avoid", adp,
        ifelse(drafted == FALSE & target == "Avoid", lead(adp),
          lead(adp, 2)
        )
      )) - (adp)
    ),
    tier = ifelse(drafted == TRUE, NA, tier),
    round_ = ceiling(rank / 12)
  ) |>
  ungroup() |>
  group_by(round_) |>
  mutate(
    pick = row_number(),
    pick = as.character(pick),
    pick = ifelse(nchar(pick) == 1, paste0("0", pick), pick)
  ) |>
  ungroup() |>
  mutate(adp_formatted = paste(round_, pick, sep = ".")) |>
  select(-round_, -pick) |>
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
    teams |>
      mutate(team_abbr = ifelse(team_abbr == "LA", "LAR", team_abbr)) |>
      select(team_abbr, ends_with("color")),
    by = c("team" = "team_abbr")
  ) |>
  head(300) |>
  mutate(
    url = glue::glue(
      "https://sleepercdn.com/content/nfl/players/thumb/{sleeper_id}.jpg"
    ),
    round = get_round(rank)
  ) |>
  left_join(
    teams |> select(team = team_abbr, team_logo_wikipedia),
    by = "team"
  )


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


draft_picks <- get_draft_picks()
df_base <- get_base_table(clean_fantasy_life_adp('~/Downloads/nfl_adp.csv', 2024), draft_picks)
df_base


df_base <- df_base |> mutate(drafted = ifelse(merge_name %in% c('ceedee lamb', 'tyreek hill', 'christian mccaffrey'), FALSE, drafted))

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
  relocate(url, .after = position) |>
  relocate(team_logo_wikipedia, .after = team) |>
  relocate(target, .after = adp_formatted) |>
  relocate(rank, .before = player) |>
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
        style = function(value, index) {
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
      round = colDef(
        name = 'Round',
        align = 'center',
        style = function(value, index) {
          #if(!is.numeric(value)) return()
          normalized <- (df_base$round[index] - min(df_base$round)) / (max(df_base$round) - min(df_base$round))
          color <- BuYlRd(normalized)
          list(background = color)
        }
      )


      )


    )
