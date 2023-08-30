
get_draft_picks <- function() {
  league_id <- '996102938634387456'
  
  my_league <- ffscrapr::ff_connect(platform = 'sleeper', 
                                    season = 2023, 
                                    league_id = league_id)
  
  
  draft_picks <- ffscrapr::ff_draft(my_league) 
  return(draft_picks)
}



get_pick_number <- function(draft_picks) {
  if(nrow(draft_picks) <= 12) 1
  else(nrow(draft_picks[12:nrow(draft_picks), ]))
}


get_round <- function(pick_number) ceiling(pick_number/12)

get_positions_drafted <- function(draft_picks) {
  draft_picks |> 
    count(pos)
}


clean_adp <- function(adp) {
  adp |> 
    janitor::clean_names() |> 
    mutate(
      team = case_when(
        team == 'JAC' ~ 'JAX',
        TRUE ~ team
      ),
      player = case_when(
        player == 'Gabe Davis' ~ 'Gabriel Davis',
        TRUE ~ player
      )
    ) |> 
    mutate(merge_name = ffscrapr::dp_clean_names(player, lowercase = TRUE))
}


get_base_adp <- function(adp, player_ids, ffc_adp, underdog_adp) {
  
  adp <- clean_adp(adp)
  
  df <- adp |> 
    left_join(
      player_ids |> 
        mutate(team = case_when(
          name == 'Dalvin Cook' ~ 'NYJ',
          merge_name == 'ezekiel elliott' ~ 'NE',
          team == 'SFO' ~ 'SF',
          team == 'KCC' ~ 'KC',
          team == 'LVR' ~ 'LV',
          team == 'GBP' ~ 'GB', 
          team == 'TBB' ~ 'TB', 
          team == 'NEP' ~ 'NE',
          team == 'NOS' ~ 'NO',
          team == 'JAC' ~ 'JAX',
          TRUE ~ team
        )) |> 
        select(-name, -position),
      by = c('merge_name', 'team')
    ) |> 
    full_join(ffc_adp, by=c('merge_name', 'team')) |> 
    filter(!is.na(db_season), !team %in% c('DST'), is.na(str_extract(pos, 'K'))) |> 
    left_join(underdog_adp |> 
                select(merge_name, team, current_adp, ovr_rank, pos_rank) |> 
                mutate(team = case_when(
                  merge_name == 'dalvin cook' ~ 'NYJ',
                  merge_name == 'ezekiel elliott' ~ 'NE',
                  merge_name == 'deandre hopkins' ~ 'TEN',
                  merge_name == 'donovan peoples-jones' ~ 'CLE',
                  TRUE ~ team
                  )
                ), 
              by = c('merge_name', 'team')
              ) |> 
    mutate(
      avg_adp = (espn *.25) + (sleeper * .25) + (nfl * .15) + (adp * .25) + (rt_sports * .10)
    ) |> 
    mutate(
      ud_rank_diff = rank - ovr_rank,
      ud_adp_diff = avg_adp - current_adp
    ) |> 
    select(
      rank
      , player
      , team
      , bye = bye.x
      , avg_adp
      , adp_formatted
      , ud_rank = ovr_rank
      , ud_rank_diff
      , ud_adp_diff
      , pos_rank
      , high
      , low
      , stdev
      , ud_adp = current_adp
      , adp
      , pos
      , espn
      , sleeper
      , nfl
      , rt_sports
      , avg
      , merge_name
      , sleeper_id
      , age
      , position
    ) 
  return(df)
}





get_df_base <- function(df, draft_picks) {
  df |>
    left_join(
      draft_picks |> 
        select(player_id, franchise_name, round, pick) |> 
        mutate(draft_position = (round*max(pick)) + pick) |> 
        select(player_id, draft_position), 
      by = c('sleeper_id' = 'player_id')
    ) |> 
    mutate(drafted = !is.na(draft_position),
           value = draft_position - avg_adp,
           target = case_when(
             merge_name %in% c(
               'jerry jeudy',
               'miles sanders',
               'jonathan taylor',
               'najee harris',
               'chris godwin',
               'george kittle',
               'james conner',
               'dandre swift',
               'michael pittman',
               'travis etienne',
               'josh jacobs',
               'kyle pitts',
               'aj dillon',
               'kadarius toney',
               'jameson williams',
               'saquon barkley',
               'cam akers',
               'christian kirk',
               'treylon burks',
               'dalton schultz',
               'adam theilen'
               
               
             ) ~ 'Avoid',
             merge_name %in% c(
               'tank bigsby',
               'dj moore',
               'christian watson',
               'diontae johnson',
               'james cook',
               'brandin cooks',
               'jaylen warren',
               'darren waller',
               'drake london',
               'pat freiermuth',
               'anthony richardson',
               'elijah moore',
               'nick chubb',
               'garrett wilson',
               'austin ekeler'
               
             ) ~ 'Target',
             TRUE ~ '')) |>
    mutate(position = ifelse(is.na(position), substr(pos, 1, 2), position)) |> 
    rowwise() |> 
    mutate(
      upper = floor(min(sleeper, espn, nfl, adp, rt_sports)),
      lower = ceiling(max(sleeper, espn, nfl, adp, rt_sports))
    ) |> 
    ungroup() |>
    arrange(avg_adp) |> 
    mutate(rank = row_number()) |> 
    group_by(position) |> 
    mutate(
      tier = round(
        lead(ifelse(drafted == FALSE & target != 'Avoid', avg_adp, 
                    ifelse(drafted == FALSE & target == 'Avoid', lead(avg_adp),
                    lead(avg_adp, 2)))) - (avg_adp)
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
    mutate(adp_formatted = paste(round_, pick, sep = '.')) |> 
    select(-round_, -pick) |> 
    relocate(position, .before = team) |> 
    mutate(
      color_ = case_when(
        drafted == TRUE ~ 'grey',
        position == 'TE' ~ '#feae58cc',
        position == 'QB' ~ '#ef74a1cc',
        position == 'RB' ~  '#8ff2cacc',
        position == 'WR' ~ '#56c9f8cc',
        TRUE ~ 'black'
      )
    ) |> 
    left_join(
      teams |> 
        mutate(team_abbr = ifelse(team_abbr == 'LA', 'LAR', team_abbr)) |> 
        select(team_abbr, ends_with('color')), 
      by=c('team'='team_abbr')
    ) |> 
    head(200) |> 
    mutate(url = glue::glue(
      "https://sleepercdn.com/content/nfl/players/thumb/{sleeper_id}.jpg"
    ),
    round = get_round(rank)) |> 
    left_join(
      teams |> select(team = team_abbr, team_logo_wikipedia),by='team'
    )
}



orange_pal <- function(x) rgb(colorRamp(c("#ffe4aa", "#ff9500"))(x), maxColorValue = 255)
blue_pal <- function(x) rgb(colorRamp(c("#aae4ff", "#0095ff"))(x), maxColorValue = 255)
red_pal <- function(x) rgb(colorRamp(c("#ffaae4", "#ff1100"))(x), maxColorValue = 255)
blue_pal <- function(x) rgb(colorRamp(c("#aae4ff", "#0095ff"))(x), maxColorValue = 255)
BuYlRd <- function(x) rgb(colorRamp(c("#7fb7d7", "#ffffbf", "#fc8d59", bias = 3))(x), maxColorValue = 255)
BuYlRd <- function(x) rgb(colorRamp(c("#000000","#004949","#009292","#ff6db6","#ffb6db",
                                               "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
                                               "#920000","#924900","#db6d00","#24ff24","#ffff6d"))(x), maxColorValue = 255)
                                               

create_reactable <- function(df_base) {
  df_base |> 
    mutate(range = lower - upper) |> 
    relocate(url, .after = position) |> 
    relocate(team_logo_wikipedia, .after = team) |> 
    relocate(target, .after = adp_formatted) |> 
    select(-c(team, drafted, value, draft_position, age, sleeper_id, merge_name,
              team_color, color_, tier, round, espn, sleeper, nfl, rt_sports, 
              pos, round, avg_adp, avg)) |> 
    reactable(
      style = list(fontSize = "17px"),
      defaultPageSize = 50,
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
        ud_adp_diff = colDef(
          name = "UD ADP Diff",
          format = colFormat(digits = 1),
          style = list(position = 'sticky', left = 0),
          headerStyle = list(position = 'sticky', left = 0)
        ),
        stdev = colDef(
          name = "Variability",
          # Render the bar charts using a custom cell render function
          cell = function(value) {
            width <- value
            bar_chart(value, width = width, fill = "#3fc1c9")
          },
          # And left-align the columns
          align = "left",
          style = list(position = 'sticky', left = 0),
          headerStyle = list(position = 'sticky', left = 0)
        ),
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
        ud_rank_diff = colDef(
          name = 'UD Rank Diff',
          style = function(value) {
            if(is.na(value)) return()
            normalized <- ((value - min(df_base$ud_rank_diff)) / (max(df_base$ud_rank_diff) - min(df_base$ud_rank_diff)))
            if(value >= 0) color <- blue_pal(normalized)
            else color <- red_pal(normalized)
            #color <- orange_pal(normalized)
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
        adp_formatted = colDef(
          name = 'Round',
          align = 'center',
          style = function(value, index) {
            #if(!is.numeric(value)) return()
            normalized <- (df_base$round[index] - min(df_base$round)) / (max(df_base$round) - min(df_base$round))
            color <- BuYlRd(normalized)
            list(background = color)
          }
        ),
        bye = colDef(
          name = 'Bye',
          cell = reactablefmtr::color_tiles(
            df_base, 
            colors = c('red', 'yellow', 'orange', 'blue', 'green', 'pink', 'cyan'),
            bias = 5,
            text_size = 35
          )
        ),
        ud_rank = colDef(name = 'UD Rank'),
        pos_rank = colDef(name = 'UD Pos Rank'),
        high = colDef(name = 'High'),
        low = colDef(name = 'Low'),
        ud_adp = colDef(name = 'UD ADP'),
        adp = colDef(name = 'ADP'),
        upper = colDef(name = 'Upper'),
        lower = colDef(name = 'Lower')
        
        
        
      )
      
      
    )
  
}


bar_chart <- function(label, width = "100%", height = "0.875rem", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "0.375rem", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}























create_gt <- function(df_base) {
  df_base  |> 
    mutate(round_ = get_round(rank)) |> 
    relocate(target, .after = adp_formatted) |> 
    relocate(bye, .after = team) |> 
    gt() |> 
    cols_hide(
      c(pos, nfl, rt_sports, avg, merge_name, sleeper_id, age,
        drafted, draft_position, color_, team_color, value, round_,
        tier)
    ) |> 
    gt::tab_header(
      title = gt::md('**Draft Assistant**')
    ) |> 
    tab_options(
      table.background.color = '#2d3649',
      table.border.bottom.color = "black",
      table.font.color = 'white',
      table_body.hlines.color = 'black',
      table_body.vlines.color = 'black'
    ) |> 
    tab_style(
      style = cell_borders(
        sides = "all", color = "black"
      ),
      locations = cells_body(
        rows = everything()
      )
    ) |> 
    gt::opt_interactive(use_compact_mode = TRUE, use_page_size_select = TRUE, page_size_values = c(10, 15, 20, 30, 50, 100)) |> 
    data_color(
      target_columns = c(rank, player, position),
      columns = color_,
      fn = function(x) df_base$color_,
    ) |> 
    data_color(
      columns = c(rank, player, position),
      palette = c('black'),
      apply_to = c("text")
    ) |> 
    nflplotR::gt_nfl_logos(columns = 'team') |> 
    data_color(
      columns = team,
      fn = function(x) df_base$team_color
    ) |> 
    cols_align(
      align = "center",
      columns = where(is.numeric)
    ) |> 
    data_color(
      columns = tier,
      palette = 'RdGy',
      reverse = TRUE
    ) |> 
    fmt_number(
      columns = c(adp, avg_adp, ud_adp_diff, ud_adp), decimals = 1
    ) |> 
    data_color(
      ud_rank_diff, 
      palette = 'YlGn',
      rows = ud_rank_diff > 0
    ) |> 
    data_color(
      ud_rank_diff, 
      palette = 'PuBu',
      rows = ud_rank_diff <= 0,
      reverse = TRUE
    ) |> 
    data_color(
      target_columns = adp_formatted,
      columns = round_,
      reverse = TRUE,
      palette = c("#000000","#004949","#009292","#ff6db6","#ffb6db",
                           "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
                           "#920000","#924900","#db6d00","#24ff24","#ffff6d")
    ) |> 
    data_color(
      target_columns = everything(),
      rows = drafted == TRUE,
      columns = target,
      palette = 'grey'
    ) |> 
    data_color(
      target_columns = everything(),
      rows = drafted == TRUE,
      columns = target,
      palette = '#333333',
      apply_to = c("text")
    ) |> 
    cols_align(
      align = "center",
      columns = c(team)
    ) |> 
    data_color(
      columns = target,
      rows = target == 'Target',
      palette = 'green'
    ) |> 
    data_color(
      columns = target,
      rows = target == 'Avoid',
      palette = 'red'
    )
}



