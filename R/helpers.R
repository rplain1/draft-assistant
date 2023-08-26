
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
               'saquon barkley'
               
               
             ) ~ 'Avoid',
             merge_name %in% c(
               'tank bigsby',
               'dj moore',
               'christian watson',
               'diontae johnson',
               'james cook',
               'brandin cooks',
               'jaylen warren'
               
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
    head(200)
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



