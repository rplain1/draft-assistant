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
        "jerry jeudy"
      ) ~ "Avoid",
      merge_name %in% c(
        "tank bigsby"
      ) ~ "Target",
      TRUE ~ ""
    )
  ) |>
  #mutate(position = ifelse(is.na(position), substr(pos, 1, 2), position))
  rowwise() |>
  mutate(
    upper = floor(min(underdog, nffc, rt, yahoo, adp)),
    lower = ceiling(max(underdog, nffc, rt, yahoo, adp))
  ) |>
  ungroup() |>
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
