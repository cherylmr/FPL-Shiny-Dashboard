# refresh_data.R
# Pulls data from the (unofficial) FPL API and caches it in data/*.rds.
# Run this whenever you want fresh numbers (e.g. after each gameweek):
#   source("refresh_data.R")
# The Shiny app only reads the cached files, so it never calls the API itself.

library(httr2)
library(dplyr)
library(purrr)
library(tibble)

setwd("C:/Users/chery/OneDrive/Desktop/RStudio/FPL Shiny App")

fpl_get <- function(path) {
  request("https://fantasy.premierleague.com/api/") %>%
    req_url_path_append(path) %>%
    req_user_agent("fpl-shiny-dashboard (personal project)") %>%
    req_retry(max_tries = 3) %>%
    req_perform() %>%
    resp_body_json(simplifyVector = TRUE)
}

dir.create("data", showWarnings = FALSE)

# ---- 1. Core tables (one call) ---------------------------------------------
bootstrap <- fpl_get("bootstrap-static/")

teams <- as_tibble(bootstrap$teams) %>%
  select(team = id, team_name = name, team_short = short_name)

positions <- as_tibble(bootstrap$element_types) %>%
  select(element_type = id, position = singular_name_short)

events <- as_tibble(bootstrap$events) %>%
  select(id, name, finished, is_current, is_next)

# These come back from the API as text
text_numeric <- c(
  "selected_by_percent", "form", "points_per_game",
  "influence", "creativity", "threat", "ict_index",
  "expected_goals", "expected_assists", "expected_goal_involvements"
)

players <- as_tibble(bootstrap$elements) %>%
  mutate(across(any_of(text_numeric), as.numeric)) %>%
  transmute(
    id, web_name, team, element_type, status, news,
    price       = now_cost / 10,
    price_start = (now_cost - cost_change_start) / 10,
    total_points, minutes, goals_scored, assists, bonus,
    selected_by_percent, form, points_per_game,
    influence, creativity, threat, ict_index,
    expected_goals, expected_assists, expected_goal_involvements,
    transfers_in_event, transfers_out_event
  ) %>%
  left_join(teams, by = "team") %>%
  left_join(positions, by = "element_type")

# ---- 2. Fixtures (one call) -------------------------------------------------
fixtures <- as_tibble(fpl_get("fixtures/")) %>%
  select(id, event, kickoff_time, finished,
         team_h, team_a, team_h_difficulty, team_a_difficulty)

# ---- 3. Per-gameweek player stats (one call per finished gameweek) ---------
get_gw_stats <- function(gw) {
  Sys.sleep(0.5)  # be polite to the API
  live <- fpl_get(sprintf("event/%d/live/", gw))
  bind_cols(
    tibble(element = live$elements$id, gameweek = gw),
    as_tibble(live$elements$stats)
  )
}

finished_gws <- events$id[events$finished]
gw_stats <- map_dfr(finished_gws, get_gw_stats)

# ---- 4. Metadata ------------------------------------------------------------
next_gw <- if (any(events$is_next)) events$id[events$is_next][1] else max(finished_gws) + 1

meta <- list(
  refreshed    = Sys.time(),
  finished_gws = length(finished_gws),
  next_gw      = next_gw
)

saveRDS(players,  "data/players.rds")
saveRDS(teams,    "data/teams.rds")
saveRDS(fixtures, "data/fixtures.rds")
saveRDS(gw_stats, "data/gw_stats.rds")
saveRDS(meta,     "data/meta.rds")

message("Done: ", nrow(players), " players, ", length(finished_gws),
        " finished gameweeks. Next gameweek: ", next_gw)
