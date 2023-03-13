#' Prep data on Selection Sunday
#' 
#' This function should (hopefully) be our one-stop shop for updating data in the future.
#' It downloads the prediction file from 538, which includes all of the information we need to
#' construct the bracket. The one thing that still requires manual intervention is specifying
#' which region has the Nos. 1, 2, 3 and 4 overall seeds. 538 provides ESPN team ID numbers,
#' so we don't have to map those. But we do have to map the population distribution names.
#' Right now, the mapper runs fully automatically, but I can't guarantee it will always be
#' correct in the future. We can provide functionality for manual override in the future.
#' 
#' @param year integer
#' @param league "men" or "women"
#' @param region.rank a named list giving the overall ranking of the No. 1 seed within each region.
#'  Names must exactly match the region names in the 538 data file, so take a look at that first.
#' @param skip.population.distribution logical, should scraping population distribution be skipped?
#'  (for example, maybe it's not available yet)
#' @param skip.game.results logical, should scraping game results be script (for example, maybe
#'  you've already done this)
#' @param verbose logical, should progress be printed to console? (recommended because this shows
#'  the team mappings that were automatically made, and they may be incorrect)
#' 
#' @return a list of data to save into the package (don't forget to update documentation!)
#'
#' @examples
#' data.women.2023 = prep.data(
#'   year = 2023,
#'   league = "women",
#'   region.rank = c("Greenville 1" = 1, "Greenville 2" = 2, "Seattle 3" = 3, "Seattle 4" = 4),
#'   skip.population.distribution = TRUE
#'   skip.game.results = TRUE
#' )
#' bracket.women.2023 = data.women.2023$bracket
#' pred.538.women.2023 = data.women.2023$pred.538
#' teams.women = data.women.2023$teams
#' save(bracket.women.2023, file = "data/bracket.women.2023.RData")
#' save(pred.538.women.2023, file = "data/pred.538.women.2023.RData")
#' save(teams.women, file = "data/teams.women.RData")
#' 
#' data.men.2023 = prep.data(
#'   year = 2023,
#'   league = "men",
#'   region.rank = c("South" = 1, "Midwest" = 2, "West" = 3, "East" = 4),
#'   skip.population.distribution = FALSE,
#'   skip.game.results = TRUE
#' )
#' bracket.men.2023 = data.men.2023$bracket
#' pred.538.men.2023 = data.men.2023$pred.538
#' pred.pop.men.2023 = data.men.2023$pred.pop
#' teams.men = data.men.2023$teams
#' save(bracket.men.2023, file = "data/bracket.men.2023.RData")
#' save(pred.538.men.2023, file = "data/pred.538.men.2023.RData")
#' save(pred.pop.men.2023, file = "data/pred.pop.men.2023.RData")
#' save(teams.men, file = "data/teams.men.RData")
#'
#' @export
#' @author saberpowers
#' 
prep.data = function(year,
                     league = c("men", "women"),
                     region.rank,
                     skip.population.distribution = FALSE,
                     skip.game.results = FALSE,
                     verbose = TRUE) {

  league = match.arg(league)

  if (verbose) {
    message("Downloading 538 data ...")
  }

  url.538 = paste0(
    "https://projects.fivethirtyeight.com/march-madness-api/",
    year,
    "/fivethirtyeight_ncaa_forecasts.csv"
  )

  data.538 = read.csv(url.538) %>%
    # 538 updates their predictions every day, so make sure to grab the pre-tournament predictions
    dplyr::mutate(first.forecast.date = min(forecast_date)) %>%
    dplyr::filter(forecast_date == first.forecast.date, gender == paste0(league, "s"))

  # Identify the teams playing in the first-round play-in and collapse their data within each pair
  data.538.playin = data.538 %>%
    dplyr::filter(playin_flag == 1) %>%
    dplyr::group_by(
      team_region,
      # Extract number from team seed (e.g. 16a -> 16, 16b -> 16)
      team_seed = as.numeric(gsub(".*?([0-9]+).*", "\\1", team_seed))
    ) %>%
    dplyr::summarize(
      team_id = paste(team_id, collapse = "/"),
      team_name = paste(team_name, collapse = "/"),
      rd2_win = sum(rd2_win),
      rd3_win = sum(rd3_win),
      rd4_win = sum(rd4_win),
      rd5_win = sum(rd5_win),
      rd6_win = sum(rd6_win),
      rd7_win = sum(rd7_win),
      .groups = "drop"
    )

  # Combine the collapsed data from the play-in teams with the data from the non-play-in teams
  data.538.collapsed = data.538 %>%
    dplyr::filter(playin_flag == 0) %>%
    dplyr::mutate(team_id = as.character(team_id), team_seed = as.numeric(team_seed)) %>%
    dplyr::bind_rows(data.538.playin)
  
  bracket = data.538.collapsed %>%
    dplyr::arrange(
      # The overall seeding follows a "snake" pattern within the regions across seeds
      # For odd-numbered seeds, that seed in the No. 1 region has the highest overall seed,
      # and that seed in the No. 4 region has the lowest overall seed.
      # For even-numbered seeds, it's reversed.
      team_seed + ifelse((team_seed %% 2) == 1, +1, -1) * region.rank[team_region] / 10
    ) %>%
    dplyr::pull(team_id)

  pred.538 = data.538.collapsed %>%
    dplyr::select(
      name = team_name,
      round1 = rd2_win,
      round2 = rd3_win,
      round3 = rd4_win,
      round4 = rd5_win,
      round5 = rd6_win,
      round6 = rd7_win
    )

  # Use the 538-provided ESPN team IDs to update the name.538 mapping in the teams data object

  teams.map.538 = data.538 %>%
    dplyr::transmute(id = as.character(team_id), name.538.new = team_name)

  teams = eval(parse(text = paste("mRchmadness::teams", league, sep = "."))) %>%
    dplyr::mutate(name = as.character(name))

  teams = teams %>%
    dplyr::left_join(teams.map.538, by = "id") %>%
    dplyr::transmute(name, id, name.pop, name.538 = dplyr::coalesce(name.538.new, name.538))


  if (!skip.population.distribution) {

    if (verbose) {
      message("Scraping population distribution and mapping teams...")
    }

    pred.pop = scrape.population.distribution(year = year, league = paste0(league, "s"))

    teams.map.pop = map.population.team.names(pop.names = pred.pop$name, teams = teams)

    teams = teams %>%
      dplyr::left_join(dplyr::rename(teams.map.pop$full.map, name.pop.new = name), by = "id") %>%
      dplyr::transmute(name, id, name.pop = dplyr::coalesce(name.pop.new, name.pop), name.538)
    
    if (verbose) {
      message("Automatically made the following mappings:")
      print(teams.map.pop$auto.map)
    }

  } else {
    pred.pop = NULL
  }


  if (!skip.game.results) {

    if (verbose) {
      message("Scraping game results ...")
    }

    games = scrape.game.results(year = year, league = paste0(league, "s"))
  } else {
    games = NULL
  }


  return(
    list(
      bracket = bracket,
      pred.538 = pred.538,
      pred.pop = pred.pop,
      teams = teams,
      games = games
    )
  )
}
