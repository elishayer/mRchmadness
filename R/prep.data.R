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
#'   skip.population.distribution = FALSE,
#'   skip.game.results = TRUE
#' )
#' bracket.women.2023 = data.women.2023$bracket
#' pred.538.women.2023 = data.women.2023$pred.538
#' pred.pop.women.2023 = data.women.2023$pred.pop
#' teams.women = data.women.2023$teams
#' save(bracket.women.2023, file = "data/bracket.women.2023.RData")
#' save(pred.538.women.2023, file = "data/pred.538.women.2023.RData")
#' save(pred.pop.women.2023, file = "data/pred.pop.women.2023.RData")
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
                     skip.game.results = FALSE,
                     verbose = TRUE) {

  `%>%` = dplyr::`%>%`

  league = match.arg(league)

  # Get games ----
  if (!skip.game.results) {

    if (verbose) {
      message("Scraping game results ...")
    }

    games = scrape.game.results(year = year, league = paste0(league, "s"))
  } else {
    games = NULL
  }


  # Get population distribution and bracket ----

  pred.pop = scrape.population.distribution(league)

  bracket = pred.pop %>%
    dplyr::arrange(seed) %>%
    dplyr::pull(team.id)


  return(
    list(
      games = games,
      bracket = bracket,
      pred.pop = pred.pop
    )
  )
}
