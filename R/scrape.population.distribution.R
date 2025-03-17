#' Scrape the average rate of teams being picked to win across all ESPN brackets
#'
#' @param year a numeric value of the year, between 2002 and 2017 inclusive
#' @param league either 'mens' or 'womens'
#' @return data.frame giving percentage of population picking each team in each round
#' @examples
#' populationDistribution = scrape.population.distribution(2017)
#' @export
#' @author eshayer
scrape.population.distribution = function(year, league = c('men', 'women')) {
  league = match.arg(league)
  `%>%` = dplyr::`%>%`

  challenge.id = dplyr::case_when(
    year == 2024 & league == "men" ~ 240,
    year == 2024 & league == "women" ~ 241,
    year == 2025 & league == "men" ~ 257,
    year == 2025 & league == "women" ~ 258
  )
  api = glue::glue(
    "https://gambit-api.fantasy.espn.com/apis/v1/propositions?challengeId={challenge.id}"
  )

  pred.pop.round = list()

  for (round in 1:6) {

    filter = glue::glue(
      "filter=%7B%22filterPropositionScoringPeriodIds%22%3A%7B%22value%22%3A%5B{round}%5D%7D%7D"
    )
    endpoint = glue::glue("{api}&{filter}")
    data = do.call(dplyr::bind_rows, args = jsonlite::fromJSON(endpoint)$possibleOutcomes)

    pred.pop.round[[round]] = data.frame(
      seed = sapply(
        X = data$mappings,
        FUN = function(x) {
          x |>
            dplyr::filter(type == "RANKING") |>
            dplyr::pull(value) |>
            as.integer()
        }
      ),
      team.id = sapply(
        X = data$mappings,
        FUN = function(x) {
          team_id = x |>
            dplyr::filter(type == "COMPETITOR_ID") |>
            dplyr::pull(value)
          ifelse(length(team_id) == 0, NA, team_id)
        }
      ),
      name = data$name
    )

    pred = sapply(data$choiceCounters, function(x) x$percentage)
    pred.pop.round[[round]][[paste0("round", round)]] = pred
  }

  pred.pop <- pred.pop.round[[1]]
  for (round in 2:6) {
    pred.pop <- pred.pop |>
      dplyr::left_join(pred.pop.round[[round]], by = c("seed", "team.id", "name")) |>
      dplyr::arrange(seed)
  }

  pred.pop
}
