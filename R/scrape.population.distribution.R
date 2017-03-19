#' Scrape the average rate of teams being picked to win across all ESPN brackets
#'
#' @param year
#' @returns data.frame giving percentage of population picking each team in each round
#' @examples
#' populationDistribution = scrape.population.distribution(2017)
scrape.population.distribution = function(year) {
  if (!(year %in% c(2016, 2017))) stop(paste0('The year ', year, ' is not available'))
  url = paste0('http://games.espn.com/tournament-challenge-bracket/', year, '/en/whopickedwhom')
  
  cells = xml2::read_html(url) %>%
    rvest::html_nodes('table.wpw-table td')
  
  names = cells %>%
    rvest::html_nodes('span.teamName') %>%
    rvest::html_text(trim = TRUE)
  
  probabilities = cells %>%
    rvest::html_nodes('span.percentage') %>%
    rvest::html_text(trim = TRUE) %>%
    substr(0, nchar(.) - 1) %>%
    as.numeric / 100
  
  round = rep(1:6, 64)
  
  data.frame(names = names, probabilities = probabilities, round = round)
}
