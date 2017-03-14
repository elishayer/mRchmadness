#' Scrape the average rate of teams being picked to win across all ESPN brackets
#'
#' @param year
#' @returns data.frame giving percentage of population picking each team in each round
#' @examples
#' populationDistribution = ScrapePopulationDistribution(2017)
ScrapePopulationDistribution = function(year) {
  if (!(year %in% c(2016, 2017))) stop(paste0('The year ', year, ' is not available'))
  url = paste0('http://games.espn.com/tournament-challenge-bracket/', year, '/en/whopickedwhom')
  
  cells = read_html(url) %>%
    html_nodes('table.wpw-table td')
  
  names = cells %>%
    rvest::html_nodes('span.teamName') %>%
    html_text
  
  probabilities = cells %>%
    rvest::html_nodes('span.percentage') %>%
    html_text %>%
    substr(0, nchar(.) - 1) %>%
    as.numeric %>%
    `/`(100)
  
  round = rep(0:5, 64)
  
  data.frame(names = names, probabilities = probabilities, round = round)
}
