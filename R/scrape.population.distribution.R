#' Scrape the average rate of teams being picked to win across all ESPN brackets
#'
#' @param year
#' @returns data.frame giving percentage of population picking each team in each round
#' @examples
#' populationDistribution = scrape.population.distribution(2017)
#' @export
#' @author eshayer
scrape.population.distribution = function(year) {
<<<<<<< HEAD

  `%>%` = dplyr::`%>%`

  if (!(year %in% c(2016, 2017))) {
    stop(paste0('The year ', year, ' is not available'))
  }

  url = paste0('http://games.espn.com/tournament-challenge-bracket/', year,
    '/en/whopickedwhom')
=======
  if (!(year %in% c(2016, 2017)))
    stop(paste0('The year ', year, ' is not available'))

  url = paste0('http://games.espn.com/tournament-challenge-bracket/',
               year, '/en/whopickedwhom')
>>>>>>> 72468c807f05df10f8167538b5b23276d2fe912d
  
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
