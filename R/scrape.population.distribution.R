#' Scrape the average rate of teams being picked to win across all ESPN brackets
#'
#' @param year the numeric year to scrape
#' @param league either 'mens' or 'womens'
#' @return data.frame giving percentage of population picking each team in each round
#' @examples
#' populationDistribution = scrape.population.distribution(2017)
#' @export
#' @author eshayer
scrape.population.distribution = function(year, league = c('mens', 'womens')) {
  league = match.arg(league)
  `%>%` = dplyr::`%>%`

  url = paste0('http://games.espn.com/tournament-challenge-bracket',
               ifelse(league == 'mens', '', '-women'), '/', year,
               '/en/whopickedwhom')
  
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

  results = data.frame(names = names, probabilities = probabilities, round = as.numeric(round)) %>%
      tidyr::spread(round, probabilities) %>%
      dplyr::mutate(names = as.character(names)) %>%
      dplyr::mutate(names = ifelse(names == 'BON/LA', 'BON/UCLA',
                            ifelse(names == 'NCC/TS', 'NCC/Texas Southern',
                            ifelse(names == 'ASU/SJU', "ASU/St John's",
                            ifelse(names == 'MSM/TXSO', 'MSM/Texas Southern',
                            ifelse(names == 'MSU/UCLA', 'Michigan State/UCLA',
                            ifelse(names == 'WICH/DRKE', 'Wichita State/Drake',
                            ifelse(names == 'Florida State', 'FSU',
                            ifelse(names == 'Kansas State', 'KSU',
                            ifelse(names == 'North Carolina', 'UNC',
                            ifelse(names == 'Ohio State', 'OSU',
                            ifelse(names == 'UNC Greensboro', 'UNCG',
                            ifelse(names == 'Virginia', 'UVA',
                            ifelse(names == 'New Mexico State', 'New Mexico St',
                            ifelse(names == 'South Dakota State', 'South Dakota St', names))))))))))))))) %>%
      dplyr::mutate(names = as.factor(names))
  colnames(results) = c("name", paste0("round", 1:6))

  results
}
