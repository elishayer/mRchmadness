#' Scrape the game-by-game results of the NCAA MBB seaon
#'
#' @param year a numeric value of the year, between 2002 and 2017 inclusive
#' @param league either 'mens' or 'womens'
#' @return data.frame with game-by-game results
#' @export
#' @author eshayer
scrape.game.results = function(year, league = c('mens', 'womens')) {
  league = match.arg(league)
  `%>%` = dplyr::`%>%`

  if (missing(year))
    stop('scrape.game.results: A year must be provided')
  if (!(class(year) %in% c('integer', 'numeric')))
    stop('scrape.game.results: The year must be numeric')
  if (year < 2002)
    stop('2002 is the earliest available season')
  if (year > 2023)
    warning('2023 is the latest season on which the scraper was tested')

  teams = scrape.teams(league)
  
  results = data.frame(game.id = character(0),
                       primary.id = character(0),
                       primary.score = character(0),
                       other.id = character(0),
                       other.score = character(0),
                       home = character(0),
                       location = character(0),
                       ot = character(0))
  
  for (team.id in teams$id) {
    results = rbind(results, scrape.team.game.results(year, team.id, league))
  }

  results = results %>%
    dplyr::mutate(home = ifelse(location %in% c('H', 'A'),
                                ifelse(location == 'H', TRUE, FALSE),
                                ifelse(is.na(other.id) |
                                         primary.id < other.id,
                                       TRUE, FALSE)))
  
  results = results %>%
    dplyr::transmute(game.id = game.id,
                     home.id = ifelse(home, primary.id, other.id),
                     away.id = ifelse(home, other.id, primary.id),
                     home.score = ifelse(home, primary.score, other.score),
                     away.score = ifelse(home, other.score, primary.score),
                     neutral = ifelse(location == 'N', 1, 0),
                     ot = ot)
  
  results$home.id = ifelse(is.na(results$home.id), 'NA', results$home.id)
  results$away.id = ifelse(is.na(results$away.id), 'NA', results$away.id)
  
  results = results %>%
    dplyr::filter(home.id %in% results$away.id & away.id %in% results$home.id)
  
  unique(results)
}

#' Scrape the team ids from the ESPN NCAA MBB/WBB index
#'
#' @param league either 'mens' or 'womens'
#' @return data.frame of team names and ids
#' @author eshayer
scrape.teams = function(league) {
  `%>%` = dplyr::`%>%`

  url = paste0('http://www.espn.com/', league, '-college-basketball/teams')
  
  team.links = xml2::read_html(url) %>%
    rvest::html_nodes('section.TeamLinks')
  
  name = team.links %>%
    rvest::html_nodes('.pl3 h2') %>%
    rvest::html_text(trim = TRUE)
  
  id = team.links %>%
    rvest::html_nodes('.pl3 > a') %>%
    rvest::html_attr('href') %>%
    strsplit('/') %>%
    sapply(identity) %>%
    `[`(6,)
  
  data.frame(name = name, id = id, stringsAsFactors = FALSE)
}

#' Scrape game results for a single team-year combination
#' @param year a character value representing a year
#' @param team.id an ESPN team id
#' @param league either 'mens' or 'womens'
#' @return data.frame of game data for the team-year
#' @author eshayer
scrape.team.game.results = function(year, team.id, league) {
  `%>%` = dplyr::`%>%`
  year = as.character(year)
  team.id = as.character(team.id)

  url = paste0('http://www.espn.com/', league, '-college-basketball/',
               'team/schedule/_/id/', team.id, '/season/', year)
  
  rows = xml2::read_html(url) %>%
    rvest::html_nodes('.Table__TBODY tr')

  # filter out headers and unplayed (tournament) games
  rows = rows[sapply(rows,
    function(tr) { rvest::html_nodes(tr, 'td') %>% length == 7 &
                   !rvest::html_text(tr) %>% startsWith('Date')})]

  if (length(rows) == 0) {
    return (data.frame(game.id = character(0),
                       primary.id = character(0),
                       primary.score = character(0),
                       other.id = character(0),
                       other.score = character(0),
                       home = character(0),
                       location = character(0),
                       ot = character(0)))
  }

  opponent.cells = rows %>%
    rvest::html_nodes('td:nth-child(2)')
  
  result.cells = rows %>%
    rvest::html_nodes('td:nth-child(3)')
  
  skip = result.cells %>%
    rvest::html_text(trim = TRUE) %in%
    c('Canceled', 'Postponed', 'Suspended') %>%
    which
  skip = result.cells %>%
    rvest::html_node('a') %>%
    rvest::html_attr('href') %>%
    strsplit('/') %>%
    sapply(function(row) row[5] %in% c('preview', 'onair')) %>%
    which %>%
    c(skip)
  skip = result.cells %>%
    rvest::html_node('.ml4') %>%
    rvest::html_text(trim = TRUE) %>%
    is.na %>%
    which %>%
    c(skip)
  skip = result.cells %>%
    rvest::html_node('.ml4') %>%
    rvest::html_text(trim = TRUE) %>%
    nchar %>% `==`(0) %>%
    which %>%
    c(skip)

  if (length(skip) > 0) {
    opponent.cells = opponent.cells[-skip]
    result.cells = result.cells[-skip]
  }

  if (length(opponent.cells) == 0) {
    return (data.frame(game.id = character(0),
                       primary.id = character(0),
                       primary.score = character(0),
                       other.id = character(0),
                       other.score = character(0),
                       home = character(0),
                       location = character(0),
                       ot = character(0)))
  }

  won = result.cells %>%
    rvest::html_node('.fw-bold') %>%
    rvest::html_text(trim = TRUE) == 'W'
  score = result.cells %>%
    rvest::html_node('.ml4') %>%
    rvest::html_text(trim = TRUE) %>%
    strsplit(' ') %>%
    sapply(function(row) row[1]) %>%
    strsplit('-') %>%
    sapply(identity) %>%
    t
  other = opponent.cells %>%
    rvest::html_node('.opponent-logo a') %>%
    rvest::html_attr('href') %>%
    strsplit('/') %>%
    sapply(function(row) row[6])
  neutral = opponent.cells %>%
    rvest::html_text(trim = TRUE) %>%
    endsWith('*')
  at.or.vs = opponent.cells %>%
    rvest::html_node('.pr2') %>%
    rvest::html_text(trim = TRUE)
  location = ifelse(neutral, 'N', ifelse(at.or.vs == 'vs', 'H', 'A'))
  ot = result.cells %>%
    rvest::html_node('.ml4') %>%
    rvest::html_text(trim = TRUE) %>%
    strsplit(' ') %>%
    sapply(function(row) row[2]) %>%
    ifelse(is.na(.), '', .)
  game.id = result.cells %>%
    rvest::html_node('.ml4 a') %>%
    rvest::html_attr('href') %>%
    # Extract all numbers from the URL game link (assume this is the game ID)
    gsub(".*?([0-9]+).*", "\\1", .)
  
  data.frame(game.id = game.id,
             primary.id = team.id,
             primary.score = score[matrix(c(1:nrow(score), ifelse(won, 1, 2)),
                                          ncol = 2, byrow = FALSE)],
             other.id = other,
             other.score = score[matrix(c(1:nrow(score), ifelse(won, 2, 1)),
                                        ncol = 2, byrow = FALSE)],
             location = location,
             ot = ot,
             stringsAsFactors = FALSE)
}
