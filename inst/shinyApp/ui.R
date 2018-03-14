shiny::shinyUI(shiny::fluidPage(
  shiny::titlePanel('mRchmadness: 2018'),

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::selectInput('sex', 'Men or Women:',
                         choices = c('Men')),

      shiny::selectInput('scoring', 'Scoring Scheme:',
                         choices = c('Traditional',
                                     'Add Seed',
                                     'Multiply Seed')),
      
      shiny::numericInput('pool.size', 'Pool Size:',
                          value = 10, min = 2, max = 100, step = 1),
      
      shiny::selectInput('criterion', 'Maximized Criterion:',
                         choices = c('Score',
                                     'Percentile',
                                     'Win Probability')),
      
      shiny::selectInput('prob.source', 'Win Probability Source',
                         choices = c('Bradley-Terry Model',
                                     'FiveThirtyEight',
                                     'KenPom')),
      
      shiny::numericInput('num.brackets', 'Number of Test Brackets',
                          value = 100, min = 10, max = 1000,
                          step = 10),
      
      shiny::numericInput('num.sims', 'Number of Simulations',
                          value = 1000, min = 100, max = 10000,
                          step = 100),
      
      shiny::numericInput('num.test', 'Number of Test Simulations',
                          value = 1000, min = 100, max = 10000,
                          step = 100),

      shiny::actionButton('btn.optimal', 'Find Best Bracket',
                          class = "btn btn-primary")
    ),
  
    # Show a plot of the generated distribution
    shiny::mainPanel(
      shiny::p(
        'Check out the ',
        shiny::a('README', href = 'https://github.com/elishayer/mRchmadness/blob/master/README.md'),
        ' on GitHub to learn more about how to use mRchmadness!'
      ),
      shiny::p(
        'Note that while there is a memory limit on the Shiny app available ',
        shiny::a('here', href = 'https://saberpowers.shinyapps.io/mRchmadness/'),
        'via shinyapps.io, you can run the Shiny app locally if you want more computational muscle.'
      ),
      shiny::p('NOTE: The bracket is men\'s only for the moment, pending the release of women\'s population picks from ESPN. We will add support for the women\'s bracket as soon as those data are available. This is as of the evening of March 12th, 2018.', style='color: red;'),
      shiny::br(),
      shiny::tabsetPanel(
        shiny::tabPanel('Empty Bracket',
          shiny::plotOutput("bracket.empty.plot", height = 600)
        ),
        shiny::tabPanel('Filled Bracket',
          shiny::plotOutput("bracket.filled.plot", height = 600)
        ),
        shiny::tabPanel('Test Results',
                        shiny::plotOutput("bracket.diagnostics", height = 600)
        )
      )
    )
  )
))
