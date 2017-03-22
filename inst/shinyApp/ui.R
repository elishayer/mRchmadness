shiny::shinyUI(shiny::fluidPage(
  shiny::titlePanel('mRchmadness: 2017'),

  shiny::sidebarLayout(
    shiny::sidebarPanel(
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
                                     'FiveThirtyEight')),
      
      shiny::numericInput('num.brackets', 'Number of Test Brackets',
                          value = 100, min = 10, max = 1000,
                          step = 10),
      
      shiny::numericInput('num.sims', 'Number of Simulations',
                          value = 1000, min = 100, max = 10000,
                          step = 100),
      
      shiny::actionButton('btn.optimal', 'Find Best Bracket',
                          class = "btn btn-primary")
    ),
  
    # Show a plot of the generated distribution
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel('Empty Bracket',
          shiny::plotOutput("bracket.empty.plot", height = 600)
        ),
        shiny::tabPanel('Filled Bracket',
          shiny::plotOutput("bracket.filled.plot", height = 600)
        )
      )
    )
  )
))
