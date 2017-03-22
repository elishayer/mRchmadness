bt.prob.matrix = mRchmadness::bradley.terry(mRchmadness::games.2017)

criterion.map = matrix(c('percentile', 'score', 'win'),
                       dimnames = list(c('Percentile',
                                         'Score',
                                         'Win Probability')))

shiny::shinyServer(function(input, output) {
  scoring = shiny::eventReactive(input$btn.optimal, {input$scoring})
  pool.size = shiny::eventReactive(input$btn.optimal, {input$pool.size})
  criterion = shiny::eventReactive(input$btn.optimal, {input$criterion})
  prob.source = shiny::eventReactive(input$btn.optimal, {input$prob.source})
  num.brackets = shiny::eventReactive(input$btn.optimal, {input$num.brackets})
  num.sims = shiny::eventReactive(input$btn.optimal, {input$num.sims})
  
  output$bracket.empty.plot = shiny::renderPlot({
    mRchmadness::draw.bracket(mRchmadness::bracket.2017)
  })
  
  output$bracket.filled.plot = shiny::renderPlot({
    bonus.round = 2 ^ (0:5)
    bonus.seed = rep(0, 16)
    bonus.combine = 'add'

    if (scoring() %in% c('Add Seed', 'Multiply Seed')) {
      bonus.seed = 1:16
    }
    
    if (scoring() == 'Multiply Seed') {
      bonus.combine = 'multiply'
    }
    
    # only bt for now, until 538 is ready to go
    prob.matrix = bt.prob.matrix
    
    bracket.filled = mRchmadness::find.bracket(bracket.empty =
                                                 mRchmadness::bracket.2017,
                                  probability.matrix = prob.matrix,
                                  num.candidates = num.brackets(),
                                  num.sims = num.sims(),
                                  criterion = criterion.map[criterion(),],
                                  pool.size = pool.size(),
                                  bonus.round = bonus.round,
                                  bonus.seed = bonus.seed,
                                  bonus.combine = bonus.combine)
    
    mRchmadness::draw.bracket(mRchmadness::bracket.2017, bracket.filled)
  })
})
