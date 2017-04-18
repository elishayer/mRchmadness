bt.men = mRchmadness::bradley.terry(mRchmadness::games.men.2017)
bt.women = mRchmadness::bradley.terry(mRchmadness::games.women.2017)

criterion.map = matrix(c('percentile', 'score', 'win'),
                       dimnames = list(c('Percentile',
                                         'Score',
                                         'Win Probability')))

shiny::shinyServer(function(input, output) {
  sex = shiny::eventReactive(input$btn.optimal, {input$sex})
  scoring = shiny::eventReactive(input$btn.optimal, {input$scoring})
  pool.size = shiny::eventReactive(input$btn.optimal, {input$pool.size})
  criterion = shiny::eventReactive(input$btn.optimal, {input$criterion})
  prob.source = shiny::eventReactive(input$btn.optimal, {input$prob.source})
  num.brackets = shiny::eventReactive(input$btn.optimal, {input$num.brackets})
  num.sims = shiny::eventReactive(input$btn.optimal, {input$num.sims})
  num.test = shiny::eventReactive(input$btn.optimal, {input$num.test})

  output$bracket.empty.plot = shiny::renderPlot({
    bracket = eval(parse(text = paste('mRchmadness::bracket',
                                      tolower(sex()), '2017', sep = '.')))
    mRchmadness::draw.bracket(bracket, league = tolower(sex()))
  })

  filled.bracket = shiny::reactive({
    bonus.round = 2 ^ (0:5)
    bonus.seed = rep(0, 16)
    bonus.combine = 'add'

    if (scoring() %in% c('Add Seed', 'Multiply Seed')) {
      bonus.seed = 1:16
    }

    if (scoring() == 'Multiply Seed') {
      bonus.combine = 'multiply'
    }

    empty = eval(parse(text = paste('mRchmadness::bracket',
                                    tolower(sex()), '2017', sep = '.')))

    prob.matrix = NULL
    prob.src = NULL
    league = tolower(sex())

    if (prob.source() == 'Bradley-Terry Model') {
      prob.matrix = eval(parse(text = paste('bt', tolower(sex()), sep = '.')))
    } else if (prob.source() == 'FiveThirtyEight') {
      prob.src = '538'
    }

    progress = shiny::Progress$new(max = num.sims())
    on.exit(progress$close())
    progress$set(message = 'Finding your bracket')

    mRchmadness::find.bracket(bracket.empty = empty,
                              prob.matrix = prob.matrix,
                              prob.source = prob.src,
                              league = league,
                              num.candidates = num.brackets(),
                              num.sims = num.sims(),
                              criterion = criterion.map[criterion(),],
                              pool.size = pool.size(),
                              bonus.round = bonus.round,
                              bonus.seed = bonus.seed,
                              bonus.combine = bonus.combine,
                              shiny.progress = progress)
  })

  output$bracket.filled.plot = shiny::renderPlot({
    empty = eval(parse(text = paste('mRchmadness::bracket',
                                    tolower(sex()), '2017', sep = '.')))

    mRchmadness::draw.bracket(empty, filled.bracket())
  })

  output$bracket.diagnostics = shiny::renderPlot({
    bonus.round = 2 ^ (0:5)
    bonus.seed = rep(0, 16)
    bonus.combine = 'add'

    if (scoring() %in% c('Add Seed', 'Multiply Seed')) {
      bonus.seed = 1:16
    }

    if (scoring() == 'Multiply Seed') {
      bonus.combine = 'multiply'
    }

    empty = eval(parse(text = paste('mRchmadness::bracket',
                                    tolower(sex()), '2017', sep = '.')))

    prob.matrix = NULL
    prob.src = NULL
    league = tolower(sex())

    if (prob.source() == 'Bradley-Terry Model') {
      prob.matrix = eval(parse(text = paste('bt', tolower(sex()), sep = '.')))
    } else if (prob.source() == 'FiveThirtyEight') {
      prob.src = '538'
    }

    progress = shiny::Progress$new(max = num.sims())
    on.exit(progress$close())
    progress$set(message = 'Testing your bracket')

    results = mRchmadness::test.bracket(empty, filled.bracket(),
                                        prob.matrix = prob.matrix,
                                        prob.source = prob.src,
                                        pool.source = 'pop',
                                        league = league,
                                        year = 2017,
                                        pool.size = pool.size(),
                                        num.sims = num.test(),
                                        bonus.round = bonus.round,
                                        bonus.seed = bonus.seed,
                                        bonus.combine = bonus.combine,
                                        shiny.progress = progress)

    hist(results$percentile,
         xlab = 'Percentile',
         main = paste0('Probability of winning pool: ', round(100 * mean(results$win), 1), '%'))
  })
})
