library('shiny')
source('globals.R')

server <- shinyServer(
  function(input, output, session) {
    values <- reactiveValues(clicks = NULL)
    observeEvent(input$submit, values$clicks <- NULL)
    observeEvent(
      input$click,
      values$clicks <- rbind(values$clicks, c(input$click$x, input$click$y))
    )
    output$render_card <- renderPlot({
      card <- eventReactive(input$submit, cards[[input$type]])
      seed <- eventReactive(input$submit, char_to_seed(input$name))
      ccbingo_shiny(card(), seed(), input$wins, values$clicks)
    })
    output$meme <- renderImage(
      deleteFile = FALSE, list(src = 'img/ccbingo.png')
    )
  }
)
