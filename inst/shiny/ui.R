library('shiny')

ui <- shinyUI(
  navbarPage(
    span(tagList(icon('video'), 'zoom bingo')),
    tabPanel(
      'play bingo',
      sidebarLayout(
        sidebarPanel(
          selectizeInput(
            'type', label = 'Select the card topic', selected = 'zoom',
            choices = names(cards)
          ),
          checkboxGroupInput(
            'wins', label = 'Ways to win:',
            c('Columns' = 'col', 'Rows' = 'row',
              'Diagonals' = 'diag', 'Corners' = 'corner'),
            selected = c('col', 'row', 'diag', 'corner')
          ),
          textInput('name', label = 'Who is playing?'),
          actionButton('submit', 'Play!')
        ),
        mainPanel(plotOutput('render_card', click = 'click'))
      )
    ),
    tabPanel('source', mainPanel(imageOutput('meme')))
  )
)
