### shiny app functions
# cards, play_shiny, char_to_seed, check_for_win, bingo
###


cards <- list(
  phone = c(
    'Am I on speaker?',
    'Are you on speaker?',
    'Can you take me off speaker?'
  ),
  
  skype = c(
    'Who uses skype?'
  ),
  
  webex = c(
    'My battery is dying',
    'webex crashed'
  ),
  
  zoom = c(
    '___, are you there?',
    '*animal noises*',
    '*child noises*',
    '*loud, painful echo/feedback*',
    '*sound of someone typing, possibly with a hammer*',
    'Can everyone go on mute?',
    'Can everyone see my screen?',
    'Can everyone stay on five more minutes?',
    'Can we take this offline?',
    'Can you email that to everyone?',
    'Can you hear me?',
    'Can you share your screen?',
    'Hello? Hello?',
    'Hey, guys, I have to jump to another call',
    'Hi, who just joined?',
    'I have a bad connection',
    'I have a hard stop at...',
    'I think there\'s a lag',
    'I think you\'re on mute',
    'I\'ll have to get back to you',
    'I\'m sorry, you cut out there',
    'It\'s still loading',
    'My connection is bad',
    'Next slide please',
    'So, *faded out* I can *unintelligible* by *cuts out*, ok?',
    'Sorry, didn\'t catch that, can you repeat?',
    'Sorry, go ahead',
    'Sorry, I was on mute',
    'Sorry, I\'m having connection issues',
    'Sorry, I\'m late... [lame excuse]',
    'Uh, ___, you\'re still sharing'
  )
)

ccbingo_shiny <- function(card, seed, wins, clicks, ...) {
  seed <- if (is.character(seed))
    char_to_seed(seed) else as.integer(seed)
  
  if (is.null(clicks)) {
    set.seed(seed)
    bingo(card, NULL, ...)
    return(invisible(NULL))
  }
  
  if (inherits(clicks, 'list'))
    clicks <- do.call('cbind', clicks)
  sel <- t(apply(clicks, 1L, function(x) {
    x <- unname(findInterval(abs(x), 0:5) * sign(x) * c(1, -1))
    if (any(x > 5 | x < 1 | identical(x, c(3, 3))))
      c(NA, NA) else x
  }))
  sel <- na.omit(sel)
  
  dup <- apply(sel, 1L, toString)
  dup <- ave(seq_along(dup), dup, FUN = length)
  sel <- sel[dup %% 2 == 1, , drop = FALSE]
  
  set.seed(seed)
  mat <- bingo(card, sel, ...)
  win <- check_for_win(mat, wins)
  
  if (nzchar(win)) {
    set.seed(seed)
    bingo(card, sel, ...)
    text(grconvertX(0.5, 'ndc'), grconvertY(0.5, 'ndc'), 'w i n n e r !',
         col = 'red', family = 'serif', srt = 30, cex = 10, xpd = NA)
  }
  
  win
}

char_to_seed <- function(player) {
  x <- gsub('[^A-z0-9 ]', '', player)
  x <- strsplit(x, '')[[1L]]
  sum(as.integer(factor(x, c(' ', letters, LETTERS, 0:9))))
}

ccbingo_app <- function() {
  path <- system.file('shiny', package = 'ccbingo')
  shiny::runApp(path, launch.browser = TRUE)
}

check_for_win <- function(m, wins = c('col', 'row', 'corner', 'diag')) {
  if (is.function(wins))
    return(wins(m))
  
  rn <- which(rowSums(m) == 5L)
  cn <- which(colSums(m) == 5L)
  lr <- all(diag(m))
  rl <- all(m[cbind(5:1, 1:5)])
  cr <- all(m[row(m) %in% c(1, 5) & col(m) %in% c(1, 5)])
  
  if (length(rn) && 'row' %in% wins)
    paste('row', rn)
  else if (length(cn) && 'col' %in% wins)
    paste('col', cn)
  else if (lr && 'diag' %in% wins)
    'major diagonal'
  else if (rl && 'diag' %in% wins)
    'minor diagonal'
  else if (cr && 'corner' %in% wins)
    'corners'
  else character(1L)
}

bingo <- function(card = '', select = NULL, title = NULL, free = NULL,
                  width = 25L, col.grid = c('grey95', 'white'),
                  col.select = 'lightyellow1', col.free = 'tomato') {
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  draw_card <- function(select = NULL, title, free, col.grid,
                        col.select, col.free) {
    plot(
      NA, type = 'n', xlim = c(0, 5), ylim = c(-5, 0),
      axes = FALSE, ann = FALSE
    )
    
    cc <- matrix(rep_len(col.grid, 25L), 5L, 5L)
    cc[select] <- col.select[1L]
    cc[3L, 3L] <- col.free[1L]
    
    co <- expand.grid(xleft = 0:4, ybottom = -(0:4), xright = 1:5, ytop = -(1:5))
    co <- c(co, list(col = cc))
    do.call('rect', co)
    
    op <- par(family = 'serif')
    text(2.5, -2.5, free %||% 'FREE', font = 2L, cex = 2)
    mt(title %||% 'CONFERENCE CALL', c(1, 5) - 0.5, cex = 1, line = 3)
    mt('BINGO', c(1, 5) - 0.5, cex = 3)
    par(family = op$family)
    
    gr <- matrix(FALSE, 5L, 5L)
    gr[3L, 3L] <- TRUE
    gr[!(cc %in% col.grid)] <- TRUE
    
    invisible(t(gr))
  }
  
  mt <- function(x, to, from = range(x), space = '', ...) {
    l <- cbind(strsplit(x, '')[[1L]], ' ')[, nzchar(space) + 1L]
    x <- seq_along(l)
    x <- (x - from[1L]) / diff(from) * diff(to) + to[1L]
    mtext(l, at = x, ...)
  }
  
  card <- sample(card, 25L, replace = length(card) < 25L)
  
  op <- par(mar = c(0, 0, 5, 0), family = 'serif')
  on.exit(par(op))
  
  gr <- draw_card(select, title, free, col.grid, col.select, col.free)
  sq <- sapply(card, function(x)
    paste0(strwrap(x, width), collapse = '\n'))
  
  co <- expand.grid(x = 1:5 - 0.5, y = -(1:5) + 0.5)
  co <- c(co, list(col = 1L, labels = replace(sq, 13L, '')))
  do.call('text', co)
  
  invisible(gr)
}
