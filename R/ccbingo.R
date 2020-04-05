### ccbingo functions
# ccbingo, char_to_seed, ccbingo_app
#
# unexported
# ccbingo_shiny, check_for_win, bingo
###


#' Conference call bingo
#' 
#' @description
#' Play conference call bingo locally or with \code{\link[shiny]{shiny}}.
#' 
#' \code{ccbingo} runs in a regular \code{R} session and prompts the user for
#' each click using \code{\link{locator}}. \code{ccbingo:::ccbingo_shiny} has
#' similar functionality but requires a list of click coordinates.
#' 
#' \code{char_to_seed} creates a simple string-to-seed mapping which allows
#' cards to be reproduced from a player name, for example.
#' 
#' \code{cards} is a list of built-in phrases to use for cards.
#' 
#' @param card a vector of character strings to make random cards; strings
#' are randomly selected from \code{card} without replacement but recycled
#' as needed to draw a 5x5 grid
#' @param seed an integer passed to \code{\link{set.seed}} to reproduce cards;
#' alternatively, a character string to be converted to a seed with
#' \code{char_to_seed}
#' @param wins ways to win the game, any combination of \code{"col"},
#' \code{"row"}, \code{"corner"}, or \code{"diag"}; alternatively, a function
#' to determine wins returning \code{''} for no win and a non-zero length
#' string for wins
#' @param ... additional arguments passed to \code{ccbingo:::bingo} such as
#' \code{title} (card title), \code{free} (free space text), \code{width}
#' (text width per box), \code{col.grid} (grid colors), \code{col.select}
#' (color of selected boxes), or \code{col.free} (color of free box)
#' @param player player name
#' 
#' @examples
#' char_to_seed('joe')
#' char_to_seed('sue')
#' 
#' \dontrun{
#' ## these are not exported
#' ccbingo:::bingo()
#' ccbingo:::bingo(letters)
#' ccbingo:::bingo(
#'   cards$zoom, col.grid = rainbow(6), col.free = 'white',
#'   free = '<3', title = 'come and play'
#' )
#' 
#' ## play interactively in an r session
#' ccbingo(cards$zoom)
#' ccbingo(cards$zoom, title = 'my bingo', free = '<3', col.free = 'white',
#'         col.select = 'transparent', col.grid = rainbow(6))
#' 
#' 
#' ## same as above except pass a list of recorded clicks
#' 
#' ## interactive -- first set up a game then make clicks
#' ccbingo(cards$zoom)
#' clicks <- locator(5)
#' ccbingo:::ccbingo_shiny(cards$zoom, 1, NULL, clicks)
#' 
#' ## or pass a list of clicks directly to game
#' set.seed(1)
#' clicks <- list(x = runif(15, 0, 5), y = -runif(15, 0, 5))
#' ccbingo:::ccbingo_shiny(cards$zoom, 1, NULL, clicks)
#' 
#' ## run shiny app
#' ccbingo_app()
#' }
#' 
#' @name ccbingo

#' @rdname ccbingo
#' @export
ccbingo <- function(card, seed = 1L, wins = c('col', 'row', 'corner', 'diag'),
                    ...) {
  seed <- if (is.character(seed))
    char_to_seed(seed) else as.integer(seed)
  set.seed(seed)
  mat <- bingo(card, NULL, ...)
  sel <- NULL
  
  select_box <- function(coords = NULL) {
    co <- if (is.null(coords))
      unlist(locator(1L)) else coords
    if (is.null(co))
      return(NULL)
    
    co <- unname(findInterval(abs(co), 0:5) * sign(co) * c(1, -1))
    
    if (any(co > 5 | co < 1)) {
      message('Outside of grid -- select a box')
      co <- Recall()
    } else if (identical(co, c(3, 3))) {
      message('FREE space -- select a box')
      co <- Recall()
    }
    
    co
  }
  
  while (TRUE) {
    cur <- select_box()
    if (is.null(cur))
      return('game stopped')
    sel <- rbind(sel, cur)
    dup <- duplicated(sel) | duplicated(sel, fromLast = TRUE)
    sel <- sel[!dup, , drop = FALSE]
    
    set.seed(seed)
    mat <- bingo(card, sel, ...)
    win <- check_for_win(mat, wins)
    
    if (nzchar(win))
      break
  }
  
  cat(sprintf('You win!\n\n--> %s\n\nFake an injury and end the call\n', win))
  invisible(win)
}

#' @rdname ccbingo
#' @export
char_to_seed <- function(player) {
  x <- gsub('[^A-z0-9 ]', '', player)
  x <- strsplit(x, '')[[1L]]
  sum(as.integer(factor(x, c(' ', letters, LETTERS, 0:9))))
}

#' @rdname ccbingo
#' @export
ccbingo_app <- function() {
  path <- system.file('shiny', package = 'ccbingo')
  shiny::runApp(path, launch.browser = TRUE)
}

#' ccbingo for shiny
#' 
#' Similar to \code{\link{ccbingo}} but uses a list of click coordinates since
#' \code{\link[shiny]{shiny}} does not call \code{\link{locator}}.
#' 
#' @param card,seed,wins,... see \code{\link{ccbingo}}
#' @param clicks for the \code{\link[shiny]{shiny}} app, a list of click
#' locations (in \link[=grconvertX]{user coordinates})
#' 
#' @examples
#' \dontrun{
#' ## first set up a game then make clicks
#' ccbingo(cards$zoom)
#' clicks <- locator(5)
#' ccbingo:::ccbingo_shiny(cards$zoom, 1, NULL, clicks)
#' 
#' ## or pass a list of clicks directly to game
#' set.seed(1)
#' clicks <- list(x = runif(15, 0, 5), y = -runif(15, 0, 5))
#' ccbingo:::ccbingo_shiny(cards$zoom, 1, NULL, clicks)
#' }
#' 
#' @noRd

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

#' Checking for wins
#' 
#' @param m a 5x5 logical matrix of selected boxes
#' @param wins ways to win the game, any combination of \code{"col"},
#' \code{"row"}, \code{"corner"}, or \code{"diag"}; alternatively, a function
#' to determine wins returning \code{''} for no win and a non-zero length
#' string for wins
#' 
#' @examples
#' m <- matrix(FALSE, 5, 5)
#' diag(m) <- TRUE
#' m
#' ccbingo:::check_for_win(m)
#' ## dont count diagonal wins
#' ccbingo:::check_for_win(m, c('col', 'row', 'corner'))
#' 
#' six_boxes <- function(x) if (sum(x) > 5) 'winner' else ''
#' ccbingo:::check_for_win(m, six_boxes)
#' m[, 1] <- TRUE
#' ccbingo:::check_for_win(m, six_boxes)
#' 
#' @noRd

check_for_win <- function(m, wins = c('col', 'row', 'corner', 'diag')) {
  if (is.function(wins))
    return(wins(m))
  
  if (is.null(wins))
    wins <- c('col', 'row', 'corner', 'diag')
  
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

#' Draw a bingo card
#' 
#' @param card a vector of character strings to make random cards; strings
#' are randomly selected from \code{card} without replacement but recycled
#' as needed to draw a 5x5 grid
#' @param select an optional matrix of selected locations
#' @param title,free strings for the main title and free space
#' @param width a positive integer giving the target column for wrapping
#' lines; see \code{\link{strwrap}}
#' @param col.grid,col.select,col.free colors for the grid, selected boxes,
#' and free space, respectively
#' 
#' @examples
#' bingo()
#' bingo(letters)
#' bingo(cards$zoom)
#' 
#' @noRd

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
