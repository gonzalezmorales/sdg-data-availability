# Doughnut plot (https://gist.github.com/chelsyx/ebc8ff5d7125a79d4297)

#' 
#' @param x a vector of non-negative numerical quantities.
#'          The values in \code{x} are displayed as the areas of 
#'          doughnut slices.
#' @param labels one or more expressions or character strings giving
#'          names for the slices.  Other objects are coerced by
#'          \code{\link{as.graphicsAnnot}}.  For empty or \code{NA}
#'          (after coercion to character) labels, no label nor 
#'          pointing line is drawn.
#' @param edges the circular outline of the doughnut is approximated 
#'          by a polygon with this many edges.
#' @param outer.radius the doughnut is drawn centered in a square box 
#'          whose sides range from \eqn{-1} to \eqn{1}.  If the 
#'          character strings labeling the slices are long it may be 
#'          necessary to use a smaller radius.
#' @param inner.radius Size of the inner radius to be whited 
#' @param clockwise logical indicating if slices are drawn clockwise 
#'          or counter clockwise (i.e., mathematically positive 
#'          direction), the latter is default. 
#' @param init.angle number specifying the \emph{starting angle} (in
#'          degrees) for the slices. Defaults to 0 (i.e., 
#'          \sQuote{3 o'clock}) unless \code{clockwise} is true where 
#'          \code{init.angle} defaults to 90 (degrees), (i.e., 
#'          \sQuote{12 o'clock}).
#' @param density the density of shading lines, in lines per inch.
#'          The default value of \code{NULL} means that no shading 
#'          lines are drawn. Non-positive values of \code{density} 
#'           also inhibit the drawing of shading lines.
#' @param angle the slope of shading lines, given as an angle in  
#'          degrees (counter-clockwise).
#' @param col a vector of colors to be used in filling or shading
#'          the slices. If missing a set of 6 pastel colours is
#'          used, unless \code{density} is specified when 
#'          \code{par("fg")} is used. 
#' @param border, lty (possibly vectors) arguments passed to 
#'          \code{\link{polygon}} which draws each slice.
#' @param main an overall title for the plot.
#' @param \dots graphical parameters can be given as arguments to
#'          \code{pie}.  They will affect the main title and 
#'          labels only.
#'          
#' @author Original pie plot by R Core, amended by Markus Gesmann 
#'         for a doughnut plot
#' @keywords hplot
#' @seealso \code{\link{pie}} 
#' @export
#' @examples
#'  x <- c(2,4,3,2,4)
#'  doughnut(x)
#'  ## Add lables
#'  doughnut(x, labels=LETTERS[1:5])

doughnut <-
  function (x, labels = names(x), edges = 200, outer.radius = 0.8, 
            inner.radius=0.6, clockwise = FALSE,
            init.angle = if (clockwise) 90 else 0, density = NULL, 
            angle = 45, col = NULL, border = FALSE, lty = NULL, 
            main = NULL, ...)
  {
    if (!is.numeric(x) || any(is.na(x) | x < 0))
      stop("'x' values must be positive.")
    if (is.null(labels))
      labels <- as.character(seq_along(x))
    else labels <- as.graphicsAnnot(labels)
    x <- c(0, cumsum(x)/sum(x))
    dx <- diff(x)
    nx <- length(dx)
    plot.new()
    pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    if (pin[1L] > pin[2L])
      xlim <- (pin[1L]/pin[2L]) * xlim
    else ylim <- (pin[2L]/pin[1L]) * ylim
    plot.window(xlim, ylim, "", asp = 1)
    if (is.null(col))
      col <- if (is.null(density))
        palette()
    else par("fg")
    col <- rep(col, length.out = nx)
    border <- rep(border, length.out = nx)
    lty <- rep(lty, length.out = nx)
    angle <- rep(angle, length.out = nx)
    density <- rep(density, length.out = nx)
    twopi <- if (clockwise)
      -2 * pi
    else 2 * pi
    t2xy <- function(t, radius) {
      t2p <- twopi * t + init.angle * pi/180
      list(x = radius * cos(t2p), 
           y = radius * sin(t2p))
    }
    for (i in 1L:nx) {
      n <- max(2, floor(edges * dx[i]))
      P <- t2xy(seq.int(x[i], x[i + 1], length.out = n),
                outer.radius)
      polygon(c(P$x, 0), c(P$y, 0), density = density[i], 
              angle = angle[i], border = border[i], 
              col = col[i], lty = lty[i])
      Pout <- t2xy(mean(x[i + 0:1]), outer.radius)
      lab <- as.character(labels[i])
      if (!is.na(lab) && nzchar(lab)) {
        lines(c(1, 1.05) * Pout$x, c(1, 1.05) * Pout$y)
        text(1.2 * Pout$x, 1.2 * Pout$y, labels[i], 
             xpd = TRUE, adj = ifelse(Pout$x < 0, 1, 0), 
             ...)
      }
      ## Add white disc          
      Pin <- t2xy(seq.int(0, 1, length.out = n*nx),
                  inner.radius)
      polygon(Pin$x, Pin$y, density = density[i], 
              angle = angle[i], border = border[i], 
              col = "white", lty = lty[i])
    }
    
    title(main = main, ...)
    invisible(NULL)
  }