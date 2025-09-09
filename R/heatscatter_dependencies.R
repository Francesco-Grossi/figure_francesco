### heatscatterpoints ###


#' @export
#' @name heatscatterpoints
#' @aliases LSD.heatscatterpoints
#' @title A colored scatterplot based on a two-dimensional Kernel Density Estimation (add to an existing plot)
#' @description Visualize two dimensional data in a three dimensional fashion facilitating a color encoded Kernel Density Estimation (add to an existing plot).
#' @param x a numeric vector.
#' @param y a numeric vector.
#' @param pch plotting 'character'. This can either be a single character or an integer code for one of a set of graphics symbols. (see '?pch', to be passed to plot).
#' @param cexplot a numerical value giving the amount by which the points should be magnified relative to the default.
#' @param nrcol a non-negative integer specifying the number of colors to be used (defaults to 100, if not specified).
#' @param grid an integer specifying the size of the grid used for the KDE.
#' @param colpal a character vector containing R built-in color names or a name of a \code{LSD} colorpalette as a character string (see disco() or \code{\link{disco}}) (defaults to "heat", if not specified).
#' @param simulate logical: if \code{TRUE} (\code{FALSE} by default), a converted colorpalette is used to simulate dichromat vision according to \url{http://www.daltonize.org} (see \code{\link{daltonize}}).
#' @param daltonize logical: if \code{TRUE} (\code{FALSE} by default), a converted colorpalette is used to enhance dichromat vision according to \url{http://www.daltonize.org} (see \code{\link{daltonize}}).
#' @param cvd character string implying the type of color vision deficiency ("p" for protanope, "d" for deuteranope or "t" for tritanope).
#' @param alpha alpha value: a two-digit integer between 01 and 99 for color opacity, i.e. appearance of partial or full transparency (usage omitted by default).
#' @param rev logical: if \code{TRUE} (\code{FALSE} by default), a reversed colorpalette is used.
#' @param xlim x limits, standard graphics parameter.
#' @param ylim y limits, standard graphics parameter.
#' @param only a character string which contains 'x' if the density should only be computed for the x axis, 'y' for the y axis (defaults to 'none' for the two-dimensional case).
#' @param add.contour logical: if \code{TRUE} (\code{FALSE} by default), the contour lines are added to the plot.
#' @param nlevels an integer giving the number of levels of the contour lines.
#' @param color.contour R build-in color for the contour lines.
#' @param greyscale logical: if \code{TRUE} (\code{FALSE} by default), the used colorpalette is converted to greyscales.
#' @param log a character string which contains "x" if the x axis is to be logarithmic, "y" if the y axis is to be logarithmic and "xy" or "yx" if both axes are to be logarithmic.
#' @param ggplot a logical: if \code{TRUE} uses the ggplot2 library to create plots. Defaults to \code{FALSE}.
#' @param xlab x labels, standard graphics parameter.
#' @param ylab y labels, standard graphics parameter.
#' @param ... additional parameters to be passed to points and plot.
#' @author Bjoern Schwalb
#' @seealso \code{\link{comparisonplot}}, \code{\link{demotour}}, \code{\link{disco}}, \code{\link{colorpalette}}
#' @note Two-Dimensional Kernel Density Estimation adapted and modified from Venables and Ripley's MASS package (see reference).
#' @references Venables, W. N. and Ripley, B. D. (2002) \emph{Modern Applied Statistics with S.} Fourth edition. Springer.
#' @examples points <- 10^4
#' x <- c(rnorm(points / 2), rnorm(points / 2) + 4)
#' y <- x + rnorm(points, sd = 0.8)
#' x <- sign(x) * abs(x)^1.3
#'
#' plot.new()
#' plot.window(xlim = c(-5, 15), ylim = c(-4, 8))
#' heatscatterpoints(x, y, add.contour = TRUE, color.contour = "green", greyscale = TRUE)
#' axis(1)
#' axis(2)
#' box()
#' @keywords scatterplot heatcolors


heatscatterpoints <- function(x, y, pch = 19, cexplot = 0.5, nrcol = 30, grid = 100, colpal = "heat", simulate = FALSE, daltonize = FALSE, cvd = "p", alpha = NULL, rev = FALSE, xlim = NULL, ylim = NULL, only = "none", add.contour = FALSE, nlevels = 10, color.contour = "black", greyscale = FALSE, log = "", ggplot = FALSE, xlab = NULL, ylab = NULL, ...) {
  # soundcheck #
  
  if (!is.vector(x) | !is.vector(y)) stop("First two argument must be numeric vectors!")
  if (length(x) != length(y)) stop("Data vectors must be of the same length!")
  sound <- which((!(is.na(x) | is.nan(x) | (x == Inf) | (x == -Inf))) & (!(is.na(y) | is.nan(y) | (y == Inf) | (y == -Inf))))
  if (length(sound) == 0) stop("There are no valid point pairs to plot!")
  x <- x[sound]
  y <- y[sound]
  if (!is.null(xlim)) {
    cut <- x >= xlim[1] & x <= xlim[2]
    x <- x[cut]
    y <- y[cut]
  }
  if (!is.null(ylim)) {
    cut <- y >= ylim[1] & y <= ylim[2]
    y <- y[cut]
    x <- x[cut]
  }
  
  # color handling #
  colpal <- colorpalette(colpal, nrcol, simulate = simulate, daltonize = daltonize, cvd = cvd, alpha = alpha, rev = rev)
  if (greyscale) {
    colpal <- convertgrey(colpal)
  }
  
  # binninmg function #
  
  todiscrete <- function(t, tmin, tmax, bins) {
    erg <- round((t - tmin) / (tmax - tmin) * bins + 0.5)
    erg <- pmin(pmax(erg, 1), bins)
    return(erg)
  }
  
  # kde2d.adj function: adapted and modified from Venables and Ripley's MASS package (distributed under the GPL-2 | GPL-3 license, https://cran.r-project.org/web/packages/MASS/index.html) #
  
  kde2d.adj <- function(x, y, h, n = 25, lims = c(range(x), range(y)), only = "none") {
    nx <- length(x)
    gx <- seq.int(lims[1], lims[2], length.out = n)
    gy <- seq.int(lims[3], lims[4], length.out = n)
    bandwidth.nrd.adj <- function(x) {
      r <- quantile(x, c(0.25, 0.75))
      h <- (r[2] - r[1]) / 1.34
      return(4 * 1.06 * min(sqrt(var(x)), h) * length(x)^(-1 / 5))
    }
    if (missing(h)) {
      bx <- bandwidth.nrd.adj(x)
      by <- bandwidth.nrd.adj(y)
      if (all(c(bx, by) == 0)) {
        h <- rep(0.01, 2)
      } else if (any(c(bx, by) == 0)) {
        h <- rep(max(bx, by), 2)
      } else {
        h <- c(bx, by)
      }
    } else {
      h <- rep(h, length.out = 2)
    }
    h <- h / 4
    ax <- outer(gx, x, "-") / h[1]
    ay <- outer(gy, y, "-") / h[2]
    norm.ax <- dnorm(ax)
    norm.ay <- dnorm(ay)
    if (only == "x") {
      norm.ay <- rep(1, length(ay))
    }
    if (only == "y") {
      norm.ax <- rep(1, length(ax))
    }
    z <- tcrossprod(matrix(norm.ax, , nx), matrix(norm.ay, , nx)) / (nx * h[1] * h[2])
    list(x = gx, y = gy, z = z)
  }
  
  # handle 'log' option #
  
  if (log == "") {
    xlog <- x
    ylog <- y
  } else if (log == "x") {
    xlog <- log(x, 10)
    ylog <- y
  } else if (log == "y") {
    xlog <- x
    ylog <- log(y, 10)
  } else if (log %in% c("xy", "yx")) {
    xlog <- log(x, 10)
    ylog <- log(y, 10)
  }
  
  # estimate two-dimensional KDE for color encoding #
  
  d <- kde2d.adj(xlog, ylog, n = grid, only = only)
  
  # binning #
  
  xdiscrete <- todiscrete(xlog, min(xlog), max(xlog), bins = grid)
  ydiscrete <- todiscrete(ylog, min(ylog), max(ylog), bins = grid)
  
  # color assignment #
  
  getfrommat <- function(a) {
    d$z[a[1], a[2]]
  }
  heatvec <- unlist(apply(cbind(xdiscrete, ydiscrete), 1, getfrommat))
  coldiscrete <- todiscrete(heatvec, min(d$z), max(d$z), bins = nrcol)
  
  if (ggplot) {
    require(ggplot2)
    
    df_plot <- dplyr::tibble(x = x, y = y, col = 1:length(x))
    
    # create new ggplot
    gg <- df_plot %>%
      ggplot(aes(x = x, y = y)) +
      geom_point(aes(colour = factor(col)), show.legend = FALSE) +
      scale_colour_manual(values = colpal[coldiscrete]) +
      theme_classic() +
      labs(x = xlab, y = ylab)
    
    # print(gg)
  } else {
    # add to existing graphics device #
    points(x, y, col = colpal[coldiscrete], pch = pch, cex = cexplot, ...)
    gg <- NULL
  }
  
  # handle 'add.contour' option #
  
  if (add.contour) {
    contour(d, add = TRUE, nlevels = nlevels, col = color.contour)
  }
  
  return(gg)
}


### aliases ###


LSD.heatscatterpoints <- heatscatterpoints


### heatscatter ###


#' @export
#' @name heatscatter
#' @aliases LSD.heatscatter
#' @title A colored scatterplot based on a two-dimensional Kernel Density Estimation
#' @description Visualize two dimensional data in a three dimensional fashion facilitating a color encoded Kernel Density Estimation.
#' @param x a numeric vector.
#' @param y a numeric vector.
#' @param pch plotting 'character'. This can either be a single character or an integer code for one of a set of graphics symbols. (see '?pch', to be passed to plot).
#' @param cexplot a numerical value giving the amount by which the points should be magnified relative to the default.
#' @param nrcol a non-negative integer specifying the number of colors to be used (defaults to 100, if not specified).
#' @param grid an integer specifying the size of the grid used for the KDE.
#' @param colpal a character vector containing R built-in color names or a name of a \code{LSD} colorpalette as a character string (see disco() or \code{\link{disco}}) (defaults to "heat", if not specified).
#' @param simulate logical: if \code{TRUE} (\code{FALSE} by default), a converted colorpalette is used to simulate dichromat vision according to \url{http://www.daltonize.org} (see \code{\link{daltonize}}).
#' @param daltonize logical: if \code{TRUE} (\code{FALSE} by default), a converted colorpalette is used to enhance dichromat vision according to \url{http://www.daltonize.org} (see \code{\link{daltonize}}).
#' @param cvd character string implying the type of color vision deficiency ("p" for protanope, "d" for deuteranope or "t" for tritanope).
#' @param alpha alpha value: a two-digit integer between 01 and 99 for color opacity, i.e. appearance of partial or full transparency (usage omitted by default).
#' @param rev logical: if \code{TRUE} (\code{FALSE} by default), a reversed colorpalette is used.
#' @param xlim x limits, standard graphics parameter.
#' @param ylim y limits, standard graphics parameter.
#' @param xlab x labels, standard graphics parameter.
#' @param ylab y labels, standard graphics parameter.
#' @param main title(s) of the plot, standard graphics parameter.
#' @param cor logical: if \code{TRUE} (\code{FALSE} by default), the correlation is added to the title.
#' @param method a character specifying the correlation method to use ('spearman' (default), 'pearson' or 'kendall').
#' @param only a character string which contains 'x' if the density should only be computed for the x axis, 'y' for the y axis (defaults to 'none' for the two-dimensional case).
#' @param add.contour logical: if \code{TRUE} (\code{FALSE} by default), the contour lines are added to the plot.
#' @param nlevels an integer giving the number of levels of the contour lines.
#' @param color.contour R build-in color for the contour lines.
#' @param greyscale logical: if \code{TRUE} (\code{FALSE} by default), the used colorpalette is converted to greyscales.
#' @param log a character string which contains "x" if the x axis is to be logarithmic, "y" if the y axis is to be logarithmic and "xy" or "yx" if both axes are to be logarithmic.
#' @param ggplot a logical: if \code{TRUE} uses the ggplot2 library to create plots. Defaults to \code{FALSE}.
#' @param ... additional parameters to be passed to points and plot.
#' @author Achim Tresch, Bjoern Schwalb
#' @seealso \code{\link{comparisonplot}}, \code{\link{demotour}}, \code{\link{disco}}, \code{\link{colorpalette}}
#' @note Two-Dimensional Kernel Density Estimation adapted and modified from Venables and Ripley's MASS package (see reference).
#' @references Venables, W. N. and Ripley, B. D. (2002) \emph{Modern Applied Statistics with S.} Fourth edition. Springer.
#' @examples points <- 10^4
#' x <- c(rnorm(points / 2), rnorm(points / 2) + 4)
#' y <- x + rnorm(points, sd = 0.8)
#' x <- sign(x) * abs(x)^1.3
#'
#' heatscatter(x, y)
#'
#' heatscatter(x, y, colpal = "bl2gr2rd", main = "bl2gr2rd", cor = FALSE)
#'
#' heatscatter(x, y, cor = FALSE, add.contour = TRUE, color.contour = "red", greyscale = TRUE)
#'
#' heatscatter(x, y, colpal = "spectral", cor = FALSE, add.contour = TRUE)
#' @keywords scatterplot heatcolors


heatscatter <- function(x, y, pch = 19, cexplot = 0.5, nrcol = 30, grid = 100, colpal = "heat", simulate = FALSE, daltonize = FALSE, cvd = "p", alpha = NULL, rev = FALSE, xlim = NULL, ylim = NULL, xlab = NULL, ylab = NULL, main = "heatscatter", cor = FALSE, method = "spearman", only = "none", add.contour = FALSE, nlevels = 10, color.contour = "black", greyscale = FALSE, log = "", ggplot = FALSE, ...) {
  # parse variable names #
  
  if (is.null(xlab)) {
    xlab <- deparse(substitute(x))
  }
  if (is.null(ylab)) {
    ylab <- deparse(substitute(y))
  }
  
  # soundcheck #
  
  if (!is.vector(x) | !is.vector(y)) stop("First two argument must be numeric vectors!")
  if (length(x) != length(y)) stop("Data vectors must be of the same length!")
  sound <- which((!(is.na(x) | is.nan(x) | (x == Inf) | (x == -Inf))) & (!(is.na(y) | is.nan(y) | (y == Inf) | (y == -Inf))))
  if (length(sound) == 0) stop("There are no valid point pairs to plot!")
  x <- x[sound]
  y <- y[sound]
  if (!is.null(xlim)) {
    cut <- x >= xlim[1] & x <= xlim[2]
    x <- x[cut]
    y <- y[cut]
  }
  if (!is.null(ylim)) {
    cut <- y >= ylim[1] & y <= ylim[2]
    y <- y[cut]
    x <- x[cut]
  }
  
  # handle 'log' option #
  
  if (log == "") {
    valid <- 1:length(x)
  } else if (log == "x") {
    valid <- which(x > 0)
  } else if (log == "y") {
    valid <- which(y > 0)
  } else if (log %in% c("xy", "yx")) {
    valid <- intersect(which(x > 0), which(y > 0))
  }
  x <- x[valid]
  y <- y[valid]
  
  # handle 'cor' option #
  
  if (cor) {
    main <- paste(main, " cor = ", round(cor(x, y, method = method), digits = 2))
  }
  
  # handle graphics device  #
  
  if (ggplot) {
    heatscatterpoints(x, y, pch = pch, cexplot = cexplot, nrcol = nrcol, grid = grid, colpal = colpal, simulate = simulate, daltonize = daltonize, cvd = cvd, alpha = alpha, rev = rev, xlim = xlim, ylim = ylim, only = only, add.contour = add.contour, nlevels = nlevels, color.contour = color.contour, greyscale = greyscale, log = log, ggplot = ggplot, xlab = xlab, ylab = ylab, ...)
  } else {
    plot(x, y, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, main = "", type = "n", log = log, ...)
    heatscatterpoints(x, y, pch = pch, cexplot = cexplot, nrcol = nrcol, grid = grid, colpal = colpal, simulate = simulate, daltonize = daltonize, cvd = cvd, alpha = alpha, rev = rev, xlim = xlim, ylim = ylim, only = only, add.contour = add.contour, nlevels = nlevels, color.contour = color.contour, greyscale = greyscale, log = log, ggplot = ggplot, xlab = xlab, ylab = ylab, ...)
    mtext(paste(main), 3, 2, cex = 1.25)
  }
}


### aliases ###


LSD.heatscatter <- heatscatter


### heatpairs ###


#' @export
#' @name heatpairs
#' @aliases LSD.heatpairs
#' @title Pairwise colored scatterplot based on a two-dimensional Kernel Density Estimation
#' @description Pairwise visualization of two dimensional data in a three dimensional fashion facilitating a color encoded Kernel Density Estimation.
#' @param mat a matrix with numerical entries.
#' @param main title(s) of the plot, standard graphics parameter.
#' @param xlim x limits, standard graphics parameter.
#' @param ylim y limits, standard graphics parameter.
#' @param labels a character vector giving the labels to be shown on the diagonal.
#' @param add.points logical: if \code{TRUE} (\code{FALSE} by default), a certain 'group' of points can be colored in all pairwise plots.
#' @param group indices or rownames of 'mat' to be highlighted in all pairwise plots (not necessarily all).
#' @param color.group R build-in color in which the 'group' of points should be highlighted.
#' @param method a character specifying the correlation method to use ('spearman' (default), 'pearson' or 'kendall').
#' @param colpal a character vector containing R built-in color names or a name of a \code{LSD} colorpalette as a character string (see disco() or \code{\link{disco}}) (defaults to "heat", if not specified).
#' @param simulate logical: if \code{TRUE} (\code{FALSE} by default), a converted colorpalette is used to simulate dichromat vision according to \url{http://www.daltonize.org} (see \code{\link{daltonize}}).
#' @param daltonize logical: if \code{TRUE} (\code{FALSE} by default), a converted colorpalette is used to enhance dichromat vision according to \url{http://www.daltonize.org} (see \code{\link{daltonize}}).
#' @param cvd character string implying the type of color vision deficiency ("p" for protanope, "d" for deuteranope or "t" for tritanope).
#' @param alpha alpha value: a two-digit integer between 01 and 99 for color opacity, i.e. appearance of partial or full transparency (usage omitted by default).
#' @param rev logical: if \code{TRUE} (\code{FALSE} by default), a reversed colorpalette is used.
#' @param pch plotting 'character'. This can either be a single character or an integer code for one of a set of graphics symbols. (see '?pch', to be passed to plot).
#' @param cexplot a numerical value giving the amount by which the points should be magnified relative to the default.
#' @param cor.cex a numerical value giving the amount by which the correlation characters should be magnified relative to the default.
#' @param nrcol a non-negative integer specifying the number of colors to be used (defaults to 100, if not specified).
#' @param grid an integer specifying the size of the grid used for the KDE.
#' @param only a character string which contains 'x' if the density should only be computed for the x axis, 'y' for the y axis (defaults to 'none' for the two-dimensional case).
#' @param add.contour logical: if \code{TRUE} (\code{FALSE} by default), the contour lines are added to the plot.
#' @param nlevels an integer giving the number of levels of the contour lines.
#' @param color.contour R build-in color for the contour lines.
#' @param greyscale logical: if \code{TRUE} (\code{FALSE} by default), the used colorpalette is converted to greyscales.
#' @param log a character string which contains "x" if the x axis is to be logarithmic, "y" if the y axis is to be logarithmic and "xy" or "yx" if both axes are to be logarithmic.
#' @param ... additional parameters to be passed to points and plot
#' @author Bjoern Schwalb
#' @seealso \code{\link{comparisonplot}}, \code{\link{demotour}}, \code{\link{disco}}, \code{\link{colorpalette}}
#' @examples points <- 10^4
#' x <- rnorm(points / 2)
#' x <- c(x, x + 2.5)
#' y <- x + rnorm(points, sd = 0.75)
#' x <- sign(x) * abs(x)^1.3
#' mat <- cbind(x, y, x + rnorm(points, sd = 0.5))
#' colnames(mat) <- c("x", "y", "z")
#' rownames(mat) <- 1:nrow(mat)
#'
#' heatpairs(mat, labels = c(expression(Xi), expression(Lambda), expression(Delta)))
#' @keywords scatterplot heatcolors


heatpairs <- function(mat, main = "heatpairs", xlim = NULL, ylim = NULL, labels = NULL, add.points = FALSE, group = NULL, color.group = "magenta", method = "spearman", colpal = "heat", simulate = FALSE, daltonize = FALSE, cvd = "p", alpha = NULL, rev = FALSE, pch = 19, cexplot = 0.5, cor.cex = 2.5, nrcol = 30, grid = 100, only = "none", add.contour = FALSE, nlevels = 10, color.contour = "black", greyscale = FALSE, log = "", ...) {
  if (!is.matrix(mat)) stop("First argument must be a matrix !")
  if (is.null(xlim)) {
    xlim <- c(min(mat, na.rm = TRUE), max(mat, na.rm = TRUE))
  }
  if (is.null(ylim)) {
    ylim <- c(min(mat, na.rm = TRUE), max(mat, na.rm = TRUE))
  }
  if (is.null(labels)) {
    labels <- colnames(mat)
  }
  
  # handle 'log' option #
  
  if (log == "") {
    valid <- 1:dim(mat)[1]
  } else if (log %in% c("x", "y", "xy", "yx")) {
    valid <- apply(mat, 1, function(x) {
      all(x > 0)
    })
  }
  mat <- mat[valid, ]
  
  pairs(mat, labels = labels, xlim = xlim, ylim = ylim, lower.panel = function(x, y, ...) {
    {if (log == "") {
      x.pos <- diff(xlim) / 2 + xlim[1]
      y.pos <- diff(ylim) / 2 + ylim[1]
    } else if (log == "x") {
      x.pos <- 10^(diff(log(xlim, 10)) / 2 + log(xlim, 10)[1])
      y.pos <- diff(ylim) / 2 + ylim[1]
    } else if (log == "y") {
      x.pos <- diff(xlim) / 2 + xlim[1]
      y.pos <- 10^(diff(log(ylim, 10)) / 2 + log(ylim, 10)[1])
    } else if (log %in% c("xy", "yx")) {
      x.pos <- 10^(diff(log(xlim, 10)) / 2 + log(xlim, 10)[1])
      y.pos <- 10^(diff(log(ylim, 10)) / 2 + log(ylim, 10)[1])
    }}
    text(x.pos, y.pos, round(cor(x, y, method = method, use = "na.or.complete"), digits = 2), cex = cor.cex)
  }, main = main, upper.panel = function(x, y, ...) {
    heatscatterpoints(x, y, colpal = colpal, pch = pch, cexplot = cexplot, nrcol = nrcol, grid = grid, simulate = simulate, daltonize = daltonize, cvd = cvd, alpha = alpha, rev = rev, only = only, add.contour = add.contour, nlevels = nlevels, color.contour = color.contour, greyscale = greyscale, log = log, ...)
    if (log == "") {
      abline(a = 0, b = 1)
    } else if (log == "x") {
      lines(seq(xlim[1], xlim[2], length.out = 100), seq(ylim[1], ylim[2], length.out = 100))
    } else if (log == "y") {
      lines(seq(xlim[1], xlim[2], length.out = 100), seq(ylim[1], ylim[2], length.out = 100))
    } else if (log %in% c("xy", "yx")) {
      abline(a = 0, b = 1)
    }
    if (add.points) {
      points(x[rownames(mat) %in% group], y[rownames(mat) %in% group], col = color.group, log = log, ...)
    }
  }, log = log, ...)
}


### aliases ###


LSD.heatpairs <- heatpairs

### colorpalette ###


#' @export
#' @name colorpalette
#' @aliases LSD.colorpalette
#' @title Provides colorpalettes containing R built-in colors
#' @description Provides pre-designed colorpalettes (character vectors containing R built-in colors) of this and several other R packages (grDevices, RColorBrewer, colorRamps) as well as custom-made ones.
#' @param colpal a character vector containing R built-in color names or a name of a \code{LSD} colorpalette as a character string (see disco() or \code{\link{disco}}).
#' @param nrcol a non-negative integer specifying the number of colors to be used (defaults to length of \code{colpal}, if not specified).
#' @param simulate logical: if \code{TRUE} (\code{FALSE} by default), a converted colorpalette is returned to simulate dichromat vision according to \url{http://www.daltonize.org} (see \code{\link{daltonize}}).
#' @param daltonize logical: if \code{TRUE} (\code{FALSE} by default), a converted colorpalette is returned to enhance dichromat vision according to \url{http://www.daltonize.org} (see \code{\link{daltonize}}).
#' @param cvd character string implying the type of color vision deficiency ("p" for protanope, "d" for deuteranope or "t" for tritanope).
#' @param alpha alpha value: a two-digit integer between 01 and 99 for color opacity, i.e. appearance of partial or full transparency (usage omitted by default).
#' @param rev logical: if \code{TRUE} (\code{FALSE} by default), a reversed colorpalette is returned.
#' @return \code{colorpalette} returns a vector containing R built-in colors in hexadecimal representation.
#' @author Achim Tresch, Bjoern Schwalb
#' @seealso \code{\link{disco}}, \code{\link{demotour}}
#' @examples colorpalette("heat")
#' colorpalette(c("darkred","grey","darkblue"),10)
#' @keywords color alpha


colorpalette = function(colpal,nrcol = NULL,simulate = FALSE,daltonize = FALSE,cvd = "p",alpha = NULL,rev = FALSE)
{
  if (length(colpal) > 1){palette = colpal}
  else{palette = switch(colpal,
                        
                        # custom-made palettes #
                        
                        heat = c("grey","darkblue","red","orange","gold"),
                        crazyred = c( "#940000","#A50000","#FF5C5C","#FFB9B9"),
                        crazygreen = c("dark green","#009700","green","#C0F5D0"),
                        crazyblue = c("dark blue","blue","#7390EE","light blue"),
                        mountain = c("light green","dark green","black","dark grey","#F0F0F0"),
                        girly = c("violet","violetred","violetred1","purple","purple3"),
                        jamaica = c("red","yellow","green"),
                        standard = c("brown","gold","yellow","lightyellow","white"),
                        colorblind = c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7"),
                        
                        # palettes from the RColorBrewer package #
                        
                        ylorrd = rev(c("#FFFFCC","#FFEDA0","#FED976","#FEB24C","#FD8D3C","#FC4E2A","#E31A1C","#BD0026","#800026")),
                        ylorbr = rev(c("#FFFFE5","#FFF7BC","#FEE391","#FEC44F","#FE9929","#EC7014","#CC4C02","#993404","#662506")),
                        ylgnbu = rev(c("#FFFFD9","#EDF8B1","#C7E9B4","#7FCDBB","#41B6C4","#1D91C0","#225EA8","#253494","#081D58")),
                        ylgn = rev(c("#FFFFE5","#F7FCB9","#D9F0A3","#ADDD8E","#78C679","#41AB5D","#238443","#006837","#004529")),
                        reds = rev(c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D")),
                        rdpu = rev(c("#FFF7F3","#FDE0DD","#FCC5C0","#FA9FB5","#F768A1","#DD3497","#AE017E","#7A0177","#49006A")),
                        purples = rev(c("#FCFBFD","#EFEDF5","#DADAEB","#BCBDDC","#9E9AC8","#807DBA","#6A51A3","#54278F","#3F007D")),
                        purd = rev(c("#F7F4F9","#E7E1EF","#D4B9DA","#C994C7","#DF65B0","#E7298A","#CE1256","#980043","#67001F")),
                        pubugn = rev(c("#FFF7FB","#ECE2F0","#D0D1E6","#A6BDDB","#67A9CF","#3690C0","#02818A","#016C59","#014636")),
                        pubu = rev(c("#FFF7FB","#ECE7F2","#D0D1E6","#A6BDDB","#74A9CF","#3690C0","#0570B0","#045A8D","#023858")),
                        orrd = rev(c("#FFF7EC","#FEE8C8","#FDD49E","#FDBB84","#FC8D59","#EF6548","#D7301F","#B30000","#7F0000")),
                        oranges = rev(c("#FFF5EB","#FEE6CE","#FDD0A2","#FDAE6B","#FD8D3C","#F16913","#D94801","#A63603","#7F2704")),
                        greys = rev(c("#FFFFFF","#F0F0F0","#D9D9D9","#BDBDBD","#969696","#737373","#525252","#252525","#000000")),
                        greens = rev(c("#F7FCF5","#E5F5E0","#C7E9C0","#A1D99B","#74C476","#41AB5D","#238B45","#006D2C","#00441B")),
                        gnbu = rev(c("#F7FCF0","#E0F3DB","#CCEBC5","#A8DDB5","#7BCCC4","#4EB3D3","#2B8CBE","#0868AC","#084081")),
                        bupu = rev(c("#F7FCFD","#E0ECF4","#BFD3E6","#9EBCDA","#8C96C6","#8C6BB1","#88419D","#810F7C","#4D004B")),
                        bugn = rev(c("#F7FCFD","#E5F5F9","#CCECE6","#99D8C9","#66C2A4","#41AE76","#238B45","#006D2C","#00441B")),
                        blues = rev(c("#F7FBFF","#DEEBF7","#C6DBEF","#9ECAE1","#6BAED6","#4292C6","#2171B5","#08519C","#08306B")),
                        spectral = c("#9E0142","#D53E4F","#F46D43","#FDAE61","#FEE08B","#FFFFBF","#E6F598","#ABDDA4","#66C2A5","#3288BD","#5E4FA2"),
                        rdylgn = c("#A50026","#D73027","#F46D43","#FDAE61","#FEE08B","#FFFFBF","#D9EF8B","#A6D96A","#66BD63","#1A9850","#006837"),
                        rdylbu = c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"),
                        rdgy = c("#67001F","#B2182B","#D6604D","#F4A582","#FDDBC7","#FFFFFF","#E0E0E0","#BABABA","#878787","#4D4D4D","#1A1A1A"),
                        rdbu = c("#67001F","#B2182B","#D6604D","#F4A582","#FDDBC7","#F7F7F7","#D1E5F0","#92C5DE","#4393C3","#2166AC","#053061"),
                        puor = c("#7F3B08","#B35806","#E08214","#FDB863","#FEE0B6","#F7F7F7","#D8DAEB","#B2ABD2","#8073AC","#542788","#2D004B"),
                        prgn = c("#40004B","#762A83","#9970AB","#C2A5CF","#E7D4E8","#F7F7F7","#D9F0D3","#A6DBA0","#5AAE61","#1B7837","#00441B"),
                        piyg = c("#8E0152","#C51B7D","#DE77AE","#F1B6DA","#FDE0EF","#F7F7F7","#E6F5D0","#B8E186","#7FBC41","#4D9221","#276419"),
                        brbg = c("#543005","#8C510A","#BF812D","#DFC27D","#F6E8C3","#F5F5F5","#C7EAE5","#80CDC1","#35978F","#01665E","#003C30"),
                        
                        # palettes from the grDevices package #
                        
                        standardterrain = terrain.colors(9),
                        standardtopo = topo.colors(9),
                        standardheat = heat.colors(9),
                        standardrainbow = rainbow(9,start=0.7,end=0.1),
                        standardcm = cm.colors(9),
                        
                        # palettes from the colorRamps package #
                        
                        bl2gr = c("#0000FF","#001AE6","#0033CC","#004DB3","#006699","#008080","#009966","#00B34C","#00CC33","#00E619","#00FF00"),
                        bl2gr2rd = c("#0000BF","#0000FF","#0080FF","#00FFFF","#40FFBF","#80FF80","#BFFF40","#FFFF00","#FF8000","#FF0000","#BF0000"),
                        bl2rd = c("#0000FF","#0033FF","#0066FF","#0099FF","#00CCFF","#00FFFF","#FFCC00","#FF9900","#FF6600","#FF3300","#FF0000"),
                        bl2yl = c("#0000FF","#1A1AE6","#3333CC","#4D4DB3","#666699","#808080","#999966","#B3B34C","#CCCC33","#E6E619","#FFFF00"),
                        cy2yl = c("#00FFFF","#1AFFE6","#33FFCC","#4DFFB3","#66FF99","#80FF80","#99FF66","#B3FF4C","#CCFF33","#E6FF19","#FFFF00"),
                        gr2rd = c("#00FF00","#1AE600","#33CC00","#4DB300","#669900","#808000","#996600","#B34C00","#CC3300","#E61900","#FF0000"),
                        ma2gr = c("#FF00FF","#E61AE6","#CC33CC","#B34DB3","#996699","#808080","#669966","#4CB34C","#33CC33","#19E619","#00FF00"),
                        matlablike = c("#0000AA","#0040FF","#0080FF","#40BFFF","#80FFFF","#BFFFBF","#FFFF80","#FFBF40","#FF8000","#FF4000","#AA0000"),
                        matlablike2 = c("#0000BF","#0000FF","#0080FF","#00FFFF","#40FFBF","#80FF80","#BFFF40","#FFFF00","#FF8000","#FF0000","#BF0000")
  )}
  if (is.null(palette)){stop("'colpal' should be a valid colorpalette name (see 'disco()') or a character vector of at least two R built-in color names for interpolation!")}
  if (is.null(nrcol)){nrcol = length(palette)}
  palette = try(colorRampPalette(palette)(nrcol),silent = TRUE)
  if (class(palette) == "try-error"){stop("'colpal' should be a valid colorpalette name (see 'disco()') or a character vector of at least two R built-in color names for interpolation!")}
  if (rev){palette = rev(palette)}
  if (simulate){palette = daltonize(palette,cvd = cvd,show = FALSE)$simulated}
  if (daltonize){palette = daltonize(palette,cvd = cvd,show = FALSE)$enhanced}
  if (!is.null(alpha)){palette = convertcolor(palette,alpha)}
  return(palette)
}


# alias #


LSD.colorpalette = colorpalette
