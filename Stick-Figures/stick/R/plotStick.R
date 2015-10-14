
#' @title Plot Stick Men and Women at coordinates
#' 
#' @description Create a stick person plot. 
#' A new plot is created with points specified by x and y.
#' Stick people are scaled so that ten people could be stacked
#' head to toe in the plotting area. Set argument tower to change this.
#' Stick people will not wear clothes unless col is specified.
#' Their line colours can be specified with linecol.
#' 
#' @param x numeric vector, progress of stick people from left to right
#' @param y numeric vector, progress of stick people from bottom to top
#' @param xlim length 2 numeric, x-axis limits to use, otherwise range of x expanded
#' @param ylim length 2 numeric, y-axis limits to use, otherwise range of y expanded
#' @param xlab single character, x axis label
#' @param ylab single character, y axis label
#' @param lwd line weight
#' @param linecol color of lines
#' @param hatcol color of hat, or NULL to supress hat (default \code{NULL})
#' @param shcol color of shirt, or NULL to supress shirt (default \code{NULL})
#' @param clcol color of clothes, or NULL to supress clothes (default \code{NULL})
#' @param col matrix of colors for fill, overriding shcol, clcol and hatcol
#'     with up to three columns (default \code{NULL})
#' @param arms character vector "down", "neutral", "up", "hip", "wave"
#' @param gender character vector "male", "female"
#' @param face list or character vector "default" ("neutral"), "happy", "sad", "annoyed", "surprised"
#'     face can also be a numeric matrix with 4 rows and four columns. 
#'     See \code{\link{drawStick}} for more details.
#' @param legs single character "default" or "apart"
#' @param hat single logical plot hat or automatic if NA (default NA)
#' @param tower single numeric approximate number of stick people 
#' that could be stacked in the plotting area (default 10)
#' @param newplot single logical should new plot be created before adding stick men? (default TRUE)
#' @return NULL
#' @export
#' @author Francis Smart [cre, cph, aut], Chris Campbell (Mango Solutions) [aut]
#' @examples
#' plotStick(x = 1:10, y = c(1:5, 5:1))
#' points(1:10, c(1:5, 5:1), col = 2)
#' points(1:10, c(1:5, 5:1), col = 2, pch = 3)
#' plotStick(x = 1:10, y = sin(1:10), hatcol = 2, shcol = rainbow(10), 
#'     clcol = 1:10, gender = c("male", "female"), 
#'     arms = c("down", "neutral", "up", "hip", "wave"))
#' plotStick(x = 1:10, y = cos(1:10), 
#'     hat = c("none", "shapka", "beanie", "fedora"), col = rainbow(30), cex = 2)

plotStick <- function(x, y, xlim, ylim, xlab = "x", ylab = "y", 
    lwd = 1, linecol = 1, hatcol = NULL, shcol = NULL, clcol = NULL, 
    col = NULL, gender = c("male", "female"), 
    arms = "default", face = "default", legs = "default", hat = "default", 
    tower = 10, newplot = TRUE, ...) {
    
    if (missing(x)) { stop("x is missing") }
    
    if (missing(y)) { stop("y is missing") }
    
    lngtx <- length(x)
    
    if (lngtx != length(y)) { stop("x and y must be the same length") }
    
    if (!newplot) {
        if (identical(x = dev.cur(), y = c("null device" = 1L))) { 
            stop("cannot add stick men to NULL device") }
        xlim <- par("usr")[1:2]
        ylim <- par("usr")[3:4]
        ex <- diff(xlim) / tower
        ey <- diff(ylim) / tower 
        
    } else {
        mx <- missing(xlim)
        
        if (mx) { xlim <- range(x[is.finite(x)]) }
        
        if (diff(xlim) == 0) { 
            signs <- c(-1, 1)
            pad <- 0.3 * ylim
            xlim <- xlim + signs * pad 
        }
        
        ex <- diff(xlim) / tower
        
        if (mx) { xlim <- xlim + c(-ex, ex) }
        
        my <- missing(ylim)
        
        if (my) { ylim <- range(y[is.finite(y)]) }
        
        if (diff(ylim) == 0) { 
            signs <- c(-1, 1)
            pad <- 0.3 * ylim
            ylim <- ylim + signs * pad 
        }
        
        ey <- diff(ylim) / tower 
        
        if (my) { ylim <- ylim + c(-ey, ey) }
    }
    if (length(lwd) != lngtx) { lwd <- rep(lwd, times = ceiling(lngtx / length(lwd))) }
    
    if (length(linecol) != lngtx) { linecol <- rep(linecol, times = ceiling(lngtx / length(linecol))) }
    
    clothes <- matrix(NA, nrow = lngtx, ncol = 3)
    
    if (!is.null(col)) {
        
        if (is(col, "matrix") && ncol(col) != 3) { col <- c(col) }
        
        if (!is(col, "matrix")) {
            
            col <- matrix(rep(c(col), times = lngtx * 3)[seq.int(1, lngtx * 3)], 
                nrow = lngtx, ncol = 3, byrow = FALSE)
        }
        
        clothes <- col 
        
        
    } else {
        
        if (!is.null(shcol)) { clothes[, 1] <- shcol }
        
        if (!is.null(clcol)) { clothes[, 2] <- clcol }
        
        if (!is.null(hatcol)) { clothes[, 3] <- hatcol }
    }
    
    if (length(gender) != lngtx) { 
        gender <- rep(gender, times = ceiling(lngtx / length(gender))) }
    
    if (length(arms) != lngtx) { 
        arms <- rep(arms, times = ceiling(lngtx / length(arms))) }
    
    if (length(face) != lngtx) { 
        face <- rep(face, times = ceiling(lngtx / length(face))) }
    
    if (length(legs) != lngtx) { 
        legs <- rep(legs, times = ceiling(lngtx / length(legs))) }
    
    if (length(hat) != lngtx) { 
        hat <- rep(hat, times = ceiling(lngtx / length(hat))) }
    
    if (newplot) {
        plot(x, y, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, type = "n", ...)
    }
    if (hasArg("cex")) {
        
        cex <- match.call(expand.dots = TRUE)$cex
        
        ex <- ex * cex
        
        ey <- ey * cex
    }
    
    for (i in seq_along(x)) {
        
        drawStick(x = x[i] - 0.5 * ex, y = y[i] - 0.5 * ey, 
            xscale = ex, yscale = ey, 
            gender = gender[i], 
            lwd = lwd[i], 
            linecol = linecol[i], 
            hatcol = clothes[i, 3], 
            shcol = clothes[i, 1], 
            clcol = clothes[i, 2], 
            arms = arms[i], 
            face = face[i], 
            legs = legs[i], 
            hat = hat[i])
    }
    
    return(invisible(NULL))
}
