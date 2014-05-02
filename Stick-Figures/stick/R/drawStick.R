#' Make Stick People. 
#' Run tests using \pkg{testthat} call \code{test_package("stick")}.
#' 
#' \tabular{ll}{
#' Package: \tab stick \cr
#' Type: \tab Package \cr
#' Version: \tab 1.0.11 \cr
#' Date: \tab May 2014 \cr
#' Lazyload: \tab yes \cr
#' }
#' @name stick-package
#' @aliases stick
#' @docType package
#' @title Craft Stick Men and Women
#' @author Francis Smart, Mango Solutions
#' @references \url{http://www.econometricsbysimulation.com/2014/01/stick-figure-function-r.html}
#' @keywords package
NULL

#' Create a Stick Man or Woman.
#' 
#' @title Stick Man/Woman
#' @param x left bottom alignment of figure
#' @param y left bottom alignment of figure
#' @param xscale size of figure
#' @param yscale size of figure
#' @param lwd line weight
#' @param linecol color of lines
#' @param hatcol color of hat, or NULL to supress shirt (default \code{NULL})
#' @param shcol color of shirt, or NULL to supress shirt (default \code{NULL})
#' @param clcol color of clothes, or NULL to supress clothes (default \code{NULL}) 
#' @param arms single character "down", "neutral", "up", "hip", "wave"
#' @param gender "male", "female"
#' @param face single character "default" ("neutral"), "happy", "sad", "annoyed", "surprised"
#'     face can also be a numeric matrix with 4 rows and four columns.
#'     The rows give coordinates of: \enumerate{
#'     \item left eye x, y, x diameter, y diameter
#'     \item right eye x, y, x diameter, y diameter
#'     \item mouth x, y, x diameter, y diameter. 
#'         If the third value of the fourth column is Inf, x, y, x, y of a straight line.
#'     \item mouth start and stop positions. If the third value is Inf, 
#'         plot a line for the mouth (plus one unused values)
#' }
#' @param legs single character "default" or "apart"
#' @param hat: single logical plot hat 
#'     or character "none", "shapka", "beanie" or "default" (default "none")
#' @return list with locations of head, arms and legs
#' @import plotrix testthat
#' @export
#' @author Francis Smart, Mango Solutions
#' @examples
#'
#'    # Solitary annoyed figure
#'    drawStick(face = "surprised", arms = "wave")        
#'    
#'    # Map of stick figures
#'    par(mar = rep(0, 4))
#'
#'    plot(c(.25,1.25), c(0,3), type = "n", xaxt = 'n', yaxt = 'n', ann = FALSE)
#'
#'    drawStick(0, 0, arms = "hip")
#'    drawStick(0.5, 0, gender = "female", arms = "up")
#'
#'    drawStick(0, 1, arms = "neutral", lwd = 2, linecol = gray(.5),
#'             clcol = "red", face = "sad")
#'
#'    drawStick(0.5, 1, gender = "female", arms = "down", clcol = "purple",
#'             lwd = 2, linecol = gray(.5), face = "sad",hat = TRUE)
#'
#'    drawStick(0, 2, arms = "wave", linecol = gray(.7),
#'             clcol = "blue", face = "surprised",hat = FALSE)
#'
#'    drawStick(0.5, 2, gender = "female", arms = "hip", clcol = "light blue",
#'             linecol = gray(.7),face = "annoyed")

drawStick <- function(x = 0, y = 0, xscale = 1, yscale = 1, gender = c("male", "female"), 
    lwd = 3, linecol = 1, hatcol = 2, shcol = NULL, clcol = NULL, 
    arms = "default", face = "default", legs = "default", hat = "default", ...) {
    
    gender <- gender[1]
    
    # Set the figure scale, default is 1
    xs <- xscale / 100
    ys <- yscale / 100
    
    # If is undefined then give the man a hat
    if (is.na(hat)) { hat <- "default" }
    if (is.logical(hat)) { ifelse(hat, "default", "none") }
    if (gender != "male" & hat == "default") { hat <- "none" }
    
    head <- c(50, 75, 10, 15)
    torso <- c(head[1], head[2] - head[4], head[1], 35)
    
    if (dev.cur() == 1) { plot(0:1, 0:1, xlab = "", ylab = "", type = "n", axes = FALSE) }
    
    # Draw Head
    
    hed <- addHead(x = x, y = y, xs = xs, ys = ys, face = face, hat = hat, 
        lwd = lwd, linecol = linecol, hatcol = hatcol, head = head, ...)
    
    # Draw legs
    
    leg <- addLegs(x = x, y = y, xs = xs, ys = ys, legs = legs, gender = gender, 
        lwd = lwd, linecol = linecol, clcol = clcol, ...)
        
    # Draw arms
    
    arm <- addArms(x = x, y = y, xs = xs, ys = ys, arms = arms, 
        lwd = lwd, linecol = linecol, shcol = shcol, torso = torso, ...)

    return(invisible(list(head = hed, arms = arm, legs = leg)))
}


#' A Stick Man or Woman plot.
#' 
#' @title Plot Stick Men and Women at coordiates
#' @param x 
#' @param y 
#' @param xlim x-axis limits to use, otherwise range of x expanded
#' @param ylim
#' @param xlab
#' @param ylab
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
#' @param tower single numeric approximate number of stick people that could be stacked in the plotting area
#' @return NULL
#' @export
#' @author Francis Smart, Mango Solutions
#' @examples
#'     plotStick(x = 1:10, y = c(1:5, 5:1))
#'     points(1:10, c(1:5, 5:1), col = 2)
#'     points(1:10, c(1:5, 5:1), col = 2, pch = 3)
#'     plotStick(x = 1:10, y = sin(1:10), hatcol = 2, shcol = rainbow(10), 
#'         clcol = 1:10, gender = c("male", "female"), 
#'         arms = c("down", "neutral", "up", "hip", "wave"))
#'     plotStick(x = 1:10, y = cos(1:10), 
#'         hat = c("none", "shapka", "beanie", "fedora"), col = rainbow(30), cex = 2)

plotStick <- function(x, y, xlim, ylim, xlab = "x", ylab = "y", 
    lwd = 1, linecol = 1, hatcol = NULL, shcol = NULL, clcol = NULL, 
    col = NULL, gender = c("male", "female"), 
    arms = "default", face = "default", legs = "default", hat = "default", tower = 10, ...) {
    
    if (missing(x)) { stop("x is missing") }
    
    if (missing(y)) { stop("y is missing") }
    
    lngtx <- length(x)
    
    if (lngtx != length(y)) { stop("x and y must be the same length") }
    
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
    
    if (length(lwd) != lngtx) { lwd <- rep(lwd, times = ceiling(lngtx / length(lwd))) }
    
    if (length(linecol) != lngtx) { linecol <- rep(linecol, times = ceiling(lngtx / length(linecol))) }
    
    clothes <- matrix(NA, nrow = lngtx, ncol = 3)
    
    if (!is.null(col)) {
        
        if (is(col, "matrix") && ncol(col) != 3) { col <- c(col) }
        
        if (!is(col, "matrix")) {
            
            col <- matrix(rep(c(col), each = lngtx * 3)[seq.int(1, lngtx * 3)], 
                nrow = lngtx, ncol = 3, byrow = TRUE)
        }
        
        clothes <- col 
        
        
    } else {
        
        if (!is.null(shcol)) { clothes[, 1] <- shcol }
        
        if (!is.null(clcol)) { clothes[, 2] <- clcol }
        
        if (!is.null(hatcol)) { clothes[, 3] <- hatcol }
    }
    
    if (length(gender) != lngtx) { gender <- rep(gender, times = ceiling(lngtx / length(gender))) }
    
    if (length(arms) != lngtx) { arms <- rep(arms, times = ceiling(lngtx / length(arms))) }
    
    if (length(face) != lngtx) { face <- rep(face, times = ceiling(lngtx / length(face))) }
    
    if (length(legs) != lngtx) { legs <- rep(legs, times = ceiling(lngtx / length(legs))) }
    
    if (length(hat) != lngtx) { hat <- rep(hat, times = ceiling(lngtx / length(hat))) }
    
    plot(x, y, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, type = "n", ...)
    
    if (hasArg("cex")) {
        
        cex <- match.call(expand.dots = TRUE)$cex
        
        ex <- ex * cex
        
        ey <- ey * cex
    }
    
    for (i in seq_along(x)) {
        
        drawStick(x = x[i] - 0.5 * ex, y = y[i] - 0.5 * ey, xscale = ex, yscale = ey, gender = gender[i], 
            lwd = lwd[i], linecol = linecol[i], hatcol = clothes[i, 3], shcol = clothes[i, 1], clcol = clothes[i, 2], 
            arms = arms[i], face = face[i], legs = legs[i], hat = hat[i])
    }
    
    return(invisible(NULL))
}
