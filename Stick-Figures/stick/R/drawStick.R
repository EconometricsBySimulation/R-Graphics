#' Make Stick People
#' 
#' \tabular{ll}{
#' Package: \tab stick \cr
#' Type: \tab Package \cr
#' Version: \tab 1.0.10 \cr
#' Date: \tab February 2014 \cr
#' Lazyload: \tab yes \cr
#' }
#' @name stick-package
#' @aliases stick
#' @docType package
#' @title Craft Stick Men and Women
#' @author Francis Smart, Mango Solutions
#' @keywords package
NULL

#' Create a Stick Man or Woman.
#' 
#' See \url{http://www.econometricsbysimulation.com/2014/01/stick-figure-function-r.html} for more information.
#' Use \code{test_dir(path = file.path(system.file(package = "stick"), "tests"))} 
#'     (from \code{testthat}) to run tests.
#' @title Stick Man/Woman
#' @param scale size of figure
#' @param x left bottom alignment of figure
#' @param y left bottom alignment of figure
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
#'     \item mouth start and stop positions. If the third value is Inf, plot a line for the mouth (plus one unused values)
#' }
#' @param legs single character "default" or "apart"
#' @param hat single logical plot hat or automatic if NA (default NA)
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

drawStick <- function(x = 0, y = 0, xscale = 1, yscale = xscale, gender = c("male", "female"), 
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
#' @param arms character vector "down", "neutral", "up", "hip", "wave"
#' @param gender character vector "male", "female"
#' @param face list or character vector "default" ("neutral"), "happy", "sad", "annoyed", "surprised"
#'     face can also be a numeric matrix with 4 rows and four columns. 
#'     See \code{\link{drawStick}} for more details.
#' @param legs single character "default" or "apart"
#' @param hat single logical plot hat or automatic if NA (default NA)
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

plotStick <- function(x, y, xlim, ylim, xlab = "x", ylab = "y", 
    lwd = 1, linecol = 1, hatcol = NULL, shcol = NULL, clcol = NULL, gender = c("male", "female"), 
    arms = "default", face = "default", legs = "default", hat = "default", tower = 10, ...) {
    
    if (missing(x)) { stop("x is missing") }
    if (missing(y)) { stop("y is missing") }
    if (length(x) != length(y)) { stop("x and y must be the same length") }
    
    mx <- missing(xlim)
    
    if (mx) { xlim <- range(x[is.finite(x)]) }
    
    ex <- diff(xlim) / tower
    
    if (mx) { xlim <- xlim + c(-ex, ex) }
    
    my <- missing(ylim)
    
    if (my) { ylim <- range(y[is.finite(y)]) }
    
    ey <- diff(ylim) / tower 
    
    if (my) { ylim <- ylim + c(-ey, ey) }
    
    if (length(lwd) != length(x)) { lwd <- rep(lwd, times = ceiling(length(x) / length(lwd))) }
    
    if (length(linecol) != length(x)) { linecol <- rep(linecol, times = ceiling(length(x) / length(linecol))) }
    
    if (length(hatcol) != length(x)) { hatcol <- rep(hatcol, times = ceiling(length(x) / length(hatcol))) }
    
    if (length(shcol) != length(x)) { shcol <- rep(shcol, times = ceiling(length(x) / length(shcol))) }
    
    if (length(clcol) != length(x)) { clcol <- rep(clcol, times = ceiling(length(x) / length(clcol))) }
    
    if (length(gender) != length(x)) { gender <- rep(gender, times = ceiling(length(x) / length(gender))) }
    
    if (length(arms) != length(x)) { arms <- rep(arms, times = ceiling(length(x) / length(arms))) }
    
    if (length(face) != length(x)) { face <- rep(face, times = ceiling(length(x) / length(face))) }
    
    if (length(legs) != length(x)) { legs <- rep(legs, times = ceiling(length(x) / length(legs))) }
    
    if (length(hat) != length(x)) { hat <- rep(hat, times = ceiling(length(x) / length(hat))) }
    
    plot(x, y, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, type = "n")
    
    for (i in seq_along(x)) {
        
        drawStick(x = x[i] - 0.5 * ex, y = y[i] - 0.5 * ey, xscale = ex, yscale = ey, gender = gender[i], 
            lwd = lwd[i], linecol = linecol[i], hatcol = hatcol[i], shcol = shcol[i], clcol = clcol[i], 
            arms = arms[i], face = face[i], legs = legs[i], hat = hat[i])
    }
    
    return(invisible(NULL))
}
