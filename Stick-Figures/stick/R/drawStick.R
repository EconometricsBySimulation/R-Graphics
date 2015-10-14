#' Make plots with stick people using \code{\link{plotStick}}. 
#' Add stick people to an existing core graphics plot using \code{\link{pointsStick}}.
#'  
#' Run tests using \pkg{testthat} call \code{test_package("stick")}.
#' Comparison images can be checked using 
#' \pkg{\href{https://github.com/MangoTheCat/visualTest}{visualTest}}.
#' 
#' \tabular{ll}{
#' Package: \tab stick \cr
#' Type: \tab Package \cr
#' Version: \tab 1.0.12 \cr
#' Date: \tab Oct 2015 \cr
#' Lazyload: \tab yes \cr
#' }
#' @name stick-package
#' @aliases stick
#' @docType package
#' @title Craft Stick Men and Women
#' @author Francis Smart [cre, cph, aut], Chris Campbell (Mango Solutions) [aut]
#' @references \href{http://www.econometricsbysimulation.com/2014/01/stick-figure-function-r.html}{www.econometricsbysimulation.com (stick figures)}
#' @keywords package
NULL


#' @title Stick Man/Woman
#' 
#' @description Create a stick man or woman.
#' The stick person is constructed by \code{\link{addArms}}, \code{\link{addLegs}}, 
#' and \code{\link{addHead}}.
#' 
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
#' @import plotrix
#' @export
#' @author Francis Smart [cre, cph, aut], Chris Campbell (Mango Solutions) [aut]
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

