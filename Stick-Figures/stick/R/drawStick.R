#' Make Stick People
#' 
#' \tabular{ll}{
#' Package: \tab stick \cr
#' Type: \tab Package \cr
#' Version: \tab 1.0.9 \cr
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

#' Create a Stick Man or Woman
#' See http://www.econometricsbysimulation.com/2014/01/stick-figure-function-r.html for more information
#' Use \code{test_dir(path = file.path(system.file(package = "stick"), "tests"))} 
#'     (from \code{testthat}) to run tests.
#' @title Stick Man/Woman
#' @param scale: size of figure
#' @param x: left bottom alignment of figure
#' @param y: left bottom alignment of figure
#' @param lwd: line weight
#' @param linecol: color of lines
#' @param hatcol: color of hat
#' @param shcol: color of shirt, or NULL to supress shirt (default \code{NULL})
#' @param clcol: color of clothes, or NULL to supress clothes (default \code{NULL}) 
#' @param arms: single character "down", "neutral", "up", "hip", "wave"
#' @param gender: "male", "female"
#' @param face: single character "default" ("neutral"), "happy", "sad", "annoyed", "surprised"
#'     face can also be a numeric matrix with 4 rows and four columns.
#'     The rows give coordinates of: \enumerate{
#'     \item left eye x, y, x diameter, y diameter
#'     \item right eye x, y, x diameter, y diameter
#'     \item mouth x, y, x diameter, y diameter. 
#'         If the third value of the fourth column is Inf, x, y, x, y of a straight line.
#'     \item mouth start and stop positions. If the third value is Inf, plot a line for the mouth (plus one unused values)
#' }
#' @param legs: single character "default" or "apart"
#' @param hat: single logical plot hat or automatic if NA (default NA)
#' @return list with locations of head, arms and legs
#' @import plotrix testthat
#' @export
#' @author Francis Smart, Mango Solutions
#' @examples
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
#'
#'    # Solitary annoyed figure
#'    plot(c(.25,.75), c(0,1), type = "n")
#'    drawStick(0, 0, face = "surprised", gender = "male",
#'        arms = "wave", hat = FALSE)

drawStick <- function(x = 0, y = 0, scale = 1, gender = c("male", "female"), 
    lwd = 3, linecol = 1, hatcol = 2, shcol = NULL, clcol = NULL, 
    arms = "default", face = "default", legs = "default", hat = "default", shirt = FALSE, 
    ...) {
    
    gender <- gender[1]
    
    # Set the figure scale, default is 1
    s <- scale / 100

    # If is undefined then give the man a hat
    if (is.na(hat)) { hat <- "default" }
    if (is.logical(hat)) { ifelse(hat, "default", "none") }
    if (gender != "male" & hat == "default") { hat <- "none" }
    
    head <- c(50, 75, 10, 15)
    torso <- c(head[1], head[2] - head[4], head[1], 35)
    
    # Draw Head

    hed <- addHead(x = x, y = y, s = s, face = face, hat = hat, 
        lwd = lwd, linecol = linecol, hatcol = hatcol, head = head, ...)
    
    # Draw arms
    
    arm <- addArms(x = x, y = y, s = s, arms = arms, 
        lwd = lwd, linecol = linecol, shcol = shcol, torso = torso, ...)
    
    # Draw legs
    
    leg <- addLegs(x = x, y = y, s = s, legs = legs, gender = gender, 
        lwd = lwd, linecol = linecol, clcol = clcol, ...)
    
    return(invisible(list(head = hed, arms = arm, legs = leg)))  
}
