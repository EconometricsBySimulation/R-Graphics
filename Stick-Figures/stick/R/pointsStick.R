
#' @title Add Stick Men and Women at coordinates
#' 
#' @description Add Stick Men or Women to a plot. 
#' This function is a convenience wrapper to \code{\link{plotStick}} 
#' with newplot FALSE.
#' 
#' @inheritParams plotStick
#' @return NULL
#' @export
#' @author Francis Smart [cre, cph, aut], Chris Campbell (Mango Solutions) [aut]
#' @examples
#' plot(x = 1:10, y = c(1:5, 5:1), cex = 8)
#' pointsStick(1:10, c(1:5, 5:1), col = 2)
#' plot(x = 1:10, y = sin(1:10), type = "n")
#' pointsStick(x = 1:10, y = sin(1:10), hatcol = 2, shcol = rainbow(10), 
#'     clcol = 1:10, gender = c("male", "female"), 
#'     arms = c("down", "neutral", "up", "hip", "wave"))

pointsStick <- function(x, y, 
    lwd = 1, linecol = 1, hatcol = NULL, shcol = NULL, clcol = NULL, 
    col = NULL, gender = c("male", "female"), 
    arms = "default", face = "default", legs = "default", hat = "default", tower = 10) {
    
    if (missing(x)) { stop("x is missing") }
    
    if (missing(y)) { stop("y is missing") }
    
    plotStick(x = x, y = y, 
        lwd = lwd, linecol = linecol, hatcol = hatcol, shcol = shcol, clcol = clcol, 
        col = col, gender = gender, 
        arms = arms, face = face, legs = legs, hat = hat, tower = tower, newplot = FALSE)
    
}


