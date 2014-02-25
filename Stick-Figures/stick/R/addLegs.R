#' @title add legs
#' @param scale: size of figure
#' @param x: left bottom alignment of figure
#' @param y: left bottom alignment of figure
#' @param xs: x scale (default 1/100)
#' @param ys: y scale (default 1/100)
#' @param legs: single character (default \code{'default'})
#' @param gender: "male", "female"
#' @param lwd: line weight
#' @param linecol: color of lines
#' @param clcol: color of clothes, or NULL to supress clothes (default \code{NULL}) 
#' @param tail: x-y coordiates of leg join with torso (default \code{c(50, 35)})
#' @param w: width parameter (default 5)
#' @export
#' @author Francis Smart, Mango Solutions
#' @return list with locations of head, arms and legs
#' @examples
#'     plot(0:1, 0:1, type = "n")
#'     addLegs()

addLegs <- function(x = 0, y = 0, xs = 1 / 100, ys = 1 / 100, legs = "default", gender = c("male", "female"), 
    lwd = 1, linecol = 1, clcol = NULL, tail = c(50, 35), w = 5) {

    if (all(is.na(legs))) { legs <- "default" }
    
    if (is.character(legs) && length(legs) == 1) {
        
        legs <- switch(legs, 
            "apart" = {
                matrix(c(35, tail[1], 65,
                    5, tail[2], 5), nrow = 2, byrow = TRUE)
            },
            
                matrix(c(tail[1] - w, tail[1], tail[1] + w,
                    5, tail[2], 5), nrow = 2, byrow = TRUE)
            )
    }
    
    lines(x + legs[1, ] * xs, y + legs[2, ] * ys, lwd = lwd, col = linecol)
    
    # Draw dress
    if (!is.null(clcol) && gender != "male") {
        
        # Draw dress
        polygon(c(x + xs * 50, x + xs * 35, x + xs * 65),
            c(y + ys * 40, y + ys * 17, y + ys * 17),
            col = clcol, border = linecol, lwd = lwd)
    }
    
    return(invisible(legs))
}
