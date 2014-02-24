#' @title add legs
#' @param scale: size of figure
#' @param x: left bottom alignment of figure
#' @param y: left bottom alignment of figure
#' @param s: scale (default 1/100)
#' @param gender: "male", "female"
#' @param lwd: line weight
#' @param linecol: color of lines
#' @param clcol: color of clothes, or NULL to supress clothes (default \code{NULL}) 
#' @param legs: single character (default \code{'default'})
#' @param tail: x-y coordiates of leg join with torso (default \code{c(50, 35)})
#' @param w: width parameter (default 5)
#' @export
#' @author Francis Smart, Mango Solutions
#' @return list with locations of head, arms and legs
#' @examples
#'     plot(0:1, 0:1, type = "n")
#'     addLegs()

addLegs <- function(x = 0, y = 0, s = 1 / 100, gender = c("male", "female"), 
    lwd = 1, linecol = 1, clcol = NULL, legs = "default", tail = c(50, 35), w = 5) {

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
    
    lines(x + legs[1, ] * s, y + legs[2, ] * s, lwd = lwd, col = linecol)
    
    # Draw dress
    if (!is.null(clcol) && gender != "male") {
        
        # Draw dress
        polygon(c(x+s*50,x+s*35,x+s*65),
            c(y+s*40,y+s*17,y+s*17),
            col=clcol, border=linecol,lwd=lwd)
    }
    
    return(invisible(legs))
}
