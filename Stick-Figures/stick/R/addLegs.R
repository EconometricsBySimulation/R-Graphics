
#' @title Add Legs
#' 
#' @description Add legs to a plotting area.
#' Trousers or a skirt/kilt will be added on top of the legs if clcol is specified.
#' Argument legs can be a character string, or a matrix specifying the 
#' coordinates of the legs.
#' 
#' @param x left bottom alignment of figure
#' @param y left bottom alignment of figure
#' @param xs x scale (default 1/100)
#' @param ys y scale (default 1/100)
#' @param legs single character (default \code{'default'})
#' @param gender "male", "female"
#' @param lwd line weight
#' @param linecol color of lines
#' @param clcol color of clothes, or NULL to supress clothes (default \code{NULL}) 
#' @param tail x-y coordiates of leg join with torso (default \code{c(50, 35)})
#' @param w width parameter (default 5)
#' @return matrix of legs coordinates
#' @export
#' @examples
#' plot(0:1, 0:1, type = "n")
#' addLegs()

addLegs <- function(x = 0, y = 0, xs = 1 / 100, ys = 1 / 100, legs = "default", gender = c("male", "female"), 
    lwd = 1, linecol = 1, clcol = NULL, tail = c(50, 35), w = 5) {
    
    gender <- gender[1]
    
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
    
    ccol <- ceiling(ncol(legs) / 2)
    
    # Draw dress
    if (!is.null(clcol) && !is.na(clcol)) {
        
        if (gender != "male") {
            
            dress <- matrix(c(legs[1, ccol - 1] - w / 2, legs[1, ccol], legs[1, ccol + 1] + w / 2, 
                        
                              legs[2, ccol - 1] + w * 3 / 2, tail[2] + w, legs[2, ccol + 1] + w * 3 / 2), 
                nrow = 2, byrow = TRUE)
            
            # Draw dress
            polygon(x = x + xs * dress[1, ], y = y + ys * dress[2, ],
                col = clcol, border = linecol, lwd = lwd)
                
        } else {
            
            trousers <- matrix(c(legs[1, ccol], legs[1, ccol - 1] + w / 2, legs[1, ccol - 1] - w / 2, legs[1, ccol] - w,  
                        rev(c(legs[1, ccol], legs[1, ccol + 1] - w / 2, legs[1, ccol + 1] + w / 2, legs[1, ccol] + w)),
                        
                              legs[2, ccol] - w * 3 / 2, legs[2, ccol - 1], legs[2, ccol - 1], legs[2, ccol], 
                        rev(c(legs[2, ccol] - w * 3 / 2, legs[2, ccol - 1], legs[2, ccol - 1], legs[2, ccol]))), 
                nrow = 2, byrow = TRUE)
            
            # Draw Trousers
            polygon(x = x + xs * trousers[1, ], y = y + ys * trousers[2, ],
                    col = clcol, border = linecol, lwd = lwd)
            
        }
    }
    
    return(invisible(legs))
}
