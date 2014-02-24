#' add a head
#'
#' @title add head
#' @param x: left bottom alignment of figure
#' @param y: left bottom alignment of figure
#' @param s: scale (default 1/100)
#' @param face: "default" ("neutral"), "happy", "sad", "annoyed", "surprised"
#' @param hat: single logical plot hat 
#'     or single character "none" or "default" (default "none")
#' @param lwd: line weight
#' @param linecol: color of lines - any color
#' @param hatcol: color of hat - any color
#' @param head: length 4 specifying head x-y location and x-y diameters
#' @return matrix of head coordinates
#' @export
#' @examples
#'    plot(0:1, 0:1, type = "n")
#'        addHead(hat = "default")
#'        addHead(y = -0.5, face = matrix(c(46, 77, 2.5, 2,
#'                        54, 77, 2.5, 2, 
#'                        50, 70, 6, 2, 
#'                        -160, -20, 0, 0), nrow = 4, ncol = 4, byrow = TRUE))

addHead <- function(x = 0, y = 0, s = 1 / 100, face = "default", hat = "none",
    lwd = 1, linecol = 1, hatcol = 2, head = c(50, 75, 10, 15)) {
    
    draw.ellipse(x + head[1] * s, y + head[2] * s, head[3] * s, head[4] * s, lwd = lwd, border = linecol)
    
    if (all(is.na(face))) { face <- "default" }
    
    if (is.character(face) && length(face) == 1) {
        
        face <- switch(face, 
            "happy" = { 
                matrix(c(46, 77, 2.5, 2,
                    54, 77, 2.5, 2, 
                    50, 72, 6, 8, 
                    -160, -20, 0, 0), nrow = 4, ncol = 4, byrow = TRUE)
            }, 
            "sad" = {
                matrix(c(46, 75, 2, 2,
                    54, 75, 2, 2, 
                    50, 60, 6, 8, 
                    140, 40, 0, 0), nrow = 4, ncol = 4, byrow = TRUE)
            }, 
            "surprised" = {
                matrix(c(46, 78, 3, 2,
                    54, 78, 3, 2, 
                    50, 65, 3, 4, 
                    0, 360, 0, 0), nrow = 4, ncol = 4, byrow = TRUE)
            },     
            "annoyed" = {
                matrix(c(46, 76, 2, 2,
                    54, 76, 2, 1, 
                    50, 67, 3, 4, 
                    0, 360, 0, 0), nrow = 4, ncol = 4, byrow = TRUE)
            },  
            matrix(c(46, 76, 2, 1.8,
                    54, 76, 2, 1.8, 
                    50, 67, 4, 0, 
                    0, 360, 0, 0), nrow = 4, ncol = 4, byrow = TRUE)
        )
    }
    
    # Draw eyes
    draw.ellipse(x = x + face[1, 1] * s, y = y + face[1, 2] * s, a = face[1, 3] * s, b = face[1, 4] * s, lwd = lwd, border = linecol)
    draw.ellipse(x = x + face[2, 1] * s, y = y + face[2, 2] * s, a = face[2, 3] * s, b = face[2, 4] * s, lwd = lwd, border = linecol)
    
    # Draw mouth
    draw.ellipse(x = x + face[3, 1] * s, y = y + face[3, 2] * s, a = face[3, 3] * s, b = face[3, 4] * s, 
        segment = face[4, 1:2], lwd = lwd, border = linecol)
    
    # Draw hat
    if (hat != "none") {
        
        hat <- matrix(c(-14, -14, -9, -8, 
                    rev(c(14, 14, 9, 8)),
                        
                        8, 10, 10, 16,
                    rev(c(8, 10, 10, 16))), nrow = 2, byrow = TRUE)
        
        polygon(
            x + (hat[1, ] + head[1]) * s,
            y + (hat[2, ] + head[2]) * s,
            col = hatcol, border = linecol, lwd = lwd)
    }
    
    return(invisible(face))
}
