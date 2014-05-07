#' add a head
#'
#' @title add head
#' @param x: left bottom alignment of figure
#' @param y: left bottom alignment of figure
#' @param xs: x scale (default 1/100)
#' @param ys: y scale (default 1/100)
#' @param face: "default" ("neutral"), "happy", "sad", "annoyed", "surprised"
#'     face can also be a numeric matrix with 4 rows and four columns.
#'     The rows give coordinates of: \enumerate{
#'     \item left eye x, y, x diameter, y diameter
#'     \item right eye x, y, x diameter, y diameter
#'     \item mouth x, y, x diameter, y diameter. 
#'         If the third value of the fourth column is Inf, x, y, x, y of a straight line.
#'     \item mouth start and stop positions. If the third value is Inf, plot a line for the mouth (plus one unused values)
#' }
#' @param hat: single logical plot hat 
#'     or single character "none", "shapka", "beanie", "bill" or "fedora" (default "none")
#' @param lwd: line weight
#' @param linecol: color of lines - any color
#' @param hatcol: color of hat - any color, or NULL to supress hat (default 2)
#' @param head: length 4 specifying head x-y location and x-y diameters
#' @return matrix of head coordinates
#' @export
#' @examples
#'    plot(0:1, 0:1, type = "n")
#'    addHead(hat = "default")
#'    addHead(y = -0.5, face = matrix(c(46, 77, 2.5, 2,
#'                    54, 77, 2.5, 2, 
#'                    50, 70, 6, 2, 
#'                    -160, -20, 0, 0), nrow = 4, ncol = 4, byrow = TRUE))

addHead <- function(x = 0, y = 0, xs = 1 / 100, ys = 1 / 100, face = "default", hat = "none",
    lwd = 1, linecol = 1, hatcol = 2, head = c(50, 75, 10, 15)) {
    
    draw.ellipse(x + head[1] * xs, y + head[2] * ys, head[3] * xs, head[4] * ys, lwd = lwd, border = linecol)
    
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
                    46, 66, 55, 68, 
                    0,   0, Inf, 0), nrow = 4, ncol = 4, byrow = TRUE)
            },  
            matrix(c(46, 76, 2, 1.8,
                    54, 76, 2, 1.8, 
                    50, 67, 4, 0, 
                    0, 360, 0, 0), nrow = 4, ncol = 4, byrow = TRUE)
        )
    }
    
    # Draw eyes
    draw.ellipse(x = x + face[1, 1] * xs, y = y + face[1, 2] * ys, a = face[1, 3] * xs, b = face[1, 4] * ys, lwd = lwd, border = linecol)
    draw.ellipse(x = x + face[2, 1] * xs, y = y + face[2, 2] * ys, a = face[2, 3] * xs, b = face[2, 4] * ys, lwd = lwd, border = linecol)
    
    # Draw mouth
    if (is.finite(face[4, 3])) {
        
        draw.ellipse(x = x + face[3, 1] * xs, y = y + face[3, 2] * ys, a = face[3, 3] * xs, b = face[3, 4] * ys, 
            segment = face[4, 1:2], lwd = lwd, border = linecol)
        
    } else {
        
        lines(x = x + face[3, c(1, 3)] * xs, y + face[3, c(2, 4)] * ys, lwd = lwd, col = linecol)
    }
    # Draw hat
    
    if (!is.null(hatcol) && !is.na(hatcol)) {
        
        if (hat != "none") {
            
            hat <- switch(hat, 
                "shapka" = {
                    matrix(c(-9, -8, -6, -8, -6, 
                            rev(c(9, 8, 6, 8, 6)),
                                
                                7, 15, 16, 8, 17,
                            rev(c(7, 15, 16, 8, 17))), nrow = 2, byrow = TRUE)
                },
                "beanie" = "beanie",
                "bill" = "bill",
                matrix(c(-14, -14, -9, -8, 
                        rev(c(14, 14, 9, 8)),
                            
                            8, 10, 10, 16,
                        rev(c(8, 10, 10, 16))), nrow = 2, byrow = TRUE)
            )
            
            if (is.character(hat)) {
                
                draw.ellipse(x + head[1] * xs, y + head[2] * ys, head[3] * xs * 1.05, head[4] * ys * 1.05, 
                    segment = c(10, 170), col = hatcol, border = linecol, lwd = lwd)
                
                if (hat == "bill") { 
                    
                    draw.ellipse(x + head[1] * xs, y + head[2] * ys + head[4] * ys * 0.1, head[3] * xs * 1.05, head[4] * ys * 0.4, 
                        segment = c(10, 170), col = linecol, border = linecol, lwd = lwd) 
                }
                
            } else {
                
                polygon(
                    x + (hat[1, ] + head[1]) * xs,
                    y + (hat[2, ] + head[2]) * ys,
                    col = hatcol, border = linecol, lwd = lwd)
            }
        }
    }
    return(invisible(face))
}
