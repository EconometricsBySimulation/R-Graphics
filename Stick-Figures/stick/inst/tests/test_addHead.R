# Date of last change: 2014-02-25
# Last changed by: ccampbell
# 
# Original author: ccampbell
###############################################################################

context("adding head")

###############################################################################

test_that("addHead", {
    
    plot(0:1, 0:1, type = "n")
    
    a1 <- addHead(hat = "default")
    expect_that(a1, equals(matrix(c(46, 76, 2, 1.8, 
        54, 76, 2, 1.8, 
        50, 67, 4, 0, 
        0, 360, 0, 0), nrow = 4, byrow = TRUE)))
    
    a2 <- addHead(face = "happy", linecol = 4)
    expect_that(a2, equals(matrix(c(46, 77, 2.5, 2, 
        54, 77, 2.5, 2, 
        50, 72, 6, 8, 
        -160, -20, 0, 0), nrow = 4, byrow = TRUE)))
    
    a3 <- addHead(face = "sad", linecol = 5)
    expect_that(a3, equals(matrix(c(46, 75, 2, 2, 
        54, 75, 2, 2, 
        50, 60, 6, 8, 
        140, 40, 0, 0), nrow = 4, byrow = TRUE)))
    
    a4 <- addHead(face = "surprised", hat = "none")
    expect_that(a4, equals(matrix(c(46, 78, 3, 2, 
        54, 78, 3, 2, 
        50, 65, 3, 4, 
        0, 360, 0, 0), nrow = 4, byrow = TRUE)))
    
    a5 <- addHead(face = "annoyed", linecol = 3)
    expect_that(a5, equals(matrix(c(46, 76, 2, 2, 
        54, 76, 2, 1, 
        46, 66, 55, 68, 
        0, 0, Inf, 0), nrow = 4, byrow = TRUE)))
    
    m6 <- matrix(c(46, 77, 2.5, 2,
                        54, 77, 2.5, 2, 
                        50, 70, 6, 2, 
                      -160, -20, 0, 0), nrow = 4, ncol = 4, byrow = TRUE)
    a6 <- addHead(y = -0.5, face = m6)
    expect_that(a6, equals(m6))
    
    dev.off()
    }
)

