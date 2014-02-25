# Date of last change: 2014-02-25
# Last changed by: ccampbell
# 
# Original author: ccampbell
###############################################################################

context("draw stick")

###############################################################################

test_that("drawStick", {
    
    plot(0:1, 0:1, type = "n")
    
    a1 <- drawStick()
    expect_that(names(a1), equals(c("head", "arms", "legs")))
    
    a2 <- drawStick(0.25 ,0, gender = "female", face = "annoyed", 
        arms = "up", legs = "apart", clcol = 3)
    
    l2 <- list(head = matrix(c(46, 76, 2, 2, 
            54, 76, 2, 1, 
            46, 66, 55, 68, 
            0, 0, Inf, 0), nrow = 4, byrow = TRUE), 
        arms = matrix(c(30, 50, 70, 
            65, 55, 65), nrow = 2, byrow = TRUE), 
        legs = matrix(c(35, 50, 65, 
            5, 35, 5), nrow = 2, byrow = TRUE))
    expect_that(a2, equals(l2))
    
    dev.off()
    }
)

