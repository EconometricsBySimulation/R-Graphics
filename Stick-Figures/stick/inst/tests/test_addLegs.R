# Date of last change: 2014-02-25
# Last changed by: ccampbell
# 
# Original author: ccampbell
###############################################################################

context("adding legs")

###############################################################################

test_that("addLegs", {
    
    plot(0:1, 0:1, type = "n")
    
    a1 <- addLegs()
    expect_that(a1, equals(matrix(c(45, 50, 55, 5, 35, 5), nrow = 2, byrow = TRUE)))
    
    a2 <- addLegs(legs = "apart", linecol = 4)
    expect_that(a2, equals(matrix(c(35, 50, 65, 5, 35, 5), nrow = 2, byrow = TRUE)))
    
    dev.off()
    }
)

