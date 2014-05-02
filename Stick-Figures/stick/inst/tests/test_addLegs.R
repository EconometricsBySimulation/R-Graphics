# Date of last change: 2014-02-25
# Last changed by: ccampbell
# 
# Original author: ccampbell
###############################################################################

context("adding legs")

###############################################################################

test_that("addLegs", {
    
    dir.create("test-stick", showWarnings = FALSE)
    
    png(file = "test-stick/test-addLegs-01.png", height = 400, width = 400, res = 72)
    
    plot(0:1, 0:1, type = "n")
    
    a1 <- addLegs()
    expect_that(a1, equals(matrix(c(45, 50, 55, 5, 35, 5), nrow = 2, byrow = TRUE)))
    
    a2 <- addLegs(legs = "apart", linecol = 4)
    expect_that(a2, equals(matrix(c(35, 50, 65, 5, 35, 5), nrow = 2, byrow = TRUE)))
    
    dev.off()
    
    if (require(visualTest)) {
        
        finger1 <- c(27L, 28L, 27L, 5L, 3L, 15L, 31L, 26L, 12L, 3L, 10L, 26L, 29L, 
            19L, 3L, 3L, 27L, 28L, 25L)
        
        expect_that(isSimilar(file = "test-stick/test-addLegs-01.png", 
            fingerprint = finger1), is_true())
    }
})

