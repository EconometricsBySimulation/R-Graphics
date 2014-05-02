# Date of last change: 2014-05-01
# Last changed by: ccampbell
# 
# Original author: ccampbell
###############################################################################

context("adding head")

###############################################################################

test_that("addHead", {
    
    dir.create("test-stick", showWarnings = FALSE)
    
    png(file = "test-stick/test-addHead-01.png", height = 400, width = 400, res = 72)
    
    plot(0:1, 0:1, type = "n")
    
    a1 <- addHead(hat = "default")
    
    dev.off()
    
    expect_that(a1, equals(matrix(c(46, 76, 2, 1.8, 
        54, 76, 2, 1.8, 
        50, 67, 4, 0, 
        0, 360, 0, 0), nrow = 4, byrow = TRUE)))
    
    png(file = "test-stick/test-addHead-02.png", height = 400, width = 400, res = 72)
    
    plot(0:1, 0:1, type = "n")
    
    a2 <- addHead(face = "happy", linecol = 4)
    
    dev.off()
    
    expect_that(a2, equals(matrix(c(46, 77, 2.5, 2, 
        54, 77, 2.5, 2, 
        50, 72, 6, 8, 
        -160, -20, 0, 0), nrow = 4, byrow = TRUE)))
    
    png(file = "test-stick/test-addHead-03.png", height = 400, width = 400, res = 72)
    
    plot(0:1, 0:1, type = "n")
    
    a3 <- addHead(face = "sad", linecol = 5)
    
    dev.off()
    
    expect_that(a3, equals(matrix(c(46, 75, 2, 2, 
        54, 75, 2, 2, 
        50, 60, 6, 8, 
        140, 40, 0, 0), nrow = 4, byrow = TRUE)))
    
    png(file = "test-stick/test-addHead-04.png", height = 400, width = 400, res = 72)
    
    plot(0:1, 0:1, type = "n")
    
    a4 <- addHead(face = "surprised", hat = "none")
    
    dev.off()
    
    expect_that(a4, equals(matrix(c(46, 78, 3, 2, 
        54, 78, 3, 2, 
        50, 65, 3, 4, 
        0, 360, 0, 0), nrow = 4, byrow = TRUE)))
    
    png(file = "test-stick/test-addHead-05.png", height = 400, width = 400, res = 72)
    
    plot(0:1, 0:1, type = "n")
    
    a5 <- addHead(face = "annoyed", linecol = 3)
    
    dev.off()
    
    expect_that(a5, equals(matrix(c(46, 76, 2, 2, 
        54, 76, 2, 1, 
        46, 66, 55, 68, 
        0, 0, Inf, 0), nrow = 4, byrow = TRUE)))
    
    m6 <- matrix(c(46, 77, 2.5, 2,
                        54, 77, 2.5, 2, 
                        50, 70, 6, 2, 
                      -160, -20, 0, 0), nrow = 4, ncol = 4, byrow = TRUE)

    png(file = "test-stick/test-addHead-06.png", height = 400, width = 400, res = 72)
    
    plot(0:1, 0:1, type = "n")

    a6 <- addHead(y = -0.5, face = m6)
    
    dev.off()
    
    expect_that(a6, equals(m6))
    
    if (require(visualTest)) {
    
        finger1 <- c(27L, 4L, 20L, 3L, 26L, 7L, 3L, 15L, 12L, 3L, 13L, 8L, 3L, 6L, 
            3L, 6L, 3L, 9L, 3L, 3L, 3L, 6L, 6L, 6L, 3L, 6L, 3L, 6L, 15L, 
            3L, 6L, 21L, 3L, 3L, 27L, 6L, 19L, 5L, 23L, 24L)
        
        finger2 <- c(27L, 28L, 27L, 5L, 3L, 15L, 31L, 26L, 12L, 3L, 10L, 26L, 29L, 
            19L, 3L, 3L, 27L, 28L, 25L, 25L)
        
        finger3 <- c(27L, 28L, 27L, 5L, 3L, 15L, 31L, 26L, 12L, 3L, 10L, 26L, 29L, 
            19L, 3L, 3L, 27L, 28L, 25L, 25L)
        
        finger4 <- c(27L, 27L, 28L, 5L, 3L, 17L, 27L, 22L, 3L, 3L, 12L, 3L, 10L, 
            5L, 3L, 18L, 29L, 19L, 3L, 3L, 28L, 27L, 25L, 25L)
        
        finger5 <- c(27L, 28L, 27L, 5L, 3L, 17L, 29L, 26L, 12L, 3L, 10L, 26L, 29L, 
            19L, 3L, 3L, 27L, 28L, 25L, 25L)
        
        finger6 <- c(18L, 27L, 27L, 27L, 6L, 15L, 3L, 29L, 16L, 3L, 3L, 3L, 15L, 
            3L, 11L, 7L, 3L, 3L, 12L, 29L, 7L, 15L, 3L, 28L, 25L, 27L, 22L, 3L)
        
        expect_that(isSimilar(file = "test-stick/test-addHead-01.png", 
            fingerprint = finger1), is_true())
        
        expect_that(isSimilar(file = "test-stick/test-addHead-02.png", 
            fingerprint = finger2), is_true())
            
        expect_that(isSimilar(file = "test-stick/test-addHead-03.png", 
            fingerprint = finger3), is_true())
        
        expect_that(isSimilar(file = "test-stick/test-addHead-04.png", 
            fingerprint = finger4), is_true())
        
        expect_that(isSimilar(file = "test-stick/test-addHead-05.png", 
            fingerprint = finger5), is_true())
            
        expect_that(isSimilar(file = "test-stick/test-addHead-06.png", 
            fingerprint = finger6), is_true())
    }
})

