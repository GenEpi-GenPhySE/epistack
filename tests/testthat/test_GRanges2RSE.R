data("stackepi")
data("stackepi_gr")

test_that("Convert as expected", {
    expect_equal(
        stackepi,
        GRanges2RSE(stackepi_gr, patterns = c("^window_"), names = c("DNAme"))
    )
})
